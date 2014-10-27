

let document = Dom_html.document
let window = Js.Unsafe.variable "window"

let root =  Utils.get_or_create "html" 
let body = Utils.get_or_create "body"


let parent_or_root element = 
  let node = (element :> Dom.node Js.t) in
  Js.Opt.get (node##parentNode) (fun () -> (root :> Dom.node Js.t)  )  

let create_slide () =
  let slide = document##createElement(Js.string "article")  in
  slide##classList##add(Js.string "slide"); 
  slide

let create_slide_interior () = 
  let div = Dom_html.createDiv document in
  div##classList##add(Js.string "slide_interior");
  div


let encapsulate new_father element =
  let new_father, element= ( new_father :> Dom.node Js.t), (element :> Dom.node Js.t) in  
  let grand_father = parent_or_root element in
  grand_father##replaceChild(new_father,element) |> ignore;
  new_father##appendChild(element)

let translate_slide raw_slide =
  let slide = create_slide () in
  let slide_interior = create_slide_interior () in
  let slide = Utils.transfer_attrs raw_slide slide in
  let slide_interior = slide_interior |> Utils.transfer_childs raw_slide
  and parent = parent_or_root raw_slide in
  slide##classList##add(Js.string "slide");
  slide##appendChild((slide_interior:> Dom.node Js.t) ) |> ignore;  
  Dom.replaceChild parent slide raw_slide; 
  slide

let if_attr element attr f =
  Js.Opt.iter element##getAttribute(attr) f

let add_title frame= 
  let add t = 
    let h = Dom_html.createH1 document
    and text = document##createTextNode(t) in
    Dom.appendChild h text;
    Utils.insertFirst frame h in
  if_attr frame (Js.string "title" ) add 

let add_class c element  = element##classList##add(Js.string c) ; element   


let unfocused  = 
  let unfocused = Dom_html.createDiv document |> add_class "unfocused" in
  let () = Dom.appendChild root unfocused in
  unfocused

  let unfocus (element : #Dom.node Js.t)  = unfocused##appendChild((element :> Dom.node Js.t) )


let prepare_slides slides = 
  Utils.node_consume slides ( fun n -> 
			       n 
			       |> translate_slide
			       |> unfocus
			       |> ignore 
			      ) ;
  document##querySelectorAll(Js.string ".slide")

let slides = document##getElementsByTagName(Js.string "slide")
let n_slides = slides##length
let current_frame = ref 0
let event_list = ref ( [], 0,  [] )

module OrdAnim=struct
  type t=string
  let compare:string->string->int = compare
end

		 
		     
module AnimData = Map.Make (OrdAnim)
let ( +> ) database animation= let open Timeline in
			       AnimData.add animation.name animation database
			   
let animator = AnimData.empty +> Atmospheric.animation 
let null_animation =
  let nothing status element = () in
  Timeline.{name = "nothing"; run=nothing; step =nothing; suspend = nothing }

	     
	     
let animator s =  try AnimData.find s animator with
		  |Not_found -> null_animation
		     
let advance_events () = 
  let past, present, futur = !event_list in
  let apply = Timeline.apply_event animator in
  let past,futur = Timeline.flow_event_to past (present+1) futur (<=) apply in
  event_list := past, present+1, futur
				   
let reverse_events () = 
  let past, present, futur = !event_list in
  let apply = Timeline.reverse_event animator in
  let futur,past = Timeline.flow_event_to futur (present-1) past (>) apply in
  event_list := past, present-1, futur

				   
let reset_events () = event_list := ([],-1,[]) 

let prepare_events slide = 
  let () =
    event_list := ([], -1 ,  Timeline.construct_chronology slide);
    Timeline.prepare_events_tags slide
  in
  advance_events ()


let drop ()  =
  let focus = body##firstChild 
   and c = !current_frame in 
  let really_drop focus =
   Timeline.clear_status focus; 
   if c==n_slides - 1 then 
    ignore @@ unfocus focus
  else
    let cf = unfocused##childNodes##item(c) in
    Dom.insertBefore unfocused focus cf in 
 Js.Opt.iter focus really_drop;
 reset_events ()

let pick_frame k ()=
  let cf=unfocused##childNodes##item(k) in
  Js.Opt.map cf (fun x -> prepare_events x; Utils.insertFirst body x )

let pick () = pick_frame !current_frame () |> ignore 

let incr_slide () = if !current_frame = n_slides-1 then () else 
 incr current_frame 

let decr_slide () = if !current_frame = 0 then () else decr current_frame 


let seq_ l = List.fold_left (fun () f -> f () ) () l 


let incr_time () = match !event_list with 
  | _,_, [] -> seq_ [drop;incr_slide;pick]
  | _ -> advance_events () 

let decr_time () =
  match !event_list with 
  | _ ,0, _ -> seq_ [drop;decr_slide;pick]
  | _ -> reverse_events () 



let keyboard_action key handler=
  let key = key##keyCode in 
  match key  with
  | 37 | 38 -> decr_time(); Lwt.return ()    
  | 39 | 40 -> incr_time(); Lwt.return () 
  | _ -> Lwt.return ()



let centerT () =
  let targets = document##querySelectorAll(Js.string "[center]") in
  let center node =
    let centering = Dom_html.createDiv document in
    node##removeAttribute(Js.string "center");
    centering##classList##add(Js.string "centered");
    encapsulate centering node |> ignore in
  Utils.node_iter targets center 
		
let () = 
  let slides = prepare_slides slides in
  let () = Utils.node_iter slides add_title in
  let () = centerT () in
  let _ = Lwt_js_events.keypresses window keyboard_action in
  ignore @@ pick_frame 0 ()
