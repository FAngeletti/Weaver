

let document = Dom_html.document

let get_or_create name =
  let jname= Js.string name in
  let m_element = document##getElementsByTagName(jname)##item(0) in
  Js.Opt.get m_element (fun () -> document##createElement(jname) ) 

let root =  get_or_create "html" 

let body = get_or_create "body"


let window = Js.Unsafe.variable "window"


let consume_childs element f =
  let next () = Js.Opt.to_option @@ element ## firstChild in
  let rec feed ()= match next() with
    | Some c -> f c; feed () 
    | None -> () in
  feed()



let transfer_attrs origin target = 
  let t_attrs = target##attributes in 
  let clone attr = Dom.CoerceTo.attr attr##cloneNode(Js._false) in
  let transfer attr = 
    Js.Opt.iter (clone attr) @@ fun attr -> ignore @@ t_attrs##setNamedItem(attr) in
  Utils.node_iter origin##attributes transfer; target

let transfer_classes origin target = 
  Utils.classes_iter (origin##classList) (fun c ->  target##classList##add(c) |> ignore );
  target

let transfer_childs origin target  = 
  let transfer child = target##appendChild(child) |> ignore in 
  consume_childs origin transfer; target 

let parent_or_root element = 
  let node = (element :> Dom.node Js.t) in
  Js.Opt.get (node##parentNode) (fun () -> (root :> Dom.node Js.t)  )  

let replace_tag_name tagname classlist (element:Dom_html.element Js.t) =
  let fae= document##createElement(tagname) in
  let fae = fae 
  |> transfer_attrs element 
  |> transfer_classes element
  |> transfer_childs element in
  List.iter (fun c -> fae##classList##add(Js.string c)) classlist; 
  let parent = parent_or_root element in 
  Dom.replaceChild parent fae element;
  fae




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


let digit c = int_of_char c - 48

let parse_error s =
  window##alert(Js.string s)

type time_interval = 
 | Interval of int*int 
 | Point of int 
 | Right of int

let parse_timeline s =
  let n = String.length s-1 in
  let pos = ref 0 in
  let token =
    fun () -> let p = !pos in
	      if p> n then None else (incr pos; Some s.[p]) in
  let undo () = decr pos in
  let ( |>| ) f g = let a = f () in a ::g () in
  let ( >>= ) f g=  let a= f ()  in g a in    
  let rec start ()=  parse_interval |>|  parse_union
  and parse_r_interval ns ()= match token () with
    | Some('-') -> parse_int 0 >>= fun n -> Interval (ns, n )
    | Some('.') -> 
       let tok = token () in
       if tok = Some('.') then  Right ns else parse_error @@ Printf.sprintf "Expected character [.] in timeline %s at position %d " s !pos
    | None -> Point ns  
    | _  ->  undo(); Point ns
  and parse_interval () = ( (parse_int 0) >>= parse_r_interval ) () 
  and parse_int n ()= match token () with
    | Some('0'..'9' as c) ->  parse_int ( n*10 + digit c ) ()
    | None ->  n
    | _ -> undo(); n
  and parse_union () = match token () with
    | Some('+') -> parse_interval |>|  parse_union  
    | None -> []
    | Some(c) -> parse_error @@ Printf.sprintf "Unexpected [%c] character while parsing timeline %s" c s in
  start ()

let ( <? ) x is = 
  let x_is_in = function
    | Point p -> x=p
    | Right p -> x>=p
    | Interval (s,e) -> x>=s && x<=e in
  List.fold_left (fun t i -> t || x_is_in i ) false is   

let upper_time is  = 
  let upper = function
    | Point p-> p
    | Right p -> 0
    | Interval(s,e) -> e in
List.fold_left (fun u i -> max u @@ upper i ) 0 is 


type action = Activate| Desactivate

type  event = {time:int;action:action;node: Dom_html.element Js.t}

let jtime = Js.string "timeline"
let jdis = Js.string "disactivated"
let jact = Js.string "activated"

let fold_attribute attr f start origin =
  let element_f n acc = match Js.Opt.to_option n##getAttribute(attr) with
    | Some attr -> f n attr acc
    | None -> acc in
  let node_f acc n = let e =  Dom_html.CoerceTo.element n in
  match Js.Opt.to_option e with 
  | Some e ->  element_f e acc
  | None -> acc  in
let rec children acc parent = Utils.node_fold children (node_f acc parent) parent##childNodes in
children start origin



let construct_chronology slide= 
  let blink n start ending events = 
    {time=ending+1; action=Desactivate; node  = n }
    ::{time = start; action=Activate; node = n} :: events in
  let add_event n events= function 
    | Point p -> blink n p p events
    | Interval (s,e) -> blink n s e events
    | Right s -> {time=s;action=Activate;node = n} ::events in
  let add_timeline n attr events  = 
    let timeline = parse_timeline @@ Js.to_string attr in
    List.fold_left (add_event n) events timeline in
  let events = fold_attribute jtime add_timeline [] slide in
  let compare_event ev1 ev2 = compare ev1.time ev2.time in
  List.sort compare_event events


let prepare_frames () = 
  let frames = document##getElementsByTagName(Js.string "slide") in
  Utils.node_consume frames ( fun n -> 
			       n 
			       |> replace_tag_name (Js.string "div") ["slide"]
			       |> unfocus 
			       |> ignore 
			      ) ;
  document##querySelectorAll(Js.string ".slide")





let frames = prepare_frames () 
let n_slides = frames##length
let current_frame = ref 0
let event_list = ref ( [], 0,  [] ) 

let flow_event_to past target future test action = 
  let rec move past = function 
    | [] -> past, []
    | a::q when test a.time target -> action a; move (a::past) q
    | l -> past, l in
  move past future

let reverse_action = function 
  | Activate -> Desactivate
  | Desactivate -> Activate 

let apply_event e = match e.action with 
  | Activate -> e.node##classList##remove(jdis)
  | Desactivate -> e.node##classList##add(jdis)


let clear_status slide = 
  let clear element attr () = element##classList##add(jdis) in
  fold_attribute jtime clear () slide  



let reverse_event e = apply_event { e with action = reverse_action e.action } 


let advance_events () = 
  let past, present, futur = !event_list in
  let past,futur = flow_event_to past (present+1) futur (<=) apply_event in
  event_list := past, present+1, futur 

let reverse_events () = 
  let past, present, futur = !event_list in
  let futur,past = flow_event_to futur (present-1) past (>) reverse_event in
  event_list := past, present-1, futur


let reset_events () = event_list := ([],-1,[]) 

let prepare_events slide = 
  let () = event_list := ([], -1 ,  construct_chronology slide) in
  advance_events ()


let drop ()  =
  let focus = body##firstChild 
   and c = !current_frame in 
 let really_drop focus =  
   clear_status focus; 
   if c==n_slides - 1 then 
    ignore @@ unfocus focus
  else
    let cf = unfocused##childNodes##item(c) in
    Dom.insertBefore unfocused focus cf in 
 ignore @@ Js.Opt.map focus really_drop;
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

let decr_time () = match !event_list with 
  | _ ,0, _ -> seq_ [drop;decr_slide;pick]
  | _ -> reverse_events () 



let keyboard_action key handler=
  let key = key##keyCode in 
  match key  with
  | 37 | 38 -> decr_time(); Lwt.return ()    
  | 39 | 40 -> incr_time(); Lwt.return () 
  | _ -> Lwt.return ()

let () = 
  let frames = prepare_frames () in 
  let () = Utils.node_iter frames add_title in
  let _ = Lwt_js_events.keypresses window keyboard_action in
  ignore @@ pick_frame 0 ()
