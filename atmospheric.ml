
let window= Js.Unsafe.variable "window"

let n = ref 1 
let dt = ref 0.1
let radius = ref 0.01
let radh ()=  !radius/.2.
type vec2 = {x:float; y:float}
let dim = 3

type env = {v:float array;p:float array; step : int }	    
let env = ref [{ v= Array.make 3 0.; p = Array.make 3 0.; step=0} ] 			   

let n() = match !env with
  | {p;_}::q -> Array.length p
  | [] -> 0
    

let velocity ()= match !env with
  | {v;_}::q -> v
  | [] -> Array.make 0 0.

let position () = match !env with
  |{v;_}::q -> v
  | [] -> Array.make 0 0.

let step () = match !env with
  | [] -> -1
  | {step;_}::q -> step 
	      
let ctx=ref None
			   
module Bigarray =
  struct
    module Array1 = struct
      let get a x = let s = x * dim and a = a() in
		    a.(s), a.(s+1), a.(s+2) 
      let set a p (x,y,z) = let s = p * dim and a = a() in
       	    a.(s)<-x; a.(s+1)<-y; a.(s+2)<-z 

    end
    module Array2= struct
      let get a x y = (a()).(x*dim+y)
      let set a x y v = (a()).(x*dim+y) <- v 
      end
  end

let ()=
    position.{0} <- (0.5,0.5,0.5);
    velocity.{0} <- (0.1, - 0.2, 0.3) 

let iter_p f =
  for p=0 to n()-1 do
    f p
  done
    
		      
let update_p dt pos =
for i=0 to dim-1 do 
  position.{pos,i} <- position.{pos,i} +. dt *. velocity.{pos,i}  
done

let guard pos =
  let reflect d i =
    position.{pos,i}<-d ; velocity.{pos,i} <- -.velocity.{pos,i} in
  for i =0 to dim-1 do
    let x= position.{pos,i} in
    let radh = radh() in
    match x<radh,x+.radh>=1. with
    | true, _  -> reflect radh i
    | _, true -> reflect (1.-.radh) i
    | _ -> () 
  done

let (|||) f g = fun a -> f a; g a      
    
let update_g dt =
  iter_p ( update_p dt ||| guard )  


let draw_p ctx pos =
  let x,y,_ = position.{pos} in
  ctx##beginPath();
  ctx##arc(x,y,!radius,0.,6.3, Js._false);
  ctx##fill()

let draw ctx= iter_p  @@ draw_p ctx
let may f = function
  |Some x-> f x
  | None -> ()
				

let phydt = 0.025
			       
let rec update () =
  let clear ctx = ctx##clearRect(0.,0.,1.,1.)  in
  may clear !ctx;
  update_g !dt; 
  may draw !ctx;
  Lwt.( Lwt_js.sleep phydt >>= update ) 
			
let timer = ref ( Lwt.return () ) 

			       
let grab_ctx () =
  let doc = Dom_html.document in
  let mcanvas = doc##getElementById(Js.string "atmospheric") in
  let canvas =  Js.Opt.bind mcanvas Dom_html.CoerceTo.canvas in
  let mctx=Js.Opt.map canvas ( fun canvas->
			       (canvas##getContext(Dom_html._2d_ )), (canvas##width), (canvas##height)  ) in 
  Js.Opt.iter mctx ( fun (ctx_v,w,h) ->
		     may (fun ctx-> ctx##fillStyle <- Js.string "black") !ctx;
		     ctx:= Some ctx_v;
		     ctx_v##scale(float_of_int w, float_of_int h);
		   )
let start el =
  match !ctx with
  | Some  ctx -> timer:= update ()
  | None -> grab_ctx() ; timer := update () 
	      
let stop el =  Lwt.cancel !timer; timer:= Lwt.return () 

let run status el =
  let open Timeline in
  match status with
  | Activate -> start el
  | Desactivate -> stop el

			 
let step status el = 
  match step() with
  | _ -> ()

	   
let suspend status el = ()

let name= "atmospheric"

let animation = Timeline.{ name; run ; step ;suspend }
  

  
