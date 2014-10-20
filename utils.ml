
let window = Js.Unsafe.variable "window" 

let insertFirst p e = Dom.insertBefore p e @@ p##firstChild

let node_iter nl f =
  let n = nl##length in
  for k=0 to n-1 do
    match Js.Opt.to_option nl##item(k) with
    | Some x -> f x 
    | None -> ()  
  done 


let node_fold f start nl=
  let result = ref start in
  node_iter nl ( fun x -> result := f !result x );
  !result

let node_consume nl f =
  let rec consume () = match nl##length with
    | 0 -> ()
    | _ -> Js.Opt.iter nl##item(0) f ; consume () in
consume ()



let node_iteri nl f =  
  let n = nl##length in
  for k=0 to n-1 do
    match Js.Opt.to_option nl##item(k) with
    | Some x -> f k x 
    | None -> ()  
  done 
