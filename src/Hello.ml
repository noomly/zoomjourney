open Webapi.Canvas
open Webapi.Dom

module Async = struct
  let ( let+ ) = Promise.map
  let ( let* ) = Promise.flatMap
  let ( and* ) a b = Promise.all2 a b
end

open Async

external drawImage :
  Canvas2d.t ->
  HtmlImageElement.t ->
  sx:float ->
  sy:float ->
  sw:float ->
  sh:float ->
  dx:float ->
  dy:float ->
  dw:float ->
  dh:float ->
  unit = "drawImage"
[@@mel.send]

let canvas_size = 1024

(* let image_size = 1024 *)
(* let max_index = 4 *)
let mod_float a b = a -. (float_of_int (int_of_float (a /. b)) *. b)
let get_z max_index time = mod_float (time *. 1.) (Float.of_int max_index)

let get_scale max_index time index =
  let index = Float.of_int index in
  let scale = (2. ** get_z max_index time) *. (0.5 ** index) in
  scale

let color_of_index = function
  | 0 -> "red"
  | 1 -> "pink"
  | 2 -> "green"
  | 3 -> "yellow"
  | _ -> "black"

let draw_image canvas max_index image time index =
  let scale = get_scale max_index time index in

  if scale > 0. then
    let margin = 100. in
    let image_size =
      HtmlImageElement.width image |> Float.of_int |> fun size ->
      size -. (margin /. 2.)
    in
    let scaled_size = image_size *. scale in
    let position = (Float.of_int canvas_size /. 2.) -. (scaled_size /. 2.) in

    drawImage canvas image ~sx:(margin /. 2.) ~sy:(margin /. 2.)
      ~sw:(image_size -. (margin /. 2.))
      ~sh:(image_size -. (margin /. 2.))
      ~dx:position ~dy:position ~dw:scaled_size ~dh:scaled_size
  else ()

let slice_list list slice_start slice_length =
  let rec aux acc i =
    if i < slice_length then
      let adjusted_start =
        if slice_start >= 0 then slice_start else List.length list + slice_start
      in
      let idx = (adjusted_start + i) mod List.length list in
      let elem = List.nth list idx in
      aux (acc @ [ elem ]) (i + 1)
    else acc
  in
  aux [] 0

(* let get_visibles images time = *)
(*   let indexed_images = List.mapi (fun i img -> (i, img)) images in *)
(* let z = get_z (List.length images) time |> Int.of_float in *)
(* let max_index = List.length images in *)
(* let size = *)
(*   if z + 3 > max_index then z + (max_index - z) else Int.min 6 max_index *)
(* in *)
(* slice_list indexed_images (z - 3) size *)

let get_visibles images time =
  let indexed_images = List.mapi (fun i img -> (i, img)) images in
  slice_list indexed_images
    (Int.of_float (get_z (List.length images) time -. 3.))
    6
  |> List.sort (fun (i1, _) (i2, _) -> Int.compare i1 i2)

let load_image url =
  let promise, resolve = Promise.pending () in
  let image = HtmlImageElement.make () in
  HtmlImageElement.setSrc image url;
  HtmlImageElement.addLoadEventListener (fun _ -> resolve image) image;
  promise

let rec load_images path n =
  if n == 0 then Promise.resolved []
  else
    let* image = load_image (path ^ "/" ^ Int.to_string n ^ ".png") in
    let* images = load_images path (n - 1) in
    Promise.resolved (image :: images)

let tick images time =
  let canvas = Option.get (Document.getElementById "canvas" document) in
  let ctx = CanvasElement.getContext2d canvas in

  let visibles = get_visibles images time in

  List.iter
    (fun (i, image) -> draw_image ctx (List.length images) image time i)
    visibles;

  Promise.resolved ()

type input_event = Plus | Minus

let wait_input () =
  let promise, resolve = Promise.pending () in
  let handler e =
    match KeyboardEvent.key e with
    | "+" -> Some Plus |> resolve
    | "-" -> Some Minus |> resolve
    | _ -> None |> resolve
  in

  Document.addKeyDownEventListener handler document;
  Promise.tap promise (fun _ ->
      Document.removeKeyDownEventListener handler document)

let loop images =
  let rec loop images time =
    (* let* input = wait_input () in *)
    (* let new_time = *)
    (*   match input with *)
    (*   | Some Plus -> time +. 0.1 *)
    (*   | Some Minus -> time -. 0.1 *)
    (*   | _ -> time *)
    (* in *)
    (* new_time |> ignore; *)
    let* _ = tick images (time /. 1000. *. 0.5) in

    Webapi.requestAnimationFrame (fun time -> loop images time |> ignore);

    Promise.resolved ()
  in

  Webapi.requestAnimationFrame (fun _ -> loop images 0. |> ignore)

let () =
  let main_async =
    Js.log "hello world";

    let* images = load_images "interdim" 20 in
    (* let* images = load_images "test" 4 in *)
    loop images;

    Promise.resolved ()
  in
  main_async |> ignore
