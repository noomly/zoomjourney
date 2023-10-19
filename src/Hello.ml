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

let get_scale max_index time index =
  let index = Float.of_int index in
  (* let max_index = Float.of_int max_index in *)
  (* let scale = (((max_index -. index) *. 0.5) +. 1.) *. (time +. 1.) in *)
  (* let scale = ((2. ** index) /. 2.) +. time in *)
  let scale =
    (2. ** mod_float time (Float.of_int max_index -. 0.5)) *. (0.5 ** index)
  in
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

    (* if index = 1 || index = 2 then *)
    (*   Js.log ("index:", index, "time:", time, "scale:", scale); *)

    (* Js.log4 "position:" position "size:" scaled_size; *)

    (* Canvas2d.setFillStyle canvas String (color_of_index index); *)
    (* Canvas2d.fillRect ~x:0. ~y:0. ~w:scaled_size ~h:scaled_size canvas; *)
    drawImage canvas image ~sx:(margin /. 2.) ~sy:(margin /. 2.)
      ~sw:(image_size -. (margin /. 2.))
      ~sh:(image_size -. (margin /. 2.))
      ~dx:position ~dy:position ~dw:scaled_size ~dh:scaled_size
  else ()

(* let slice_list lst start n = *)
(*   let len = List.length lst in *)
(*   let idx i = i mod len in *)
(*   let rec aux acc i count = *)
(*     if count <= 0 then List.rev acc *)
(*     else aux (List.nth lst (idx (start + i)) :: acc) (i + 1) (count - 1) *)
(*   in *)
(*   aux [] 0 n *)

(* let slice_list list slice_start slice_length = *)
(*   let rec aux acc i = *)
(*     if i < slice_length then *)
(*       let idx = (slice_start + i) mod List.length list in *)
(*       let elem = List.nth list idx in *)
(*       aux (acc @ [ elem ]) (i + 1) *)
(*     else acc *)
(*   in *)
(*   aux [] 0 *)

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

(* let slice_list lst start n = *)
(*   let len = List.length lst in *)
(*   let idx i = i mod len in *)
(*   let rec aux acc i count = *)
(*     if count <= 0 then List.rev acc *)
(*     else aux (List.nth lst (idx (start + i)) :: acc) (i + 1) (count - 1) *)
(*   in *)
(*   aux [] 0 n *)

let get_visibles images time =
  slice_list images
    (Int.of_float (mod_float time (Float.of_int (List.length images)) -. 3.))
    6
(* (Int.of_float time) (Float.of_int (List.length images)) -. 3.)) *)
(* 6 *)

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

let tick _images time =
  let canvas = Option.get (Document.getElementById "canvas" document) in
  let ctx = CanvasElement.getContext2d canvas in

  (* Canvas2d.setFillStyle ctx String "blue"; *)
  (* Canvas2d.fillRect ~x:0. ~y:0. ~w:10000. ~h:10000. ctx; *)
  let* image1 = load_image "interdim/1.png" in
  let* image2 = load_image "interdim/2.png" in
  let* image3 = load_image "interdim/3.png" in
  let* image4 = load_image "interdim/4.png" in
  let* image5 = load_image "interdim/5.png" in
  let* image6 = load_image "interdim/6.png" in
  let* image7 = load_image "interdim/7.png" in
  let* image8 = load_image "interdim/8.png" in
  let* image9 = load_image "interdim/9.png" in
  let* image10 = load_image "interdim/10.png" in
  let* image11 = load_image "interdim/11.png" in
  let* image12 = load_image "interdim/12.png" in
  let* image13 = load_image "interdim/13.png" in
  draw_image ctx 13 image13 time 0;
  draw_image ctx 13 image12 time 1;
  draw_image ctx 13 image11 time 2;
  draw_image ctx 13 image10 time 3;
  draw_image ctx 13 image9 time 4;
  draw_image ctx 13 image8 time 5;
  draw_image ctx 13 image7 time 6;
  draw_image ctx 13 image6 time 7;
  draw_image ctx 13 image5 time 8;
  draw_image ctx 13 image4 time 9;
  draw_image ctx 13 image3 time 10;
  draw_image ctx 13 image2 time 11;
  draw_image ctx 13 image1 time 12;
  Promise.resolved ()

(* let tick images time = *)
(*   let canvas = Option.get (Document.getElementById "canvas" document) in *)
(*   let ctx = CanvasElement.getContext2d canvas in *)
(**)
(*   (* let images = List.rev images in *) *)
(*   let visibles = get_visibles images time in *)
(**)
(*   List.iteri *)
(*     (fun i image -> draw_image ctx (List.length images) image time i |> ignore) *)
(*     visibles; *)
(**)
(*   (* Canvas2d.setFillStyle ctx String "blue"; *) *)
(*   (* Canvas2d.fillRect ~x:0. ~y:0. ~w:10000. ~h:10000. ctx; *) *)
(**)
(*   (* let* image1 = load_image "interdim/1.png" in *) *)
(*   (* let* image2 = load_image "interdim/2.png" in *) *)
(*   (* let* image3 = load_image "interdim/3.png" in *) *)
(*   (* let* image4 = load_image "interdim/4.png" in *) *)
(*   (* let* image5 = load_image "interdim/5.png" in *) *)
(*   (* let* image6 = load_image "interdim/6.png" in *) *)
(*   (* let* image7 = load_image "interdim/7.png" in *) *)
(*   (* let* image8 = load_image "interdim/8.png" in *) *)
(*   (* let* image9 = load_image "interdim/9.png" in *) *)
(*   (* let* image10 = load_image "interdim/10.png" in *) *)
(*   (* let* image11 = load_image "interdim/11.png" in *) *)
(*   (* let* image12 = load_image "interdim/12.png" in *) *)
(*   (* let* image13 = load_image "interdim/13.png" in *) *)
(*   (* draw_image ctx image13 time 0; *) *)
(*   (* draw_image ctx image12 time 1; *) *)
(*   (* draw_image ctx image11 time 2; *) *)
(*   (* draw_image ctx image10 time 3; *) *)
(*   (* draw_image ctx image9 time 4; *) *)
(*   (* draw_image ctx image8 time 5; *) *)
(*   (* draw_image ctx image7 time 6; *) *)
(*   (* draw_image ctx image6 time 7; *) *)
(*   (* draw_image ctx image5 time 8; *) *)
(*   (* draw_image ctx image4 time 9; *) *)
(*   (* draw_image ctx image3 time 10; *) *)
(*   (* draw_image ctx image2 time 11; *) *)
(*   (* draw_image ctx image1 time 12; *) *)
(*   Promise.resolved () *)

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

  Webapi.requestAnimationFrame (fun d ->
      Js.log d;
      loop images 0. |> ignore)

let () =
  let main_async =
    Js.log "hello world";

    let* images = load_images "interdim" 13 in
    (* let* images = load_images "test" 4 in *)
    loop images;

    Promise.resolved ()
  in

  main_async |> ignore
