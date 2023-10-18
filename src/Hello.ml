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
let max_index = 4

let get_scale time index =
  let index = Float.of_int index in
  let max_index = Float.of_int max_index in
  let scale = (((max_index -. index) *. 0.5) +. 1.) *. (time +. 1.) in
  scale

let color_of_index = function
  | 0 -> "red"
  | 1 -> "pink"
  | 2 -> "green"
  | 3 -> "yellow"
  | _ -> "black"

let draw_image canvas image time index =
  let scale = get_scale time index in

  if (scale >= 0.5 && scale <= 1.) || true then (
    let image_size = HtmlImageElement.width image |> Float.of_int in
    let scaled_size = image_size *. scale in
    let position = (Float.of_int canvas_size /. 2.) -. (scaled_size /. 2.) in

    Js.log4 "index:" index "scale:" scale;
    Js.log4 "position:" position "size:" scaled_size;

    (* Canvas2d.setFillStyle canvas String (color_of_index index); *)
    (* Canvas2d.fillRect ~x:0. ~y:0. ~w:scaled_size ~h:scaled_size canvas; *)
    (* Js.log2 "size:" scaled_size; *)
    drawImage canvas image ~sx:0. ~sy:0. ~sw:image_size ~sh:image_size
      ~dx:position ~dy:position ~dw:scaled_size ~dh:scaled_size)
  else ()

let load_image url =
  let promise, resolve = Promise.pending () in
  let image = HtmlImageElement.make () in
  HtmlImageElement.setSrc image url;
  HtmlImageElement.addLoadEventListener (fun _ -> resolve image) image;
  promise

let tick time =
  let canvas = Option.get (Document.getElementById "canvas" document) in
  let ctx = CanvasElement.getContext2d canvas in

  (* Canvas2d.setFillStyle ctx String "blue"; *)
  (* Canvas2d.fillRect ~x:0. ~y:0. ~w:10000. ~h:10000. ctx; *)
  let* image1 = load_image "1.png" in

  (* let* image2 = load_image "2.png" in *)
  (* let* image3 = load_image "3.png" in *)
  (* let* image4 = load_image "4.png" in *)
  draw_image ctx image1 time 0;

  (* draw_image ctx image2 time 2; *)
  (* draw_image ctx image3 time 1; *)
  (* draw_image ctx image4 time 0; *)
  Promise.resolved ()

(* let run () = *)
(*   Js.log "hello world"; *)
(*   let canvas = Option.get (Document.getElementById "canvas" document) in *)
(*   let ctx = CanvasElement.getContext2d canvas in *)
(*   (* let image = HtmlImageElement.make () in *) *)
(*   (* HtmlImageElement.setSrc image "image.jpg"; *) *)
(*   (* HtmlImageElement.addLoadEventListener (fun _ -> draw_image ctx image) image; *) *)
(*   let* image1 = load_image "1.png" in *)
(*   Canvas2d.setFillStyle ctx String "blue"; *)
(*   Canvas2d.fillRect ~x:0. ~y:0. ~w:10000. ~h:10000. ctx; *)
(*   Canvas2d.setFillStyle ctx String "red"; *)
(*   Canvas2d.fillRect ~x:10. ~y:10. ~w:100. ~h:100. ctx; *)

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
(* let* key = promise in *)
(*     Document.removeKeyDownEventListener handler document; *)
(*     key *)

let loop () =
  let rec loop time =
    let* _ = tick time in

    let* input = wait_input () in
    let new_time =
      match input with
      | Some Plus -> time +. 0.1
      | Some Minus -> time -. 0.1
      | _ -> time
    in

    loop new_time
  in
  loop 0.

let () =
  Js.log "hello world";

  loop () |> ignore
(* Canvas2d.fillRect ctx 10. 10. 100. 100. *)
(* Webapi.Canvas.Canvas2d. *)
