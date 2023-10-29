open Webapi.Canvas
open Tea.App
open Tea
open Tea.Html2
open Tea.Html2.Events
open Tea.Html2.Attributes
(* open Tea.Web *)

type msg =
  | AddImage of string
  | LoadedImage of Webapi.Dom.HtmlImageElement.t
  | LoadedImages of Webapi.Dom.HtmlImageElement.t list
  | Tick of AnimationFrame.t

type model = {
  canvas : Canvas2d.t Option.t;
  show_images : bool;
  images : Webapi.Dom.HtmlImageElement.t list;
}

let load_image msg url =
  Cmd.call (fun callbacks ->
      Js.log2 "loading image:" url;
      let images = Renderer.load_image url in
      Promise.map images (fun image ->
          Js.log2 "loaded image:" url;
          !callbacks.enqueue (msg image))
      |> ignore;
      ())

let load_images msg =
  Cmd.call (fun callbacks ->
      Js.log "loading images";
      let images = Renderer.load_images "interdim" 20 in
      Promise.map images (fun images ->
          Js.log "loaded images!";
          !callbacks.enqueue (msg images))
      |> ignore;
      ())

let render images time =
  Cmd.call (fun _callbacks ->
      let ctx =
        Webapi.Dom.Document.getElementById "canvas" Webapi.Dom.document
        |> Option.get |> CanvasElement.getContext2d
      in
      Renderer.tick ctx images (time /. 1000. *. 0.25);
      ())

let init () =
  ( { canvas = None; show_images = true; images = [] },
    load_images (fun images -> LoadedImages images) )

let update model msg =
  match msg with
  | AddImage url ->
      Js.log2 "adding image:" url;
      (* (model, load_image (fun image -> LoadedImage image) url) *)
      (model, Cmd.none)
  | LoadedImages images -> ({ model with images }, Cmd.none)
  | LoadedImage image ->
      ({ model with images = image :: model.images }, Cmd.none)
  | Tick { time; _ } -> (model, render model.images time)

let view_button title msg = button [ onClick msg ] [ text title ]

let images_list images =
  let loaded_images =
    images
    |> List.map (fun image ->
           let url = Webapi.Dom.HtmlImageElement.src image in
           li [] [ img [ src url; width 100; height 100 ] [] ])
  in

  ul
    [ class' "flex flex-row" ]
    (loaded_images
    @ [
        input'
          [
            type' "file";
            accept "image/*";
            multiple true;
            onInput (fun url -> AddImage url);
          ]
          (* [ type' "file"; accept "image/*"; onInput (fun url -> AddImage url) ] *)
          [ button [] [ text "add image" ] ];
      ])

let view model =
  div []
    [
      canvas [ id "canvas"; width 1024; height 1024 ] [];
      images_list model.images;
    ]

let subscriptions model =
  match model.images with
  | [] -> Sub.none
  | _ -> AnimationFrame.every (fun time -> Tick time)

let main = standardProgram { init; update; view; subscriptions }

let () =
  let root = Webapi.Dom.Document.getElementById "root" Webapi.Dom.document in
  main (Option.map (fun root -> root |> Webapi.Dom.Element.asNode) root) ()
  |> ignore
