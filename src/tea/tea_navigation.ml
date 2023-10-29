type ('flags, 'model, 'msg) navigationProgram = {
  init : 'flags -> Web.Location.location -> 'model * 'msg Tea_cmd.t;
  update : 'model -> 'msg -> 'model * 'msg Tea_cmd.t;
  view : 'model -> 'msg Vdom.t;
  subscriptions : 'model -> 'msg Tea_sub.t;
  shutdown : 'model -> 'msg Tea_cmd.t;
}

let getLocation () = Web.Location.asRecord (Web.Document.location ())
let notifier : (Web.Location.location -> unit) option ref = ref None

let notifyUrlChange () =
  match !notifier with
  | None -> ()
  | Some cb ->
      let location = getLocation () in
      let () = cb location in
      ()

let subscribe tagger =
  let open Vdom in
  let enableCall callbacks =
    let notifyHandler location = callbacks.enqueue (tagger location) in
    let () = notifier := Some notifyHandler in
    let handler : Vdom.eventCallback =
     fun [@mel] _event -> notifyUrlChange ()
    in
    let () =
      Webapi.Dom.Window.addEventListener "popstate" handler Webapi.Dom.window
    in
    fun () ->
      Webapi.Dom.Window.removeEventListener "popstate" handler Webapi.Dom.window
  in
  Tea_sub.registration "navigation" enableCall

let replaceState url =
  let _ =
    Web.Window.History.replaceState Web.Window.window (Js.Json.parseExn "{}") ""
      url
  in
  ()

let pushState url =
  let _ =
    Web.Window.History.pushState Web.Window.window (Js.Json.parseExn "{}") ""
      url
  in
  ()

let modifyUrl url =
  Tea_cmd.call (fun _enqueue ->
      let () = replaceState url in
      let () = notifyUrlChange () in
      ())

let newUrl url =
  Tea_cmd.call (fun _enqueue ->
      let () = pushState url in
      let () = notifyUrlChange () in
      ())

let go step =
  Tea_cmd.call (fun _enqueue ->
      let _ = Web.Window.(History.go window) step in
      let () = notifyUrlChange () in
      ())

let back step = go (-step)
let forward step = go step

let navigationProgram locationToMessage stuff =
  let init flag = stuff.init flag (getLocation ()) in

  let subscriptions model =
    Tea_sub.batch [ subscribe locationToMessage; stuff.subscriptions model ]
  in

  let open! Tea_app in
  program
    {
      init;
      update = stuff.update;
      view = stuff.view;
      subscriptions;
      shutdown = stuff.shutdown;
    }
