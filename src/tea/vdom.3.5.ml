type eventCallback = Dom.event -> unit

type 'msg systemMessage =
  | Render
  | AddRenderMsg of 'msg
  | RemoveRenderMsg of 'msg

type 'msg applicationCallbacks = {
  enqueue: 'msg -> unit;
  on: 'msg systemMessage -> unit;
}

type 'msg eventHandlerCallback = string * (Dom.event -> 'msg option)
type 'msg eventHandler =
  | EventHandlerCallback of 'msg eventHandlerCallback
  | EventHandlerMsg of 'msg

type 'msg eventCache = {
  handler: eventCallback;
  cb: 'msg eventHandlerCallback option ref;
}

type 'msg property =
  | NoProp
  | RawProp of string * string
  | Attribute of string * string * string
  | Data of string * string
  | Event of string * 'msg eventHandler * 'msg eventCache option ref
  | Style of (string * string) list

type 'msg properties = 'msg property list

type 'msg t =
  | CommentNode of string
  | Text of string
  | Node of string * string * string * string * ('msg properties) * ('msg t list)

  | LazyGen of string * (unit -> 'msg t) * 'msg t ref
  | Tagger of ('msg applicationCallbacks ref -> 'msg applicationCallbacks ref) * 'msg t

let noNode: 'msg t = CommentNode ""

let comment (s: string): 'msg t = CommentNode s

let text (s: string): 'msg t = Text s

let fullnode (namespace: string) (tagName: string) (key: string) (unique: string) (props: 'msg properties ) (vdoms: 'msg t list): 'msg t =
  Node(namespace, tagName, key, unique, props, vdoms)

let node ?namespace:(namespace="") tagName ?(key="") ?(unique="") (props: 'msg properties ) vdoms: 'msg t =
  fullnode namespace tagName key unique props vdoms

let lazyGen (key: string) (fn: unit -> 'msg t): 'msg t =
  LazyGen(key, fn, ref noNode)

let noProp: 'msg property = NoProp

let prop (propName: string) (value: string): 'msg property = RawProp(propName, value)

let onCB ~key:key eventName cb: 'msg property =
  Event(eventName, EventHandlerCallback(key, cb), ref None)

let onMsg (name: string) (msg: 'msg): 'msg property =
  Event(name, EventHandlerMsg(msg), ref None)

let attribute (namespace: string) (key: string) (value: string): 'msg property =
  Attribute(namespace, key, value)

let data (key: string) (value: string): 'msg property =
  Data(key, value)

let style (key: string) (value: string): 'msg property =
  Style [(key, value)]

let styles (s): 'msg property =
  Style s

let createElementNsOptional namespace tagName =
  (* let document = Webapi.Dom.document in *)
  match namespace with
  | "" -> Webapi.Dom.Document.createElement tagName
  | ns -> Webapi.Dom.Document.createElementNS ns tagName

let nodeAt (index: int) (nodes: Dom.nodeList): Dom.node =
  Webapi.Dom.NodeList.item index nodes |> Belt.Option.getExn

external setItem: (Webapi.Dom.Element.t -> 'key -> 'value -> unit) = ""
[@@mel.set_index]
external getItem: (Webapi.Dom.Element.t -> 'key -> 'value) = ""
[@@mel.get_index]
let delItem (elem: Dom.element) (key: 'key) =
  setItem elem key Js.Undefined.empty

let rec renderToHtmlString: 'msg t -> string = function
  | CommentNode s -> "<!-- " ^ s ^ " -->"
  | Text s -> s
  | Node (namespace, tagName, _, _, props, vdoms) ->
    let renderProp x = match x with
      | NoProp -> ""
      | RawProp (k, v) -> " " ^ k ^ "=\"" ^ v ^ "\""
      | Attribute (_, k, v) -> " " ^ k ^ "=\"" ^ v ^ "\""
      | Data (k, v) -> " data-" ^ k ^ "=\"" ^ v ^ "\""
      | Event (_, _, _) -> ""
      | Style s ->
        " style=\"" ^ List.fold_left (fun acc (k, v) -> acc ^ k ^ ":" ^ v ^ ";") "" s ^ "\""
    in
    "<" ^ namespace ^ (if namespace = "" then "" else ":") ^ tagName ^
    List.fold_left (fun acc p -> acc ^ renderProp p) "" props ^ ">" ^
    List.fold_left (fun acc v -> acc ^ renderToHtmlString v) "" vdoms ^
    "</" ^ tagName ^ ">"
  | LazyGen (_, gen, cache) ->
    let vdom = gen () in
    renderToHtmlString vdom
  | Tagger (tagger, vdom) -> renderToHtmlString vdom

let emptyEventHandler: eventCallback = fun _ev -> ()

let emptyEventCB = fun _ev -> None

let eventHandler callbacks cb =
  let handler ev = match cb ev with
    | None -> ()
    | Some msg -> callbacks.enqueue msg
  in
  handler

let eventHandlerGetCB: 'msg eventHandler -> Dom.event -> 'msg option =
  switch 
  | EventHandlerCallback (_, cb), _ -> cb
  | EventHandlerMsg msg, _ -> fun _ev -> Some msg

let compareEventHandlerTypes = fun left ->
  function
  | EventHandlerCallback (cb, _), x ->
    (match left with
    | EventHandlerCallback (lcb, _) when cb = lcb -> true
    | _ -> false)
  | EventHandlerMsg lmsg, x ->
    (match left with
    | EventHandlerMsg msg when msg = lmsg -> true
    | _ -> false)

let eventHandlerRegister = fun callbacks elem name handlerType ->
  let cb = ref (eventHandlerGetCB handlerType)
  in
  let handler = eventHandler callbacks cb
  in
  Webapi.Dom.EventTarget.addEventListener elem name handler;
  Some {handler=handler; cb=cb}

let eventHandlerUnregister = fun elem name ->
  function
  | None -> None
  | Some cache ->
    Webapi.Dom.EventTarget.removeEventListener elem name cache.handler;
    None

let eventHandlerMutate = fun callbacks elem oldName newName oldHandlerType newHandlerType oldCache newCache ->
  match oldCache.contents with
  | None -> newCache := eventHandlerRegister callbacks elem newName newHandlerType
  | Some oldcache ->
    if oldName = newName then begin
      newCache := oldCache.contents;
      if compareEventHandlerTypes oldHandlerType newHandlerType then ()
      else
        let cb = eventHandlerGetCB newHandlerType in
        oldcache.cb := cb
    end
    else begin
      oldCache := eventHandlerUnregister elem oldName oldCache.contents;
      newCache := eventHandlerRegister callbacks elem newName newHandlerType
    end

let patchVNodesOnElemsPropertiesApplyAdd = fun callbacks elem _idx x ->
  match x with
  | NoProp -> ()
  | RawProp (k, v) -> setItem elem k v
  | Attribute (namespace, k, v) -> Webapi.Dom.Element.setAttributeNS elem namespace k v
  | Data (k, v) ->
    Js.log ("TODO: Add Data Unhandled", k, v);
    failwith "TODO: Add Data Unhandled"
  | Event (name, handlerType, cache) ->
    let eventTarget = Webapi.Dom.Element.asEventTarget elem in
    cache := eventHandlerRegister callbacks eventTarget name handlerType
  | Style s ->
    let elem = Webapi.Dom.HtmlElement.ofElement elem in
    match elem with
    | None -> failwith "Expected htmlelement in patchVNodesOnElems_PropertiesApply_Add"
    | Some elem ->
      let elemStyle = Webapi.Dom.HtmlElement.style elem in
      List.fold_left (fun () (k,v) -> Webapi.Dom.CssStyleDeclaration.setPropertyValue elemStyle k v) () s

let patchVNodesOnElemsPropertiesApplyRemove = fun _callbacks elem _idx x ->
  match x with
  | NoProp -> ()
  | RawProp (k, _) -> delItem elem k
  | Attribute (namespace, k, _) -> Webapi.Dom.Element.removeAttributeNS elem namespace k
  | Data (k, v) ->
    Js.log ("TODO: Remove Data Unhandled", k, v);
    failwith "TODO: Remove Data Unhandled"
  | Event (name, _, cache) ->
    let eventTarget = Webapi.Dom.Element.asEventTarget elem in
    cache := eventHandlerUnregister eventTarget name cache.contents
  | Style s ->
    let elem = Webapi.Dom.HtmlElement.ofElement elem in
    match elem with
    | None -> failwith "Expected htmlelement in patchVNodesOnElems_PropertiesApply_Remove"
    | Some elem ->
      let elemStyle = Webapi.Dom.HtmlElement.style elem in
      List.fold_left (fun () (k,_v) -> Webapi.Dom.CssStyleDeclaration.removeProperty elemStyle k |> ignore) () s

let patchVNodesOnElemsPropertiesApplyRemoveAdd = fun callbacks elem idx oldProp newProp ->
  patchVNodesOnElemsPropertiesApplyRemove callbacks elem idx oldProp;
  patchVNodesOnElemsPropertiesApplyAdd callbacks elem idx newProp

let patchVNodesOnElemsPropertiesApplyMutate = fun _callbacks elem _idx oldProp x ->
  match x with
  | NoProp -> failwith "This should never be called as all entries through NoProp are gated."
  | RawProp (k, v) -> setItem elem k v
  | Attribute (namespace, k, v) -> Webapi.Dom.Element.setAttributeNS elem namespace k v
  | Data (k, v) ->
    Js.log ("TODO: Mutate Data Unhandled", k, v);
    failwith "TODO: Mutate Data Unhandled"
  | Event (_newName, _newHandlerType, _newCache) ->
    failwith "This will never be called because it is gated"
  | Style s ->
    let elem = Webapi.Dom.HtmlElement.ofElement elem in
    match elem with
    | None -> failwith "Expected htmlelement in patchVNodesOnElemsPropertiesApplyMutate"
    | Some elem ->
      let elemStyle = Webapi.Dom.HtmlElement.style elem in
      match oldProp with
      | Style oldS -> List.fold_left2
          (fun () (ok, ov) (nk, nv) ->
            if ok = nk then
              if ov = nv then ()
              else Webapi.Dom.CssStyleDeclaration.setPropertyValue elemStyle nk nv
            else let (_ : string) = Webapi.Dom.CssStyleDeclaration.removeProperty elemStyle ok in
                 Webapi.Dom.CssStyleDeclaration.setPropertyValue elemStyle nk nv
          )
          () oldS s
      | _ -> failwith "Passed a non-Style to a new Style as a Mutations while the old Style is not actually a style!"

let rec patchVNodesOnElemsPropertiesApply = fun callbacks elem idx oldProperties newProperties ->
  match oldProperties, newProperties with
  | [], [] -> true
  | [], _::_ -> false
  | _::_, [] -> false
  | (NoProp::oldRest), (NoProp::newRest) ->
    patchVNodesOnElemsPropertiesApply callbacks elem (idx + 1) oldRest newRest
  | (RawProp (oldK, oldV) as oldProp)::oldRest, (RawProp (newK, newV) as newProp)::newRest ->
    if oldK = newK && oldV = newV then
      patchVNodesOnElemsPropertiesApply callbacks elem (idx + 1) oldRest newRest
    else begin
      patchVNodesOnElemsPropertiesApplyMutate callbacks elem idx oldProp newProp;
      patchVNodesOnElemsPropertiesApply callbacks elem (idx + 1) oldRest newRest
    end
  | (Attribute (oldNS, oldK, oldV) as oldProp)::oldRest, (Attribute (newNS, newK, newV) as newProp)::newRest ->
    if oldNS = newNS && (oldK = newK && oldV = newV) then
      patchVNodesOnElemsPropertiesApply callbacks elem (idx + 1) oldRest newRest
    else begin
      patchVNodesOnElemsPropertiesApplyMutate callbacks elem idx oldProp newProp;
      patchVNodesOnElemsPropertiesApply callbacks elem (idx + 1) oldRest newRest
    end
  | (Data (oldK, oldV) as oldProp)::oldRest, (Data (newK, newV) as newProp)::newRest ->
    if oldK = newK && oldV = newV then
      patchVNodesOnElemsPropertiesApply callbacks elem (idx + 1) oldRest newRest
    else begin
      patchVNodesOnElemsPropertiesApplyMutate callbacks elem idx oldProp newProp;
      patchVNodesOnElemsPropertiesApply callbacks elem (idx + 1) oldRest newRest
    end
  | (Event (oldName, oldHandlerType, oldCache) as _oldProp)::oldRest, (Event (newName, newHandlerType, newCache) as _newProp)::newRest ->
    let eventTarget = Webapi.Dom.Element.asEventTarget elem in
    let () = eventHandlerMutate callbacks eventTarget oldName newName oldHandlerType newHandlerType oldCache newCache in
    patchVNodesOnElemsPropertiesApply callbacks elem (idx + 1) oldRest newRest
  | (Style oldS as oldProp)::oldRest, (Style newS as newProp)::newRest ->
    if oldS = newS then
      patchVNodesOnElemsPropertiesApply callbacks elem (idx + 1) oldRest newRest
    else begin
      patchVNodesOnElemsPropertiesApplyMutate callbacks elem idx oldProp newProp;
      patchVNodesOnElemsPropertiesApply callbacks elem (idx + 1) oldRest newRest
    end
  | (oldProp::_oldRest), (newProp::_newRest) ->
    patchVNodesOnElemsPropertiesApplyRemoveAdd callbacks elem idx oldProp newProp;
    patchVNodesOnElemsPropertiesApply callbacks elem (idx + 1) oldRest newRest

let patchVNodesOnElemsProperties = fun callbacks elem oldProperties newProperties ->
  patchVNodesOnElemsPropertiesApply callbacks elem 0 oldProperties newProperties

let genEmptyProps = fun length ->
  let rec aux = fun lst x ->
    match x with
    | 0 -> lst
    | len -> aux (noProp::lst) (len - 1)
  in
  aux [] length

let mapEmptyProps = fun props ->
  List.map (fun _ -> noProp) props

let rec patchVNodesOnElemsReplaceNode = fun callbacks elem elems idx ->
  function
  | Node (newNamespace, newTagName, _, _, newProperties, newChildren) ->
    let oldChild = nodeAt idx elems in
    let newChild = createElementNsOptional newNamespace newTagName in
    let _: bool = patchVNodesOnElemsProperties callbacks newChild (mapEmptyProps newProperties) newProperties in
    let newChildNode = Webapi.Dom.Element.asNode newChild in
    let childChildren = Webapi.Dom.Node.childNodes newChildNode in
    let () = patchVNodesOnElems callbacks newChildNode childChildren 0 [] newChildren in
    let _attachedChild = Webapi.Dom.Node.insertBefore elem newChildNode oldChild in
    let _removedChild = Webapi.Dom.Node.removeChild elem oldChild
  | _ -> failwith "Node replacement should never be passed anything but a node itself"

and patchVNodesOnElemsCreateElement = fun callbacks ->
  function
  | CommentNode s -> Webapi.Dom.Document.createComment (Webapi.Dom.document) s |> Webapi.Dom.Comment.asNode
  | Text s -> Webapi.Dom.Document.createTextNode (Webapi.Dom.document) s |> Webapi.Dom.Text.asNode
  | Node (newNamespace, newTagName, _, _, newProperties, newChildren) ->
    let newChild = createElementNsOptional newNamespace newTagName in
    let _:bool = patchVNodesOnElemsProperties callbacks newChild (mapEmptyProps newProperties) newProperties in
    let newChildNode = Webapi.Dom.Element.asNode newChild in
    let childChildren = Webapi.Dom.Node.childNodes newChildNode in
    let () = patchVNodesOnElems callbacks newChildNode childChildren 0 [] newChildren in
    newChildNode
  | LazyGen (_, gen, cache) ->
    let vdom = gen () in
    let () = cache := vdom in
    patchVNodesOnElemsCreateElement callbacks vdom
  | Tagger (tagger, vdom) -> patchVNodesOnElemsCreateElement (tagger callbacks) vdom

and patchVNodesOnElemsMutateNode = fun callbacks elem elems idx oldNode newNode ->
  match oldNode, newNode with
  | Node (_, oldTagName, _, _, oldProperties, oldChildren), Node (_, newTagName, _, _, newProperties, newChildren) ->
    if oldTagName <> newTagName then
      patchVNodesOnElemsReplaceNode callbacks elem elems idx newNode
    else begin
      let child = nodeAt idx elems in
      match Webapi.Dom.Element.ofNode child with
      | None -> failwith "Expected element in patchVNodesOnElems_MutateNode"
      | Some childElement ->
        let childChildren = Webapi.Dom.Node.childNodes child in
        if patchVNodesOnElemsProperties callbacks childElement oldProperties newProperties then begin
          patchVNodesOnElems callbacks child childChildren 0 oldChildren newChildren
        end else begin
          Js.log "VDom: Failed swapping properties because the property list length changed, use `noProp` to swap properties instead, not by altering the list structure. This is a massive inefficiency until this issue is resolved.";
          patchVNodesOnElemsReplaceNode callbacks elem elems idx newNode
        end
    end
  | _ -> failwith "Non-node passed to patchVNodesOnElemsMutateNode"

and patchVNodesOnElems = fun callbacks elem elems idx oldVNodes newVNodes ->
  match oldVNodes, newVNodes with
  | Tagger (_, oldVdom)::oldRest, _ ->
    patchVNodesOnElems callbacks elem elems idx (oldVdom::oldRest) newVNodes
  | oldNode::oldRest, Tagger (newTagger, newVdom)::newRest ->
    let () = patchVNodesOnElems (newTagger callbacks) elem elems idx [oldNode] [newVdom] in
    patchVNodesOnElems callbacks elem elems (idx + 1) oldRest newRest
  | [], [] -> ()
  | [], newNode::newRest ->
    let newChild = patchVNodesOnElemsCreateElement callbacks newNode in
    let _attachedChild = Webapi.Dom.Node.appendChild elem newChild in
    patchVNodesOnElems callbacks elem elems (idx + 1) [] newRest
  | oldNode::oldRest, [] ->
    let child = nodeAt idx elems in
    let _removedChild = Webapi.Dom.Node.removeChild elem child in
    patchVNodesOnElems callbacks elem elems idx oldRest []
  | CommentNode oldS::oldRest, CommentNode newS::newRest when oldS = newS ->
    patchVNodesOnElems callbacks elem elems (idx + 1) oldRest newRest
  | Text oldText::oldRest, Text newText::newRest ->
    if oldText <> newText then begin
      let child = nodeAt idx elems in
      Webapi.Dom.Node.setNodeValue child (Js.Null.return newText)
    end;
    patchVNodesOnElems callbacks elem elems (idx + 1) oldRest newRest
  | LazyGen (oldKey, _, oldCache)::oldRest, LazyGen (newKey, newGen, newCache)::newRest ->
    if oldKey = newKey then begin
      let () = newCache := oldCache.contents in
      patchVNodesOnElems callbacks elem elems (idx + 1) oldRest newRest
    end else begin
      match oldRest, newRest with
      | LazyGen (olderKey, _, olderCache)::olderRest, LazyGen (newerKey, _, newerCache)::newerRest when olderKey = newKey && oldKey = newerKey ->
        let firstChild = nodeAt idx elems in
        let secondChild = nodeAt (idx + 1) elems in
        let _removedChild = Webapi.Dom.Node.removeChild elem secondChild in
        let _attachedChild = Webapi.Dom.Node.insertBefore elem secondChild firstChild in
        patchVNodesOnElems callbacks elem elems (idx + 2) olderRest newerRest
      | LazyGen (olderKey, _, olderCache)::olderRest, _ when (olderKey = newKey) ->
        let oldChild = nodeAt idx elems in
        let _removedChild = Webapi.Dom.Node.removeChild elem oldChild in
        let oldVdom = olderCache.contents in
        let () = newCache := oldVdom in
        patchVNodesOnElems callbacks elem elems (idx + 1) olderRest newRest
      | _, LazyGen (newerKey, _, newerCache)::newerRest when (newerKey = oldKey) ->
        let oldChild = nodeAt idx elems in
        let newVdom = newGen () in
        let () = newCache := newVdom in
        let newChild = patchVNodesOnElemsCreateElement callbacks newVdom in
        let _attachedChild = Webapi.Dom.Node.insertBefore elem newChild oldChild in
        patchVNodesOnElems callbacks elem elems (idx + 1) oldVNodes newRest
      | _ ->
        let oldVdom = oldCache.contents in
        let newVdom = newGen () in
        let () = newCache := newVdom in
        patchVNodesOnElems callbacks elem elems idx (oldVdom::oldRest) (newVdom::newRest)
    end
  | Node (_, oldTagName, oldKey, _, _, _)::oldRest, Node (_, newTagName, newKey, _, _, _)::newRest when oldKey = newKey && oldKey <> "" ->
    patchVNodesOnElems callbacks elem elems (idx + 1) oldRest newRest
  | (Node (_, oldTagName, oldKey, _, oldProperties, oldChildren) as oldNode)::oldRest, (Node (_, newTagName, newKey, _, newProperties, newChildren) as newNode)::newRest when oldKey = newKey && oldKey <> "" ->
    if oldTagName <> newTagName || oldTagName = "" || newTagName = "" then
      patchVNodesOnElemsReplaceNode callbacks elem elems idx newNode
    else begin
      let child = nodeAt idx elems in
      match Webapi.Dom.Element.ofNode child with
      | None -> failwith "Expected element in patchVNodesOnElemsMutateNode"
      | Some childElement ->
        let childChildren = Webapi.Dom.Node.childNodes child in
        if patchVNodesOnElemsProperties callbacks childElement oldProperties newProperties then begin
          patchVNodesOnElems callbacks child childChildren 0 oldChildren newChildren
        end else begin
          Js.log "VDom: Failed swapping properties because the property list length changed, use `noProp` to swap properties instead, not by altering the list structure. This is a massive inefficiency until this issue is resolved.";
          patchVNodesOnElemsReplaceNode callbacks elem elems idx newNode
        end
    end
  | (oldNode::_oldRest), (newNode::_newRest) ->
    let oldChild = nodeAt idx elems in
    let newChild = patchVNodesOnElemsCreateElement callbacks newNode in
    let _attachedChild = Webapi.Dom.Node.insertBefore elem newChild oldChild in
    let _removedChild = Webapi.Dom.Node.removeChild elem oldChild in
    patchVNodesOnElems callbacks elem elems (idx + 1) oldRest newRest
  | _ -> failwith "Incorrect number of children elements found in VDom patchVNodesOnElems"

let patchVNodesIntoElement = fun callbacks elem oldVNodes newVNodes ->
  let elems = Webapi.Dom.Node.childNodes elem in
  let () = patchVNodesOnElems callbacks elem elems 0 oldVNodes newVNodes in
  newVNodes

let patchVNodeIntoElement = fun callbacks elem oldVNode newVNode ->
  patchVNodesIntoElement callbacks elem [oldVNode] [newVNode]

let wrapCallbacksOn: type a b. (a -> b) -> systemMessage a -> systemMessage b =
  fun func -> function
    | Render -> Render
    | AddRenderMsg msg -> AddRenderMsg (func msg)
    | RemoveRenderMsg msg -> RemoveRenderMsg (func msg)

let wrapCallbacks: type a b. (a -> b) * b applicationCallbacks ref -> a applicationCallbacks ref =
  fun (func, callbacks) ->
    let enqueue msg = callbacks.enqueue (func msg) in
    let on smsg = callbacks.on (wrapCallbacksOn func smsg) in
    ref { enqueue = enqueue; on = on }

let map = fun func vdom ->
  let tagger = wrapCallbacks (func, ref(Obj.magic vdom)) in
  Tagger(Obj.magic tagger, Obj.magic vdom)
