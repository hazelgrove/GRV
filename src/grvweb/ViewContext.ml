open OptionUtil.Syntax
module Vdom = Virtual_dom.Vdom

type t = {
  model : Model.t;
  inject : Action.t -> Vdom.Event.t;
  editor_id : Editor.id;
  tabindexes : int Id.Map.t;
}

let editor_opt (ctx : t) : Editor.t option =
  Id.Map.find_opt ctx.editor_id ctx.model.editors

let cursor_sort (ctx : t) : GroveLang.Sort.t option =
  let+ editor = editor_opt ctx in
  ZGrove.cursor_sort editor.zgrove
