module Clipboard

let get: string = TextCopy.ClipboardService.GetText()

let set v : unit = TextCopy.ClipboardService.SetText(v)
