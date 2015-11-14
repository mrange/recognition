// ----------------------------------------------------------------------------------------------
// Copyright 2015 Mårten Rånge
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
// ----------------------------------------------------------------------------------------------
module Html

open System.Net
open System.Text

type Element =
  | Text              of string
  | Image             of string*string
  | Header            of int*(Element list)
  | Paragraph         of Element list
  | Mark              of Element list
  | ExternalLink      of string*(Element list)
  | OrderedList       of (Element list) list
  | UnorderedList     of (Element list) list
  | Generated         of (int -> (int -> string -> unit) -> unit)

type Page =
  {
    Title           : string
    Body            : Element list
  }

let inline text txt                 = Text txt
let inline image src alt            = Image (src,alt)
let inline header1 elements         = Header (1, elements)
let inline header2 elements         = Header (2, elements)
let inline header3 elements         = Header (3, elements)
let inline textHeader1 text         = header1 [Text text]
let inline textHeader2 text         = header2 [Text text]
let inline textHeader3 text         = header3 [Text text]
let inline paragraph elements       = Paragraph elements
let inline mark elements            = Mark elements
let inline textMark text            = Mark [Text text]
let inline externalLink url eles    = ExternalLink (url, eles)
let inline textLink url desc        = ExternalLink (url, [Text desc])
let inline imageLink url src alt    = ExternalLink (url, [Image (src, alt)])
let inline orderedList items        = OrderedList items
let inline unorderedList items      = UnorderedList items
let inline generated g              = Generated g
let inline page title body : Page   = { Title = title; Body = body; }

let generateHtml (page : Page) : string =
  let inline htmlEncode (s : string) : string = WebUtility.HtmlEncode s
  let inline urlEncode  (s : string) : string = s

  let sb = StringBuilder ()

  let append (i : int) (l : string) : unit =
    ignore <| sb.Append (' ', i)
    ignore <| sb.AppendLine l

  let rec impl i es =
    for e in es do
      match e with
      | Text text -> append i (htmlEncode text)
      | Image (src, alt) -> append i (sprintf """<img src="%s" alt="%s"/>""" (urlEncode src) (htmlEncode alt))
      | Header (level, inner) -> container "<h3>" "</h3>" i inner
      | Paragraph inner -> container "<p>" "</p>" i inner
      | Mark inner -> container "<mark>" "</mark>" i inner
      | ExternalLink (href, es) ->
        container (sprintf """<a href="%s" target="_blank">""" (urlEncode href)) "</a>" i es
      | OrderedList items ->
        append i "<ol>"
        for item in items do
          container "<li>" "</li>" (i + 2) item
        append i "</ol>"
      | UnorderedList items ->
        append i "<ul>"
        for item in items do
          container "<li>" "</li>" (i + 2) item
        append i "</ul>"
      | Generated generator -> generator i append

  and container startTag endTag i es =
    append i startTag
    impl (i + 2) es
    append i endTag

  append 0 "<html>"
  append 2 (sprintf "<title>%s</title>" (htmlEncode page.Title))
  append 2 "<body>"
  impl 2 page.Body
  append 2 "</body>"
  append 1 "</html>"

  sb.ToString ()

