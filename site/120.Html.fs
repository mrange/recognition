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

open FSharp.Reflection

open System
open System.Net
open System.Text

type Link       = Link      of string*string
type Meta       = Meta      of string*string
type Class      = Class     of string
type Attribute  = Attribute of string*string

type InputKind =
  | TextBox
  | RadioButton
  | SubmitButton

type FormMethod =
  | UseGet
  | UsePost

type HeaderSize =
  | Header1
  | Header2
  | Header3

[<NoEquality>]
[<NoComparison>]
type Element =
  | Text              of string
  | Image             of string*string
  | Header            of HeaderSize*(Element [])
  | LineBreak
  | Form              of string*FormMethod*Element []
  | FieldSet          of string*Element []
  | InputField        of InputKind*string*string
  | Paragraph         of Element []
  | Mark              of Element []
  | ExternalLink      of string*(Element [])
  | OrderedList       of (Element []) []
  | UnorderedList     of (Element []) []
  | WithClasses       of (Class [])*(Element [])
  | WithAttributes    of (Attribute [])*(Element [])
  | Generated         of (int -> Class [] -> Attribute [] -> (int -> string -> unit) -> unit)

[<NoEquality>]
[<NoComparison>]
type Page =
  {
    Title           : string
    Links           : Link []
    Metas           : Meta []
    Body            : Element []
  }

let inline text txt                           = Text txt
let inline image src alt                      = Image (src,alt)
let inline header1 elements                   = Header (Header1, elements)
let inline header2 elements                   = Header (Header2, elements)
let inline header3 elements                   = Header (Header3, elements)
let lineBreak                                 = LineBreak
let inline form action meth elements          = Form (action, meth, elements)
let inline fieldSet elements                  = FieldSet ("", elements)
let inline textField name value               = InputField (TextBox,name, value)
let inline radioField name value              = InputField (RadioButton,name, value)
let inline submitField name value             = InputField (SubmitButton,name, value)
let inline textHeader1 text                   = header1 [|Text text|]
let inline textHeader2 text                   = header2 [|Text text|]
let inline textHeader3 text                   = header3 [|Text text|]
let inline paragraph elements                 = Paragraph elements
let inline mark elements                      = Mark elements
let inline textMark text                      = Mark [|Text text|]
let inline externalLink href elements         = ExternalLink (href, elements)
let inline textLink href description          = ExternalLink (href, [|Text description|])
let inline imageLink href src alt             = ExternalLink (href, [|Image (src, alt)|])
let inline orderedList items                  = OrderedList items
let inline unorderedList items                = UnorderedList items
let inline generated g                        = Generated g
let inline withClasses classes elements       = WithClasses (classes, elements)
let inline withClasses_ classes element       = WithClasses (classes, [|element|])
let inline withAttributes attributes elements = WithAttributes (attributes, elements)
let inline withAttributes_ attributes element = WithAttributes (attributes, [|element|])

let inline link rel href                      = Link (rel,href)
let inline stylesheet href                    = Link ("stylesheet",href)
let inline meta name content                  = Meta (name,content)
let inline viewport content                   = Meta ("viewport",content)
let inline attribute key value                = Attribute (key, value)
let inline page title links metas body : Page = { Title = title; Links = links; Metas = metas; Body = body; }

let generateHtml (page : Page) : string =
  let inline htmlEncode (s : string) : string = WebUtility.HtmlEncode s
  let inline urlEncode  (s : string) : string = s

  let html  = StringBuilder 64

  let empty = [||]

  let inline emptyStr s         = String.IsNullOrEmpty s
  let inline nonEmptyStr s      = not (emptyStr s)
  let inline ch   (c : char)    = ignore <| html.Append c
  let inline str  (s : string)  = ignore <| html.Append s
  let inline indent (i : int)   = ignore <| html.Append (' ', i)
  let inline newl ()            = ignore <| html.AppendLine ()

  let inline append (i : int) (l : string) : unit =
    indent i
    ignore <| html.AppendLine l

  let inline headerTypeTag tp =
    match tp with
    | Header1       -> "h1"
    | Header2       -> "h2"
    | Header3       -> "h3"

  let inline inputKind ik =
    match ik with
    | TextBox       -> "text"
    | RadioButton   -> "radio"
    | SubmitButton  -> "submit"

  let inline formatMethod fm =
    match fm with
    | UseGet        -> "GET"
    | UsePost       -> "POST"

  let renderTag
    (closed     : bool              )
    (i          : int               )
    (name       : string            )
    (classes    : Class []          )
    (custom     : Attribute []      )
    (attributes : (string*string) [])=
    indent i
    ch '<'
    str name
    if classes.Length > 0 then
      str @" class="""
      let mutable first = true
      for (Class cls) in classes do
        if nonEmptyStr cls then
          if not first then
            ch ';'
            first <- false
          str cls
      ch '"'
    for (Attribute (key,value)) in custom do
      if nonEmptyStr key && nonEmptyStr value then
        ch ' '
        str key
        str @"="""
        str value
        ch '"'
    for key,value in attributes do
      if nonEmptyStr key && nonEmptyStr value then
        ch ' '
        str key
        str @"="""
        str value
        ch '"'
    if closed then
      ch '/'
    ch '>'
    newl ()

  let renderEndTag
    (i          : int               )
    (name       : string            )=
    indent i
    ch '<'
    ch '/'
    str name
    ch '>'
    newl ()

  let inline renderClosedTag  i name classes custom attributes = renderTag true  i name classes custom attributes
  let inline renderStartTag   i name classes custom attributes = renderTag false i name classes custom attributes

  let rec renderElements i classes attributes es =
    let inline container tag attr es =
      renderStartTag i tag classes attributes attr
      renderElements (i + 2) empty empty es
      renderEndTag i tag
    let inline listItem tag es =
      // TODO: How to handle list item attributes
      renderStartTag (i + 2) tag empty empty empty
      renderElements (i + 4) empty empty es
      renderEndTag (i + 2) tag
    for e in es do
      match e with
      | Text text ->
        append i (htmlEncode text)
      | Image (src, alt) ->
        renderClosedTag i "img" classes attributes [|"src", urlEncode src; "alt", htmlEncode alt|]
      | Header (tp, inner) ->
        container (headerTypeTag tp) empty inner
      | LineBreak ->
        renderClosedTag i "br" classes attributes empty
      | Form (action, meth, inner) ->
        container "form" [|"action", action; "method", formatMethod meth|] inner
      | FieldSet (_, inner) ->
        container "fieldset" empty inner
      | InputField (kind, name, value) ->
        renderClosedTag i "input" classes attributes [|"type", inputKind kind; "name", name; "value", value|]
      | Paragraph inner ->
        container "p" empty inner
      | Mark inner ->
        container "mark" empty inner
      | ExternalLink (href, es) ->
        container "a" [|"href", urlEncode href|] es
      | OrderedList items ->
        renderStartTag i "ol" classes attributes empty
        for item in items do
          listItem "li" item
        renderEndTag i "ol"
      | UnorderedList items ->
        renderStartTag i "ul" classes attributes empty
        for item in items do
          listItem "li" item
        renderEndTag i "ul"
      | WithClasses (newClasses, inner) -> renderElements i newClasses attributes inner
      | WithAttributes (newAttributes, inner) -> renderElements i classes newAttributes inner
      | Generated generator -> generator i classes attributes append

  append 0 "<html>"
  append 2 "<head>"
  for (Link (rel, href)) in page.Links do
    append 4 (sprintf """<link rel="%s" href="%s"/>""" rel (urlEncode href))
  for (Meta (name, content)) in page.Metas do
    append 4 (sprintf """<meta name="%s" content="%s"/>""" name content)
  append 4 (sprintf "<title>%s</title>" (htmlEncode page.Title))
  append 2 "</head>"
  append 2 "<body>"
  renderElements 2 empty empty page.Body
  append 2 "</body>"
  append 0 "</html>"

  let result = html.ToString ()
//  printfn "%s" result
  result


