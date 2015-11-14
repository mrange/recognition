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
module MyService

module Compute =
  open System
  open System.Text

  let Fibonacci n =
    // Important: impl is tail-recursive
    //  This allows F# to unwind impl into an effective for loop
    let rec impl i f s=
      if i < n then
        let t = f + s
        impl (i + 1L) s t
      else
        s
    impl 0L 1L 1L

  let Sort3 a b c =
    let inline f a b c = if b <= c then a,b,c else a,c,b

    if a <= b && a <= c then f a b c
    elif b <= a && b <= c then f b a c
    else f c a b

  let (|Scalene|Isosceles|Equilateral|Error|) (a,b,c) =
    let a,b,c = Sort3 a b c
    if c < a + b then
      match a = b, b = c with
      | true  , true  -> Equilateral  // a = b && b = c => a = c
      | false , false -> Scalene      // a,b,c sorted, a <> b && b <> c => a <> c
      | _     , _     -> Isosceles    // All other cases => Isosceles
    else
      // c is greater or equal to sum of a b ==> no triangle
      Error

  let ReverseWords (input : string) =
    let sb = StringBuilder input.Length

    // Important: impl is tail-recursive
    //  This allows F# to unwind impl into an effective for loop
    let rec impl b i =
      let inline reverse () = for ii = (i - 1) downto b do ignore <| sb.Append input.[ii]
      if i < input.Length then
        if not (Char.IsWhiteSpace input.[i]) then
          impl b (i + 1)
        else
          reverse ()
          ignore <| sb.Append input.[i]
          impl (i + 1) (i + 1)
      else
        reverse ()

    impl 0 0

    sb.ToString ()

module Parsers =
  open MiniJson.DynamicJsonModule

  let (|ParseString|_|) (p : JsonPath) =
    if p.HasValue then Some p.AsString
    else None

module Pages =
  open Html

  let PageServiceList =
    page "Service list"
      [
        textHeader1 "Service list"

        text "List of available services:"

        unorderedList
          [
            [textLink "/WhatIsYourToken"            "WhatIsYourToken" ]
            [textLink "/FibonacciNumber/3"          "FibonacciNumber" ]
            [textLink "/WhatShapeIsThis/3/3/10"     "WhatShapeIsThis" ]
            [textLink "/ReverseWords/Hello there!"  "ReverseWords"    ]
          ]
      ]

  let HtmlServiceList = generateHtml PageServiceList


module WebParts =
  open Compute
  open MiniJson.JsonModule
  open Parsers
  open Suave.Http.RequestErrors
  open System
  open System.Web
  open WebPartT

  let DoIndent = true

  let TokenJson =
    JsonObject
      [|
        "token"   , JsonString "fc893331-82b8-41d4-b5cb-4582ad813cc9"
      |]

  let JsonResponse json =
    RespondWithJson DoIndent json
    |> ToWebPart

  let GetServiceList =
    RespondWithText "text/html" Pages.HtmlServiceList
    |> ToWebPart

  let GetToken =
    TokenJson
    |> JsonResponse

  let GetFibonacciNumber n =
    let f = Fibonacci n
    JsonObject
      [|
        "input"     , JsonString (string n)
        "fibonacci" , JsonString (string f)
      |]
    |> JsonResponse

  let GetShape (a,b,c) =
    let s =
      match a,b,c with
      | Scalene     -> "Scalene"
      | Isosceles   -> "Isosceles"
      | Equilateral -> "Equilateral"
      | Error       -> "Error"
    JsonObject
      [|
        "a"         , JsonString (string a)
        "b"         , JsonString (string b)
        "c"         , JsonString (string c)
        "shape"     , JsonString s
      |]
    |> JsonResponse

  let GetReversedWords i =
    let i = i |> HttpUtility.UrlDecode
    let r = i |> ReverseWords
    JsonObject
      [|
        "input"     , JsonString i
        "reversed"  , JsonString r
      |]
    |> JsonResponse

  let PostReversedWords =
    wpt {
      let! _,q = ReceiveJson true
      match q?input with
      | ParseString i ->
        let r = i |> ReverseWords
        let json =
          JsonObject
            [|
              "input"     , JsonString i
              "reversed"  , JsonString r
            |]
        return! RespondWithJson DoIndent json
      | _ -> return! FailWith (BAD_REQUEST "Invalid input")
    } |> ToWebPart

open Suave.Http
open Suave.Http.Applicatives
open Suave.Http.RequestErrors
open Suave.Types

let App : WebPart =
  choose
    [
      GET >>= path      "/WhatIsYourToken"          >>= WebParts.GetToken
      GET >>= pathScan  "/FibonacciNumber/%u"           WebParts.GetFibonacciNumber
      GET >>= pathScan  "/WhatShapeIsThis/%u/%u/%u"     WebParts.GetShape
      GET >>= pathScan  "/ReverseWords/%s"              WebParts.GetReversedWords
      POST>>= path      "/ReverseWords"             >>= WebParts.PostReversedWords
      WebParts.GetServiceList // Fallback
    ]
