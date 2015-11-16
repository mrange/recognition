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
module RecognitionSite

module Parsers =
  open MiniJson.DynamicJsonModule

  let (|ParseString|_|) (p : JsonPath) =
    if p.HasValue then Some p.AsString
    else None

module Pages =
  open Html

  module Pure =
    let inline form           e = withClass_ [|styleRef "pure-form"; styleRef "pure-form-stacked"|] e
    let inline button         e = withClass_ [|styleRef "pure-button"|] e
    let inline primaryButton  e = withClass_ [|styleRef "pure-button"; styleRef "pure-button-primary"|] e

  let inline input n t p =
    paragraph
      [|
        textLabel n t
        textField n ""
        |> withAttributes_  [|attribute "placeholder" p|]
      |]

  let page nm body =
    page
      nm
      [|stylesheet  "http://yui.yahooapis.com/pure/0.6.0/pure-min.css"|]
      [|viewport    "width=device-width, initial-scale=1"             |]
      Model.HtmlGeneratorContext.empty
      body

  let PageRecognition =
    page
      "Recognition"
      [|
        textHeader1 "Recognition"

        form
          "/PostRecognition"
          Model.UsePost
          [|
            input
              "MY_USERID"
              "Hello, my name is:"
              "Your user name"
            input
              "AWESOME_USERID"
              "I like to recognize the awesome work done by:"
              "The user name of the person you want grant recognition"
            input
              "MOTIVATION"
              "Here are the reasons I think this person is awesome:"
              "Describe why you think the person deserves recognition from his/her peers"
            submitField "SUBMIT_IT" "Send recognition" |> Pure.primaryButton
          |]
          |> Pure.form
      |]

  let PageRecognitionReceived =
    page
      "Recognition Received"
      [|
        textHeader1 "Recognition Received!"

        text "Thank you for taking time to raise awareness of awesome employees"
      |]

  let generate page           = Generator.generateHtml page

  let HtmlRecognition         = generate PageRecognition
  let HtmlRecognitionReceived = generate PageRecognitionReceived

module WebParts =
  open MiniJson.JsonModule
  open Parsers
  open Suave.Http.RequestErrors
  open Suave.Http.Redirection
  open System
  open System.Web
  open WebPartT

  let DoIndent = true

  let JsonResponse json =
    RespondWithJson DoIndent json
    |> ToWebPart

  let GetRecognition =
    RespondWithText "text/html" Pages.HtmlRecognition
    |> ToWebPart

  let GetRecognitionReceived =
    RespondWithText "text/html" Pages.HtmlRecognitionReceived
    |> ToWebPart

  let PostRecognition =
    Request
      >>= fun _ -> RedirectWith (FOUND "/RecognitionReceived")
    |> ToWebPart

open Suave.Http
open Suave.Http.Applicatives
open Suave.Http.Redirection
open Suave.Http.RequestErrors
open Suave.Types

let App : WebPart =
  choose
    [
      GET >>= path      "/Recognition"              >>= WebParts.GetRecognition
      GET >>= path      "/RecognitionReceived"      >>= WebParts.GetRecognitionReceived
      POST>>= path      "/PostRecognition"          >>= WebParts.PostRecognition
      MOVED_PERMANENTLY "/Recognition"
    ]
