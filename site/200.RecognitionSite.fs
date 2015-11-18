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
  open Html.Common
  open Html.Model

  module Pure =
    let input         = withClass_ [|styleRef "pure-input-1"|]
    let form          = withClass_ [|styleRef "pure-form"; styleRef "pure-form-stacked"|]
    let button        = withClass_ [|styleRef "pure-button"|]
    let primaryButton = withClass_ [|styleRef "pure-button"; styleRef "pure-button-primary"|]

  module Attribute =
    let inline placeholder p =
      withAttributes_ [|attribute "placeholder" p|]

    let inline userInput id p =
      withAttributes_
        [|
          Id                  id
          attribute           "placeholder" p
          unencodedAttribute  "onblur"      (sprintf "onGetUserName('%s')" id)
        |]

  module Scripts =
    let Main = """
    function onGetUserName(inputId) {
      var query = document.getElementById(inputId).value;
      if (query.length > 2) {
        var req = new XMLHttpRequest();
        req.onreadystatechange = function() {
          if (req.readyState == 4 && req.status == 200) {
            var json      = JSON.parse(req.responseText);
            var response  = json.response;
            if (response != undefined) {
              document.getElementById(inputId).value = response;
            }
          }
        };
        req.open("GET", "/GetUserName/" + encodeURIComponent(query), true);
        req.send();
      }
    }
"""
  let inline userInput id n t p =
    inlined
      [|
        textLabel n t
        textField n ""
        |> Attribute.userInput id p
        |> Pure.input
      |]

  let inline textarea n t p =
    inlined
      [|
        textLabel n t
        tag (CustomTag "textarea") (leaf2 (Name n) (attribute "rows" "5")) [||]
        |> Attribute.placeholder p
        |> Pure.input
      |]

  let page nm body =
    page
      nm
      [|stylesheet  "http://yui.yahooapis.com/pure/0.6.0/pure-min.css"|]
      [|viewport    "width=device-width, initial-scale=1"             |]
      [|script      Scripts.Main                                      |]
      HtmlGeneratorContext.empty
      body

  let PageRecognition =
    page
      "Recognition"
      [|
        textHeader1 "Recognition"

        form
          "/PostRecognition"
          UsePost
          [|
            userInput
              "my_userid"
              "MY_USERID"
              "Hello, my name is:"
              "Your user name"
            userInput
              "awesome_userid"
              "AWESOME_USERID"
              "I like to recognize the awesome work done by:"
              "The user name of the person you want grant recognition"
            textarea
              "MOTIVATION"
              "Here are the reasons I think this person is awesome:"
              "Describe why you think the person deserves recognition from his/her peers"
            submitField "SUBMIT_IT" "Send recognition" |> Pure.primaryButton
          |]
          |> Pure.form

        text "Recognition of coworkers will be shared with relevant line managers"
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
  open Html
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

  let HtmlResponse html =
    RespondWithText "text/html" html
    |> ToWebPart

  let trim (s : string) = s.Trim ()
  let lowerCase (s : string) = s.ToLowerInvariant ()

  let users =
    let lc = Array.map lowerCase
    [|
      "marten.range"  , [|"Mårten Rånge" ; "Mårten"; "Rånge"  ; "marten.range"  |] |> lc
      "stefan.lekebo" , [|"Stefan Lekebo"; "Stefan"; "Lekebo" ; "stefan.lekebo" |] |> lc
      "rikard.kaer"   , [|"Rikard Käer"  ; "Rikard"; "Käer"   ; "rikard.käer"   |] |> lc
    |]

  let GetUserName query       =
    let dq  = query |> urlDecode |> trim
    let udq = dq |> lowerCase

    let aliasExists (aliases : string []) =
      aliases
      |> Array.exists (fun alias -> alias.StartsWith udq)

    let user =
      users
      |> Array.tryPick (fun (user, aliases) -> if aliasExists aliases then Some user else None)

    let json =
      match user with
      | Some u  -> JsonObject [|"query", JsonString dq; "response", JsonString u|]
      | None    -> JsonObject [|"query", JsonString dq|]

    JsonResponse json
  let GetRecognition          = HtmlResponse Pages.HtmlRecognition
  let GetRecognitionReceived  = HtmlResponse Pages.HtmlRecognitionReceived

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
      GET >>= pathScan  "/GetUserName/%s"               WebParts.GetUserName
      MOVED_PERMANENTLY "/Recognition"
    ]
