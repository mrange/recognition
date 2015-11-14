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
module WebPartT

[<NoEquality>]
[<NoComparison>]
type WebPartTResult<'T> =
  | SuccessfullyHandled of 'T*Suave.Types.HttpContext
  | FailedWhileHandling of Suave.Types.HttpContext
  | NotHandled

type WebPartT<'T> = Suave.Types.HttpContext -> Async<WebPartTResult<'T>>

module Monad =
  let inline Success ctx v =
    async.Return (SuccessfullyHandled (v,ctx))

  let inline Failure ctx =
    async.Return (FailedWhileHandling ctx)

  let inline Ignore () =
    async.Return NotHandled

  let Delay ft : WebPartT<'T> =
    fun ctx ->
      let t = ft ()
      t ctx

  let Return v : WebPartT<'T> =
    fun ctx ->
      Success ctx v

  let ReturnFrom t : WebPartT<'T> =
    t

  let Bind (t : WebPartT<'T>) (fu : 'T -> WebPartT<'U>) : WebPartT<'U> =
    fun ctx ->
      async {
        let! rtv = t ctx
        match rtv with
        | NotHandled                    -> return NotHandled
        | FailedWhileHandling tctx      -> return FailedWhileHandling tctx
        | SuccessfullyHandled (tv,tctx) ->
          let u = fu tv
          return! u tctx
      }

  type WebPartTBuilder()        =
    member inline x.Bind (t,fu) = Bind t fu
    member inline x.Delay ft    = Delay ft
    member inline x.Return v    = Return v
    member inline x.ReturnFrom t= ReturnFrom t

let inline ( >>= ) t fu = Monad.Bind t fu

let wpt = Monad.WebPartTBuilder ()

let Context : WebPartT<Suave.Types.HttpContext> =
  fun ctx ->
    Monad.Success ctx ctx

let Request : WebPartT<Suave.Types.HttpRequest> =
  fun ctx ->
    Monad.Success ctx ctx.request

let FromAsync (a : Async<'T>) : WebPartT<'T> =
  fun ctx ->
    async {
      let! v = a
      return SuccessfullyHandled (v, ctx)
    }

open Suave.Http.Successful
open Suave.Http.RequestErrors

let FromWebPart (wp : Suave.Types.WebPart) : WebPartT<unit> =
  fun ctx ->
    async {
      let! owp = wp ctx
      match owp with
      | None        -> return NotHandled
      | Some wpctx  -> return SuccessfullyHandled ((), wpctx)
    }

let ToWebPart (t : WebPartT<unit>) : Suave.Types.WebPart =
  fun ctx ->
    async {
      let! otv = t ctx
      match otv with
      | NotHandled                    -> return None
      | FailedWhileHandling tctx
      | SuccessfullyHandled (_, tctx) -> return Some tctx
    }

let RespondWithText (mimeType : string) (s : string) : WebPartT<unit> =
  fun ctx ->
    async {
      let! ook = OK s ctx
      match ook with
      | None        -> return NotHandled
      | Some okctx  ->
        let! omt = Suave.Http.Writers.setMimeType mimeType okctx
        match omt with
        | None        -> return NotHandled
        | Some mtctx  -> return SuccessfullyHandled ((), mtctx)
    }

let FailWith (failure : Suave.Types.WebPart) : WebPartT<_> =
  fun ctx ->
    async {
      let! ofail = failure ctx
      match ofail with
      | None      -> return NotHandled
      | Some fctx -> return FailedWhileHandling fctx
    }

open MiniJson.JsonModule
open MiniJson.DynamicJsonModule

let ReceiveJson (fullErrorInfo : bool) : WebPartT<Json*JsonPath> =
  Request
  >>= fun req ->
    match req.form with
    | (content, _)::_ ->
      let parseResult = parse fullErrorInfo content
      match parseResult with
      | Success json      -> Monad.Return (json,json.Query)
      | Failure (msg, _)  -> FailWith (BAD_REQUEST msg)
    | _ -> FailWith (BAD_REQUEST "Missing JSON content")

let RespondWithJson (doIndent : bool) (json : Json) : WebPartT<unit> =
  RespondWithText "application/json" (json.ToString doIndent)

