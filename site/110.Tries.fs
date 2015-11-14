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
module Tries

open System.Text

type CharTrie<'T> =                                             // ' Buggy F# sublime plugin...
  | CharTrie0 of 'T option                                      // ' Buggy F# sublime plugin...
  | CharTrie1 of 'T option*char*CharTrie<'T>
  | CharTrie2 of 'T option*char*CharTrie<'T>*char*CharTrie<'T>  // ' Buggy F# sublime plugin...
  | CharTrieN of 'T option*Map<char, CharTrie<'T>>

  member x.CheckInvariant () =
    match x with
    | CharTrie0 _ ->
      true
    | CharTrie1 (_,_,nt0) ->
      nt0.CheckInvariant ()
    | CharTrie2 (_, c0, nt0, c1, nt1) ->
      c0 <> c1
      && nt0.CheckInvariant ()
      && nt1.CheckInvariant ()
    | CharTrieN (_, m) ->
      // Map guarantees all keys unique
      m
      |> Map.forall (fun _ nt -> nt.CheckInvariant ())

[<GeneralizableValue>]
let Empty     = CharTrie0 None

let inline IsEmpty ct =
  match ct with
  | CharTrie0 None ->
    true
  | _ ->
    false

let inline (|MatchEmpty|MatchNonEmpty|) ct =
  match ct with
  | CharTrie0 None ->
    MatchEmpty
  | _ ->
    MatchNonEmpty

module Details =
  let rec MatchImpl (p : string) bi bm i l t =
    let inline found  ov  =
      match ov with
      | Some _ -> i, ov
      | _ -> bi, bm
    let inline next ov nt =
      match ov with
      | Some _ ->
        MatchImpl p i ov (i + 1) l nt
      | _ ->
        MatchImpl p bi bm (i + 1) l nt

    if i < l then
      let c = p.[i]
      match t with
      | CharTrie0 ov ->
        found ov
      | CharTrie1 (ov,c0,nt0) when c = c0 ->
        next ov nt0
      | CharTrie1 (ov,_,_) ->
        found ov
      | CharTrie2 (ov,c0,nt0,_,_) when c = c0 ->
        next ov nt0
      | CharTrie2 (ov,_,_,c1,nt1) when c = c1 ->
        next ov nt1
      | CharTrie2 (ov,_,_,_,_) ->
        found ov
      | CharTrieN (ov, m) ->
        match Map.tryFind c m with
        | None    ->
          found ov
        | Some nt ->
          next ov nt
    else
      match t with
      | CharTrie0 ov
      | CharTrie1 (ov,_,_)
      | CharTrie2 (ov,_,_,_,_)
      | CharTrieN (ov, _) ->
        found ov

  let rec UpdateImpl (k : string) ov i t =
    let inline next t = UpdateImpl k ov (i + 1) t
    if i < k.Length then
      let c = k.[i]
      match t with
      | CharTrie0 pov ->
        CharTrie1 (pov, c, next Empty)
      | CharTrie1 (pov, c0, nt0) when c = c0 ->
        CharTrie1 (pov, c0, next nt0)
      | CharTrie1 (pov, c0, nt0) ->
        CharTrie2 (pov, c0, nt0, c, next Empty)
      | CharTrie2 (pov, c0, nt0, c1, nt1) when c = c0 ->
        CharTrie2 (pov, c0, next nt0, c1, nt1)
      | CharTrie2 (pov, c0, nt0, c1, nt1) when c = c1 ->
        CharTrie2 (pov, c0, nt0, c1, next nt1)
      | CharTrie2 (pov, c0, nt0, c1, nt1) ->
        CharTrieN (pov, Map.empty |> Map.add c0 nt0 |> Map.add c1 nt1 |> Map.add c (next Empty))
      | CharTrieN (pov, m) ->
        match Map.tryFind c m with
        | None    ->
          CharTrieN (pov, m |> Map.add c (next Empty))
        | Some nt ->
          CharTrieN (pov, m |> Map.remove c |> Map.add c (next nt))
    else
      match t with
      | CharTrie0 _ ->
        CharTrie0 ov
      | CharTrie1 (_, c0, nt0) ->
        CharTrie1 (ov, c0, nt0)
      | CharTrie2 (_, c0, nt0, c1, nt1) ->
        CharTrie2 (ov, c0, nt0, c1, nt1)
      | CharTrieN (_, m) ->
        CharTrieN (ov, m)

  let rec TrimImpl t =
    match t with
    | CharTrie0 _ -> t
    | CharTrie1 (pov, c0, nt0) ->
      match TrimImpl nt0 with
      | MatchEmpty ->
        CharTrie0 pov
      | tnt0 ->
        CharTrie1 (pov, c0, tnt0)
    | CharTrie2 (pov, c0, nt0, c1, nt1) ->
      match TrimImpl nt0, TrimImpl nt1 with
      | MatchEmpty, MatchEmpty ->
        CharTrie0 pov
      | tnt0, MatchEmpty ->
        CharTrie1 (pov, c0, tnt0)
      | MatchEmpty, tnt1 ->
        CharTrie1 (pov, c1, tnt1)
      | tnt0, tnt1 ->
        CharTrie2 (pov, c0, tnt0, c1, tnt1)
    | CharTrieN (pov, m) ->
      let nm =
        m
        |> Map.map (fun _ nt -> TrimImpl nt)
        |> Map.filter (fun _ nt -> not (IsEmpty nt))
      match nm with
      | _ when nm.Count = 0 ->
        CharTrie0 pov
      | _ when nm.Count = 1 ->
        let kv0 = nm |> Seq.head
        CharTrie1 (pov, kv0.Key, kv0.Value)
      | _ when nm.Count = 2 ->
        let [|kv0; kv1|] = nm |> Seq.take 2 |> Seq.toArray
        CharTrie2 (pov, kv0.Key, kv0.Value, kv1.Key, kv1.Value)
      | _ ->
        CharTrieN (pov, nm)

  let rec AllImpl (sb : StringBuilder) (ra : ResizeArray<string*'T>) t =
    let inline emit ov =
      match ov with
      | Some v ->
        ignore <| ra.Add (sb.ToString (), v)
      | _ ->
        ()

    let inline next (c : char) nt =
      ignore <| sb.Append c
      AllImpl sb ra nt
      ignore <| sb.Remove (sb.Length - 1, 1)

    match t with
    | CharTrie0 ov ->
      emit ov
    | CharTrie1 (ov, c0, nt0) ->
      emit ov
      next c0 nt0
    | CharTrie2 (ov, c0, nt0, c1, nt1) ->
      emit ov
      next c0 nt0
      next c1 nt1
    | CharTrieN (ov, m) ->
      emit ov
      m |> Map.iter next

let MatchAt (p : string) (at : int) (ct : CharTrie<'T>) : int*'T option =
  Details.MatchImpl p -1 None at p.Length ct

let Match (p : string) (ct : CharTrie<'T>) : int*'T option =
  Details.MatchImpl p -1 None 0 p.Length ct

let Update (k : string) (ov : 'T option) (ct : CharTrie<'T>) : CharTrie<'T> =
  Details.UpdateImpl k ov 0 ct

let Trim (ct : CharTrie<'T>) : CharTrie<'T> =
  Details.TrimImpl ct

let All (ct : CharTrie<'T>) : (string*'T) [] =
  let sb = StringBuilder 16
  let ra = ResizeArray<string*'T> ()  // ' Buggy F# sublime plugin...
  Details.AllImpl sb ra ct
  ra.ToArray ()
