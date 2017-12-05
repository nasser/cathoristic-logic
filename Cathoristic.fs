module Cathoristic

open System
open System.Collections.Generic
open System.Linq
open FSharp.Linq
open FSharp.Core.Option
open FSharp.Core

type Symbol = String
type Prop = Top | Bot | Conj of Prop * Prop | Trans of Symbol * Prop | Bang of Set<Symbol>
type State = int
type Transition = (Symbol * (State * State) list) list
type Label = LStar | LBang of Set<Symbol>
type LTS = (State list * Transition * (State * Label) list)
type PointedLTS = (LTS * State)
type Model = PointedLTS option

/// haskell helpers ///

let union l1 l2 =
    List.append (List.distinct l1) (List.distinct l2)

let lookup i l =
    let v = List.tryFind (fun (x,_) -> x = i) l
    if isSome v then
        Some (snd <| get v)
    else
       None

/// semantics ///

let rec satsArrow ((s,r,v), w) a t =
    let satsT (x,y) = w = x && sats ((s,r,v),y) t
    let findArrow (a',rel) = a = a' && List.exists satsT rel
    List.exists findArrow r

and satsBang (((_, _, ls), w):PointedLTS) ss =
    match lookup w ls with
    | None -> false
    | Some l ->
        let satsLabel x = match x with
                          | LStar-> false
                          | (LBang ss') -> Set.isSubset ss' ss
        satsLabel l

and sats (m:PointedLTS) p =
    match (m, p) with
    | (_, Top) -> true
    | (_, Bot) -> false
    | (m, Conj (p, q)) -> sats m p && sats m q
    | (m, Trans(s, p)) -> satsArrow m s p
    | (m, Bang(ss)) -> satsBang m ss

let satisfies m p =
    match m with
    | None -> true
    | Some x -> sats x p

/// simpl ///

let maxState (((states,_, _), _):PointedLTS) : int =
    List.max states

let getLabel (((_,_,labels),w):PointedLTS) : Label =
    get (lookup w labels)

let replaceState ((states, trans, labels):LTS) ((x, y):(State * State)) : LTS =
    let f s = if s = x then y else s
    let f2 (s,s') = match (s=x, s'=x) with
                    | (true, true) -> (y,y)
                    | (true, false) -> (y, s')
                    | (false, true) -> (s, y)
                    | (false, false) -> (s, s')
    let f3 (s,l) = if s = x then (y,l) else (s,l)
    let states' = List.map f states
    let trans' = List.map (fun (s,rel) -> (s, List.map f2 rel)) trans
    let labels' = List.map f3 labels
    (states', trans', labels')

let mergeTransitions (tr1:Transition) (tr2:Transition) : Transition =
    let symbols = union (List.map fst tr1) (List.map fst tr2)
    let f s = match (lookup s tr1),(lookup s tr2) with
              | Some r,None -> (s, r)
              | None,Some r -> (s, r)
              | Some r, Some r' -> (s, r')
    List.map f symbols

let getIdentifications ((_, t, _):LTS) ((_, t', _):LTS) =
    let findMatch rel (x,y) = match lookup x rel with
                              | Some z -> if y = z then [] else [(y, z)]
                              | None -> []
    let findMatches (s, rel) = match lookup s t with
                               | None -> []
                               | Some rel' -> List.map (findMatch rel') rel
    List.map findMatches t' |> List.concat |> List.concat

let moreSpecificLabel a b = 
  match a,b with
  | LStar, x -> x
  | x, LStar -> x
  | (LBang ss), (LBang ss') -> LBang <| Set.intersect ss ss'

let mergeLabels (l1:(State*Label) list) (l2:(State*Label) list) = 
    let states = union (List.map fst l1) (List.map fst l2)
    let moreSpecificL w = match (lookup w l1, lookup w l2) with
                          | (None, Some l) -> l
                          | (Some l, None) -> l
                          | (Some l, Some l') -> moreSpecificLabel l l'
    let f s = (s, moreSpecificL s)
    List.map f states

let addLTSTo ((s, t, l):LTS) ((s', t', l'):LTS) = 
    let s'' = List.distinct <| List.append s s'
    let t'' = mergeTransitions t t'
    let l'' = mergeLabels l l'
    (s'', t'', l'')

let applyIdentifications (ids:(State*State) list) (lts:LTS) =
    List.fold replaceState lts ids

let fromState w (_, rel) =
    List.exists (fun (x,_) -> x = w) rel

let outTransitions (((_, t, _), w):PointedLTS) =
    let f (s, rel) = (s, get (lookup w rel))
    let succs = List.filter (fromState w) t
    List.sort <| (List.map f succs)

let out (m:PointedLTS) = 
    Set.ofList (List.map fst (outTransitions m))

let consistentIdentification (lts:LTS) (lts':LTS) ((w',w):(State*State)) =
    let pm = (lts, w)
    let pm' = (lts', w')
    match (getLabel pm, getLabel pm') with
    | (LStar, LStar) -> true
    | (LStar, LBang ss') -> Set.isSubset (out pm) ss'
    | (LBang ss, LStar) -> Set.isSubset (out pm') ss
    | (LBang ss, LBang ss') -> (Set.isSubset (out pm) ss') && (Set.isSubset (out pm') ss)

let consistentIdentifications (lts:LTS) (lts':LTS) (ids:(State*State) list) =
    List.forall (consistentIdentification lts lts') ids

let rec merge (m1:LTS) (m2:LTS) (ids:(State*State) list) =
    if List.isEmpty ids then
        Some (addLTSTo m2 m1)
    else
        if consistentIdentifications m1 m2 ids then
            let m2' = applyIdentifications ids m2
            let ids' = getIdentifications m1 m2'
            merge m1 m2' ids'
        else 
            None

let uniqueifyStates (((states, trans, labels), w):PointedLTS) (n:int) = 
    let states' = List.map (fun x -> x + n) states
    let uniqueifyRel rel n = List.map (fun (w, w') -> (w + n, w' + n)) rel
    let trans' = List.map (fun (s,rel) -> (s, uniqueifyRel rel n)) trans
    let labels' = List.map (fun (s,l) -> (s + n,l)) labels
    let w' = w + n
    ((states', trans', labels'), w')    

let glb (a:Model) (b:Model) : Model =
    match (a,b) with
    | None, _ -> None
    | _, None -> None
    | Some m, Some m' ->
        let n = maxState m
        let f (lts,w) (lts',w') =
            let lts'' = merge lts lts' [(w',w)]
            if isSome lts'' then
                Some (get lts'', w)
            else
                None
        f m (uniqueifyStates m' n)

let addSymbolToModel (s:Symbol) (plts:PointedLTS) : PointedLTS = 
    let ((states,trans,labels),w) = plts
    let newState = maxState plts + 1
    let states' = newState :: states
    let labels' = (newState, LStar) :: labels
    let newTran = (s, [(newState,w)])
    let trans' = mergeTransitions trans [newTran]
    ((states', trans', labels'), newState)

let rec simpl p : Model =
    match p with
    | Top -> Some (([1], [], [(1, LStar)]), 1)
    | Bot -> None
    | Conj (p,q) -> glb (simpl p) (simpl q)
    | Trans (s,p) -> let m = simpl p
                     if isSome m then
                        Some (addSymbolToModel s (get m))
                     else
                        None
    | Bang ss -> Some (([1], [], [(1, LBang ss)]), 1)

/// entailment ///

let entails p q =
    satisfies (simpl p) q

let inline (|=) (x: Prop) (y: Prop) = entails x y

/// constructors ///

let bang syms =
    Set.ofList syms |> Bang

let trans sym prop =
    Trans (sym,prop)

let conj a b =
    Conj (a,b)

[<EntryPoint>]
let main argv =
    let t1 = (bang ["a"]) |= (bang ["a"; "b"])
    let t2 = (conj (bang ["a"]) (trans "b" Top)) |= (trans "c" Top)
    let t3 = (conj (bang ["a"]) (trans "a" Top)) |= (trans "c" Top) |> not
    let t4 = (bang ["a"; "b"]) |= (bang ["b"; "a"])
    let t5 = (trans "a" (trans "b" Top)) |= trans "a" Top
    let t6 = trans "a" Top |= bang ["a"] |> not
    let t7 = conj (bang ["a"; "c"]) (bang ["b"; "c"]) |= bang ["c"]
    let t8 = conj (bang ["a"]) (trans "a" Top) |= trans "a" (bang []) |> not
    let t9 = conj (bang ["a"]) (trans "a" (trans "b" Top)) |= trans "a" (conj (trans "b" Top) (bang ["b"])) |> not
    let t10 = trans "a" (conj (trans "b" Top) (bang ["b"])) |= trans "a" (conj (trans "c" Top) (bang ["c"])) |> not
    let t11 = trans "a" Top |= trans "b" Top |> not
    let t12 = trans "a" Top |= conj (trans "a" Top) (bang ["a"]) |> not
    let t13 = trans "a" (trans "b" Top) |= trans "b" (trans "a" Top) |> not
    let t14 = conj (trans "a" (bang [])) (trans "b" (bang [])) |= trans "a" (trans "b" Top) |> not
    let t15 = conj (trans "a" Top) (conj (trans "b" Top) (trans "c" Top)) |= trans "b" Top
    let t16 = trans "a" Top |= Top
    let t17 = trans "a" (trans "b" Top) |= trans "a" Top
    let t18 = trans "a" (trans "b" Top) |= trans "b" Top |> not
    let t19 = Bot |= trans "a" Top
    let t20 = conj (bang []) (trans "a" Top) |= trans "b" Top
    let t21 = trans "a" Bot |= trans "b" Top
    let t22 = conj (trans "a" (trans "b" Top)) (trans "a" (trans "c" Top)) |= trans "a" (conj (trans "b" Top) (trans "c" Top))
    let t23 = conj (bang ["b"]) (trans "a" Top) |= trans "c" Top
    let t24 = trans "a" (conj  (trans "b" Top) (conj  (trans "c" Top)  (trans "d" Top))) |= trans "a" (trans "c" Top)
    let t25 = conj (trans "a" (conj (bang ["b"]) (trans "b" Top))) (trans "a" (bang [])) |= trans "c" Top
    let t26 = conj (trans "c" (conj (trans "b" Top) (trans "c" (bang [])))) (trans "c" (trans "c" (trans "a" Top))) |= trans "d" Top

    let tests = [t1; t2; t3; t4; t5; t6; t7; t8; t9; t10; t11; t12; t13; t14; t15; t16; t17; t18; t19; t20; t21; t22; t23; t24; t25; t26]

    if List.forall id tests then
        printfn "All tests passing"
    else
        let firstFailure = List.findIndex not tests
        printfn "First failure %A" firstFailure
    0 // return an integer exit code
