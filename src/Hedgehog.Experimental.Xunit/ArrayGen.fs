module internal ArrayGen

open System
open Hedgehog

let toGenTuple = function
  | [||] -> failwith "No generators in the list."
  | [|a|] -> gen {
    let! a = a
    return (Tuple.Create a) |> box }
  | [|a;b|] -> gen {
    let! a = a
    let! b = b
    return (a,b) |> box }
  | [|a;b;c|] -> gen {
    let! a = a
    let! b = b
    let! c = c
    return (a,b,c) |> box }
  | _ -> failwith "Too many generators in the list."
