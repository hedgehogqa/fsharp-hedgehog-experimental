module Hedgehog.Experimental.Tests.AutoGenContextTests

open Hedgehog
open Xunit
open Swensen.Unquote

type Maybe<'a> = Just of 'a | Nothing
type RecursiveType<'a> =
    { Value: Maybe<RecursiveType<'a>>}
    member this.Depth =
        match this.Value with
        | Nothing -> 0
        | Just x -> x.Depth + 1

type RecursiveGenerators =
    // override Option to always generate Some when recursion is allowed
    // using the AutoGenContext to assert recursion context preservation
    static member Option<'a>(context: AutoGenContext) =
        if context.CanRecurse then
            printfn "CurrentRecursionDepth: %d" context.CurrentRecursionDepth
            context.AutoGenerate<'a>() |> Gen.map Just
        else
            Gen.constant Nothing

[<Fact>]
let ``Should preserve recursion with generic types when using AutoGenContext.AutoGenerate``() =
    property {
        let! recDepth = Gen.int32 (Range.constant 2 5)
        let config =
           GenX.defaults
           |> AutoGenConfig.addGenerators<RecursiveGenerators>
           |> AutoGenConfig.setRecursionDepth recDepth

        let! result = GenX.autoWith<RecursiveType<int>> config
        test <@ result.Depth = recDepth @>
    } |> Property.recheck "0_8749783378671135247_1719019878934027555_"
