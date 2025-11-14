module OrTypeTest

open Xunit
open Swensen.Unquote
open Hedgehog

type Or<'A, 'B> =
    | Left of 'A
    | Right of 'B

type OrGenerators =
    static member OrSame<'A>(genA: Gen<'A>) : Gen<Or<'A, 'A>> =
        Gen.choice [
            genA |> Gen.map Left
            genA |> Gen.map Right
        ]

[<Fact>]
let ``Should generate Or with same type for both parameters``() =
    let config =
        GenX.defaults
        |> AutoGenConfig.addGenerators<OrGenerators>

    let gen = GenX.autoWith<Or<int, int>> config
    let sample = Gen.sample 0 1 gen |> Seq.head

    match sample with
    | Left x -> test <@ x >= 0 @>  // Just verify it's a valid int
    | Right x -> test <@ x >= 0 @>
