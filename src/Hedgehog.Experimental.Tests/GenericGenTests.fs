module Hedgehog.Experimental.Tests.GenericGenTests

open System
open Xunit
open Swensen.Unquote
open Hedgehog

type Uuid = Uuid of Guid
type Name = Name of string
type Id<'a> = Id of Guid
type Either<'a, 'b> = Left of 'a | Right of 'b
type Maybe<'a> = Just of 'a | Nothing

type OuterRecord = { Value: Maybe<Guid> }

type OuterClass(value: Maybe<Guid>) =
    member val Value = value with get, set


type GenericTestGenerators =

  // Test that we can override the "default" generator for a type
  static member Guid() =
    Gen.byte (Range.constantBounded())
    |> Gen.array (Range.singleton 12)
    |> Gen.map (Array.append (Array.zeroCreate 4))
    |> Gen.map Guid

  // A generator for Id<'a> to test phantom generic type
  static member Id<'a>(gen : Gen<Guid>) : Gen<Id<'a>> = gen |> Gen.map Id

  // A generator for some simple value to test a generator without parameters
  static member UuidGen() : Gen<Uuid> = GenericTestGenerators.Guid() |> Gen.map Uuid

  // A generator for some simple value to test a generator with parameters
  static member NameGen(gen: Gen<string>) : Gen<Name> =
    gen |> Gen.map (fun x -> Name ("Name: " + x))

  // A generator for Maybe<'a> to test union type with one generic type constructor
  static member AlwaysJust<'a>(genA: Gen<'a>) : Gen<Maybe<'a>> = genA |> Gen.map Just

  // A generator for Either<'a, 'b> to test union type with multiple type constructors
  static member AlwaysLeft<'a, 'b>(genB: Gen<'b>, genA: Gen<'a>) : Gen<Either<'a, 'b>> =
        genA |> Gen.map Left

let checkWith tests = PropertyConfig.defaultConfig |> PropertyConfig.withTests tests |> Property.checkWith

let isCustomGuid (guid: Guid) = guid.ToByteArray()[..3] |> Array.forall ((=) 0uy)

[<Fact>]
let ``should generate value with phantom generic type - Id<'a>``() =
   let config = GenX.defaults |> AutoGenConfig.addGenerators<GenericTestGenerators>
   checkWith 100<tests> <| property {
        let! x = GenX.autoWith<Id<string>> config
        test <@ x |> function Id a -> isCustomGuid a @>
    }

[<Fact>]
let ``should generate generic value for union type - Either<'a, 'b>``() =
   let config = GenX.defaults |> AutoGenConfig.addGenerators<GenericTestGenerators>
   checkWith 100<tests> <| property {
        let! x = GenX.autoWith<Either<int, string>> config
        test <@ x |> function Left _ -> true | _ -> false @>
    }

[<Fact>]
let ``should generate generic value for union type - Maybe<'a>``() =
   let config = GenX.defaults |> AutoGenConfig.addGenerators<GenericTestGenerators>
   checkWith 100<tests> <| property {
        let! x = GenX.autoWith<Maybe<string>> config
        test <@ x |> function Just _ -> true  | _ -> false @>
    }

[<Fact>]
let ``should generate value using a generator without parameters: Uuid``() =
   let config = GenX.defaults |> AutoGenConfig.addGenerators<GenericTestGenerators>
   checkWith 100<tests> <| property {
        let! x = GenX.autoWith<Maybe<Uuid>> config
        test <@ x |> function Just (Uuid x) -> isCustomGuid x  | _ -> failwith "todo"@>
    }

[<Fact>]
let ``should generate value using a generator with parameters: Name``() =
   let config = GenX.defaults |> AutoGenConfig.addGenerators<GenericTestGenerators>
   checkWith 100<tests> <| property {
        let! x = GenX.autoWith<Name> config
        test <@ x |> function Name x -> x.StartsWith("Name: ") @>
    }

[<Fact>]
let ``should generate outer FSharp record with generic type inside``() =
   let config = GenX.defaults |> AutoGenConfig.addGenerators<GenericTestGenerators>
   checkWith 100<tests> <| property {
        let! x = GenX.autoWith<OuterRecord> config
        test <@ x |> function { Value = Just x } -> isCustomGuid x | _ -> false @>
    }

[<Fact>]
let ``should generate outer class with generic type inside``() =
   let config = GenX.defaults |> AutoGenConfig.addGenerators<GenericTestGenerators>
   checkWith 100<tests> <| property {
        let! x = GenX.autoWith<OuterClass> config
        test <@ x |> function cls -> match cls.Value with Just v -> isCustomGuid v | _ -> false @>
    }
