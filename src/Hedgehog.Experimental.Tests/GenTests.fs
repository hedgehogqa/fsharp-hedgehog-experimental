module Hedgehog.Experimental.Tests.GenTests

open System
open Xunit
open Swensen.Unquote
open Hedgehog

let checkWith tests = PropertyConfig.defaultConfig |> PropertyConfig.withTests tests |> Property.checkWith

[<Fact>]
let ``uri generates valid URIs`` () =
    checkWith 10000<tests> <| property {
        let! uri = GenX.uri
        ignore uri
    }

[<Fact>]
let ``shuffle does not add or remove elements`` () =
    Property.check <| property {
        let! xs =
            Gen.int32 (Range.constantFrom 0 -100 100)
            |> Gen.list (Range.linear 2 10)
        let! shuffled = xs |> GenX.shuffle
        test <@ List.sort xs = List.sort shuffled @>
    }

[<Fact>]
let ``shuffle creates random permutations of the input list`` () =
    checkWith 1<tests> <| property {
        let! xs =
            Gen.int32 (Range.constantFrom 0 -100 100)
            |> Gen.list (Range.singleton 10)
            |> Gen.filter (fun l -> (List.distinct l).Length > 5)
        let! permutations = xs |> GenX.shuffle |> Gen.list (Range.singleton 100)
        test <@ permutations |> List.distinct |> List.length > 50 @>
    }

[<Fact>]
let ``shuffleCase does not add, remove, or change the order of characters`` () =
    Property.check <| property {
        let! s = Gen.string (Range.linear 2 10) Gen.alpha
        let! shuffled = s |> GenX.shuffleCase
        test <@ shuffled.ToLowerInvariant() = s.ToLowerInvariant() @>
    }

[<Fact>]
let ``shuffleCase creates random case permutations of the input string`` () =
    checkWith 1<tests> <| property {
        let! xs = Gen.string (Range.linear 50 100) Gen.alpha
        let! permutations = xs |> GenX.shuffleCase |> Gen.list (Range.singleton 100)
        test <@ permutations |> List.distinct |> List.length > 50 @>
    }

[<Fact>]
let ``iNotEqualTo does not generate a string equal to another string ignoring case`` () =
    Property.check <| property {
        let! s1 = Gen.string (Range.singleton 1) Gen.alpha
        let! s2 = Gen.string (Range.singleton 1) Gen.alpha |> GenX.iNotEqualTo s1
        test <@ s1.ToLower() <> s2.ToLower() @>
    }

[<Fact>]
let ``notSubstringOf does not generate a string that contains another string`` () =
    Property.check <| property {
        let! super = Gen.string (Range.singleton 3) Gen.alpha
        let! sub = Gen.string (Range.singleton 1) Gen.alpha |> GenX.notSubstringOf super
        test <@ not <| super.Contains sub @>
    }

[<Fact>]
let ``iNotSubstringOf does not generate a string that contains another string ignoring case`` () =
    Property.check <| property {
        let! super = Gen.string (Range.singleton 3) Gen.alpha
        let! sub = Gen.string (Range.singleton 1) Gen.alpha |> GenX.iNotSubstringOf super
        test <@ super.ToLower().IndexOf(sub.ToLower()) = -1 @>
    }

[<Fact>]
let ``notStartsWith does not generate a string that starts with another string`` () =
    Property.check <| property {
        let! sub = Gen.string (Range.singleton 1) Gen.alpha
        let! super = Gen.string (Range.singleton 2) Gen.alpha |> GenX.notStartsWith sub
        test <@ not <| super.StartsWith sub @>
    }

[<Fact>]
let ``iNotStartsWith does not generate a string that starts with another string ignoring case`` () =
    Property.check <| property {
        let! sub = Gen.string (Range.singleton 1) Gen.alpha
        let! super = Gen.string (Range.singleton 2) Gen.alpha |> GenX.iNotStartsWith sub
        test <@ not <| super.ToLower().StartsWith (sub.ToLower()) @>
    }

[<Fact>]
let ``withNull generates null some of the time`` () =
    Gen.constant "a"
    |> GenX.withNull
    |> Gen.sample 0 1000
    |> Seq.exists isNull

[<Fact>]
let ``noNull does not generate nulls`` () =
    Property.check <| property {
        let! x = Gen.constant "a" |> GenX.withNull |> GenX.noNull
        test <@ not <| isNull x @>
    }

[<Fact>]
let ``notEqualTo does not generate a value equal to another value`` () =
    Property.check <| property {
        let! x = Gen.int32 (Range.constant 1 5)
        let! y = Gen.int32 (Range.constant 1 5) |> GenX.notEqualTo x
        x <>! y
    }

[<Fact>]
let ``notEqualToOpt does not generate a value equal to another option-wrapped value`` () =
    Property.check <| property {
        let! xOpt = Gen.int32 (Range.constant 1 5) |> Gen.option
        let! y = Gen.int32 (Range.constant 1 5) |> GenX.notEqualToOpt xOpt
        test <@ match xOpt with
                | Some x -> x <> y
                | None -> true @>
    }

[<Fact>]
let ``notIn generates element that is not in list`` () =
    Property.check <| property {
        let! xs =
            Gen.int32 (Range.linearFrom 0 -100 100)
            |> Gen.list (Range.linear 1 10)
        let! x = Gen.int32 (Range.linearFrom 0 -100 100) |> GenX.notIn xs
        return not <| List.contains x xs
    }

[<Fact>]
let ``notContains generates list that does not contain element`` () =
    Property.check <| property {
        let! x = Gen.int32 (Range.linearFrom 0 -100 100)
        let! xs =
            Gen.int32 (Range.linearFrom 0 -100 100)
            |> Gen.list (Range.linear 1 10)
            |> GenX.notContains x
        return not <| List.contains x xs
    }

[<Fact>]
let ``addElement generates a list with the specified element`` () =
    Property.check <| property {
        let! x = Gen.int32 (Range.exponentialBounded ())
        let! xs =
            Gen.int32 (Range.exponentialBounded ())
            |> Gen.list (Range.linear 0 10)
            |> GenX.addElement x
        return List.contains x xs
    }

[<Fact>]
let ``sorted2 generates a sorted 2-tuple`` () =
    Property.check <| property {
        let! x1, x2 =
            Gen.int32 (Range.exponentialBounded ())
            |> Gen.tuple
            |> GenX.sorted2
        x1 <=! x2
    }

[<Fact>]
let ``sorted3 generates a sorted 3-tuple`` () =
    Property.check <| property {
        let! x1, x2, x3 =
            Gen.int32 (Range.exponentialBounded ())
            |> Gen.tuple3
            |> GenX.sorted3
        x1 <=! x2
        x2 <=! x3
    }

[<Fact>]
let ``sorted4 generates a sorted 4-tuple`` () =
    Property.check <| property {
        let! x1, x2, x3, x4 =
            Gen.int32 (Range.exponentialBounded ())
            |> Gen.tuple4
            |> GenX.sorted4
        x1 <=! x2
        x2 <=! x3
        x3 <=! x4
    }

[<Fact>]
let ``distinct2 generates 2 non-equal elements`` () =
    Property.check <| property {
        let! x1, x2 =
            Gen.int32 (Range.exponentialBounded ())
            |> Gen.tuple
            |> GenX.distinct2
        [x1; x2] |> List.distinct =! [x1; x2]
    }

[<Fact>]
let ``distinct3 generates 3 non-equal elements`` () =
    Property.check <| property {
        let! x1, x2, x3 =
            Gen.int32 (Range.exponentialBounded ())
            |> Gen.tuple3
            |> GenX.distinct3
        [x1; x2; x3] |> List.distinct =! [x1; x2; x3]
    }

[<Fact>]
let ``distinct4 generates 4 non-equal elements`` () =
    Property.check <| property {
        let! x1, x2, x3, x4 =
            Gen.int32 (Range.exponentialBounded ())
            |> Gen.tuple4
            |> GenX.distinct4
        [x1; x2; x3; x4] |> List.distinct =! [x1; x2; x3; x4]
    }

[<Fact>]
let ``increasing2 generates a 2-tuple with strictly increasing elements`` () =
    Property.check <| property {
        let! x1, x2 =
            Gen.int32 (Range.exponentialBounded ())
            |> Gen.tuple
            |> GenX.increasing2
        x1 <! x2
    }

[<Fact>]
let ``increasing3 generates a 3-tuple with strictly increasing elements`` () =
    Property.check <| property {
        let! x1, x2, x3 =
            Gen.int32 (Range.exponentialBounded ())
            |> Gen.tuple3
            |> GenX.increasing3
        x1 <! x2
        x2 <! x3
    }

[<Fact>]
let ``increasing4 generates a 4-tuple with strictly increasing elements`` () =
    Property.check <| property {
        let! x1, x2, x3, x4 =
            Gen.int32 (Range.exponentialBounded ())
            |> Gen.tuple4
            |> GenX.increasing4
        x1 <! x2
        x2 <! x3
        x3 <! x4
    }

[<Fact>]
let ``dateInterval generates two dates spaced no more than the range allows`` () =
    Property.check <| property {
        let! d1, d2 = GenX.dateInterval (Range.linear 0 100)
        (d2-d1).TotalDays <=! 100.
    }

[<Fact>]
let ``dateInterval with positive interval generates increasing dates`` () =
    Property.check <| property {
        let! d1, d2 = GenX.dateInterval (Range.linear 0 100)
        d2 >=! d1
    }

[<Fact>]
let ``dateInterval with negative interval generates increasing dates`` () =
    Property.check <| property {
        let! d1, d2 = GenX.dateInterval (Range.linear 0 -100)
        d2 <=! d1
    }

[<Fact>]
let ``withMapTo is defined for all elements in input list`` () =
    Property.check <| property {
        let! xs, f =
            Gen.int32 (Range.exponentialBounded ())
            |> Gen.list (Range.linear 1 50)
            |> GenX.withMapTo Gen.alphaNum
        xs |> List.map f |> ignore // Should not throw.
    }

[<Fact>]
let ``withDistinctMapTo is defined for all elements in input list`` () =
    Property.check <| property {
        let! xs, f =
            Gen.int32 (Range.exponentialBounded ())
            |> Gen.list (Range.linear 1 50)
            |> GenX.withDistinctMapTo Gen.alphaNum
        xs |> List.map f |> ignore // Should not throw.
    }

[<Fact>]
let ``withDistinctMapTo guarantees that distinct input values map to distinct output values`` () =
    Property.check <| property {
        let! xs, f =
            Gen.int32 (Range.exponentialBounded ())
            |> Gen.list (Range.linear 1 50)
            |> GenX.withDistinctMapTo Gen.alphaNum
        let xsDistinct = xs |> List.distinct
        xsDistinct |> List.map f |> List.distinct |> List.length =! xsDistinct.Length
    }


type RecOption =
  {X: RecOption option}
  member this.Depth =
    match this.X with
    | None -> 0
    | Some x -> x.Depth + 1

[<Fact>]
let ``auto with recursive option members does not cause stack overflow using default settings`` () =
    Property.check <| property {
        let! _ = GenX.auto<RecOption>
        return true
    }

[<Fact>]
let ``auto with recursive option members respects max recursion depth`` () =
    Property.check <| property {
        let! depth = Gen.int32 <| Range.exponential 0 5
        let! x = GenX.autoWith<RecOption> {GenX.defaults with RecursionDepth = depth}
        x.Depth <=! depth
    }

[<Fact>]
let ``auto with recursive option members generates some values with max recursion depth`` () =
    checkWith 10<tests> <| property {
        let! depth = Gen.int32 <| Range.linear 1 5
        let! xs = GenX.autoWith<RecOption> {GenX.defaults with RecursionDepth = depth}
                  |> (Gen.list (Range.singleton 100))
        test <@ xs |> List.exists (fun x -> x.Depth = depth) @>
    }


type RecArray =
  {X: RecArray array}
  member this.Depth =
    match this.X with
    | [||] -> 0
    | xs -> xs |> Array.map (fun x -> x.Depth + 1) |> Array.max

[<Fact>]
let ``auto with recursive array members does not cause stack overflow using default settings`` () =
    Property.check <| property {
        let! _ = GenX.auto<RecArray>
        return true
    }

[<Fact>]
let ``auto with recursive array members respects max recursion depth`` () =
    Property.check <| property {
        let! depth = Gen.int32 <| Range.exponential 0 5
        let! x = GenX.autoWith<RecArray> {GenX.defaults with RecursionDepth = depth; SeqRange = Range.exponential 0 5}
        x.Depth <=! depth
    }

[<Fact>]
let ``auto with recursive array members generates some values with max recursion depth`` () =
    checkWith 10<tests> <| property {
        let! depth = Gen.int32 <| Range.linear 1 5
        let! xs = GenX.autoWith<RecArray> {GenX.defaults with RecursionDepth = depth; SeqRange = Range.exponential 1 5}
                  |> (Gen.list (Range.singleton 100))
        test <@ xs |> List.exists (fun x -> x.Depth = depth) @>
    }


type RecList =
  {X: RecList list}
  member this.Depth =
    match this.X with
    | [] -> 0
    | xs -> xs |> List.map (fun x -> x.Depth + 1) |> List.max

[<Fact>]
let ``auto with recursive list members does not cause stack overflow using default settings`` () =
    Property.check <| property {
        let! _ = GenX.auto<RecList>
        return true
    }

[<Fact>]
let ``auto with recursive list members respects max recursion depth`` () =
    Property.check <| property {
        let! depth = Gen.int32 <| Range.exponential 0 5
        let! x = GenX.autoWith<RecList> {GenX.defaults with RecursionDepth = depth; SeqRange = Range.exponential 0 5}
        x.Depth <=! depth
    }

[<Fact>]
let ``auto with recursive list members generates some values with max recursion depth`` () =
    checkWith 10<tests> <| property {
        let! depth = Gen.int32 <| Range.linear 1 5
        let! xs = GenX.autoWith<RecList> {GenX.defaults with RecursionDepth = depth; SeqRange = Range.exponential 1 5}
                  |> (Gen.list (Range.singleton 100))
        test <@ xs |> List.exists (fun x -> x.Depth = depth) @>
    }


type RecResizeArray =
  {X: RecResizeArray ResizeArray}
  member this.Depth =
    match this.X with
    | x when x.Count = 0 -> 0
    | xs -> xs |> Seq.map (fun x -> x.Depth + 1) |> Seq.max

[<Fact>]
let ``auto with recursive ResizeArray members does not cause stack overflow using default settings`` () =
    Property.check <| property {
        let! _ = GenX.auto<RecResizeArray>
        return true
    }

[<Fact>]
let ``auto with recursive ResizeArray members respects max recursion depth`` () =
    Property.check <| property {
        let! depth = Gen.int32 <| Range.exponential 0 5
        let! x = GenX.autoWith<RecResizeArray> {GenX.defaults with RecursionDepth = depth; SeqRange = Range.exponential 0 5}
        x.Depth <=! depth
    }

[<Fact>]
let ``auto with recursive ResizeArray members generates some values with max recursion depth`` () =
    checkWith 10<tests> <| property {
        let! depth = Gen.int32 <| Range.linear 1 5
        let! xs = GenX.autoWith<RecResizeArray> {GenX.defaults with RecursionDepth = depth; SeqRange = Range.exponential 1 5}
                  |> (Gen.list (Range.singleton 100))
        test <@ xs |> List.exists (fun x -> x.Depth = depth) @>
    }


type RecDictionary =
  {X: System.Collections.Generic.Dictionary<RecDictionary, RecDictionary>}
  member this.Depth =
    match this.X with
    | x when x.Count = 0 -> 0
    | xs -> xs |> Seq.collect (fun x -> seq {x.Key.Depth + 1; x.Value.Depth} ) |> Seq.max

[<Fact>]
let ``auto with recursive Dictionary members does not cause stack overflow using default settings`` () =
    Property.check <| property {
        let! _ = GenX.auto<RecDictionary>
        return true
    }

[<Fact>]
let ``auto with recursive Dictionary members respects max recursion depth`` () =
    Property.check <| property {
        let! depth = Gen.int32 <| Range.exponential 0 5
        let! x = GenX.autoWith<RecDictionary> {GenX.defaults with RecursionDepth = depth; SeqRange = Range.exponential 0 5}
        x.Depth <=! depth
    }

[<Fact>]
let ``auto with recursive Dictionary members generates some values with max recursion depth`` () =
    checkWith 10<tests> <| property {
        let! depth = Gen.int32 <| Range.linear 1 5
        let! xs = GenX.autoWith<RecDictionary> {GenX.defaults with RecursionDepth = depth; SeqRange = Range.exponential 1 5}
                  |> (Gen.list (Range.singleton 100))
        test <@ xs |> List.exists (fun x -> x.Depth = depth) @>
    }


type RecSet =
  {X: Set<RecSet>}
  member this.Depth =
    if this.X.IsEmpty then 0
    else
      this.X |> Seq.map (fun x -> x.Depth + 1) |> Seq.max

[<Fact>]
let ``auto with recursive set members does not cause stack overflow using default settings`` () =
    Property.check <| property {
        let! _ = GenX.auto<RecSet>
        return true
    }

[<Fact>]
let ``auto with recursive set members respects max recursion depth`` () =
    Property.check <| property {
        let! depth = Gen.int32 <| Range.exponential 0 5
        let! x = GenX.autoWith<RecSet> {GenX.defaults with RecursionDepth = depth; SeqRange = Range.exponential 0 5}
        x.Depth <=! depth
    }

[<Fact>]
let ``auto with recursive set members generates some values with max recursion depth`` () =
    checkWith 10<tests> <| property {
        let! depth = Gen.int32 <| Range.linear 1 5
        let! xs = GenX.autoWith<RecSet> {GenX.defaults with RecursionDepth = depth; SeqRange = Range.exponential 1 5}
                  |> (Gen.list (Range.singleton 100))
        test <@ xs |> List.exists (fun x -> x.Depth = depth) @>
    }


type RecMap =
  {X: Map<RecMap, RecMap>}
  member this.Depth =
    if this.X.IsEmpty then 0
    else
      this.X |> Map.toSeq |> Seq.map (fun (k, v)  -> max (k.Depth + 1) (v.Depth + 1)) |> Seq.max

[<Fact>]
let ``auto with recursive map members does not cause stack overflow using default settings`` () =
    Property.check <| property {
        let! _ = GenX.auto<RecMap>
        return true
    }

[<Fact>]
let ``auto with recursive map members respects max recursion depth`` () =
    Property.check <| property {
        let! depth = Gen.int32 <| Range.exponential 0 5
        let! x = GenX.autoWith<RecMap> {GenX.defaults with RecursionDepth = depth; SeqRange = Range.exponential 0 5}
        x.Depth <=! depth
    }

[<Fact>]
let ``auto with recursive map members generates some values with max recursion depth`` () =
    checkWith 10<tests> <| property {
        let! depth = Gen.int32 <| Range.linear 1 5
        let! xs = GenX.autoWith<RecMap> {GenX.defaults with RecursionDepth = depth; SeqRange = Range.exponential 1 5}
                  |> (Gen.list (Range.singleton 100))
        test <@ xs |> List.exists (fun x -> x.Depth = depth) @>
    }


type MutuallyRecursive1 =
  {X: MutuallyRecursive2 option}
  member this.Depth =
    match this.X with
    | None -> 0
    | Some {X = []} -> 0
    | Some {X = mc1s} ->
        mc1s
        |> List.map (fun mc1 -> mc1.Depth + 1)
        |> List.max

and MutuallyRecursive2 =
  {X: MutuallyRecursive1 list}
  member this.Depth =
    if this.X.IsEmpty then 0
    else
      let depths =
        this.X
        |> List.choose (fun mc1 -> mc1.X)
        |> List.map (fun mc2 -> mc2.Depth + 1)
      if depths.IsEmpty then 0 else List.max depths

[<Fact>]
let ``auto with mutually recursive types does not cause stack overflow using default settings`` () =
    Property.check <| property {
        let! _ = GenX.auto<MutuallyRecursive1>
        let! _ = GenX.auto<MutuallyRecursive2>
        return true
    }

[<Fact>]
let ``auto with mutually recursive types respects max recursion depth`` () =
    Property.check <| property {
        let! depth = Gen.int32 <| Range.exponential 0 5
        let! x1 = GenX.autoWith<MutuallyRecursive1> {GenX.defaults with RecursionDepth = depth; SeqRange = Range.exponential 0 5}
        let! x2 = GenX.autoWith<MutuallyRecursive2> {GenX.defaults with RecursionDepth = depth; SeqRange = Range.exponential 0 5}
        x1.Depth <=! depth
        x2.Depth <=! depth
    }

[<Fact>]
let ``auto with mutually recursive types generates some values with max recursion depth`` () =
    checkWith 10<tests> <| property {
        let! depth = Gen.int32 <| Range.linear 1 5
        let! xs1 = GenX.autoWith<MutuallyRecursive1> {GenX.defaults with RecursionDepth = depth; SeqRange = Range.exponential 1 5}
                  |> (Gen.list (Range.singleton 100))
        let! xs2 = GenX.autoWith<MutuallyRecursive2> {GenX.defaults with RecursionDepth = depth; SeqRange = Range.exponential 1 5}
                  |> (Gen.list (Range.singleton 100))
        test <@ xs1 |> List.exists (fun x -> x.Depth = depth) @>
        test <@ xs2 |> List.exists (fun x -> x.Depth = depth) @>
    }

[<Fact>]
let ``auto with UInt64 generates UInt64`` () =
    Property.check <| property {
        let! _ = GenX.auto<uint64>
        return true
    }

[<Fact>]
let ``auto can generate valid URIs`` () =
    checkWith 10000<tests> <| property {
        let! uri = GenX.auto<System.Uri>
        ignore uri
    }


type Enum =
  | A = 1
  | B = 2


[<Fact>]
let ``auto can generate enums`` () =
    checkWith 1<tests> <| property {
        let! enums =
          GenX.auto<Enum>
          |> GenX.cList 1000 1000
        test <@ enums |> List.contains Enum.A @>
        test <@ enums |> List.contains Enum.B @>
        test <@ enums |> List.forall (fun e -> e = Enum.A || e = Enum.B) @>
    }


[<Fact>]
let ``auto can generate byte`` () =
    Property.check <| property {
        let! _ = GenX.auto<byte>
        ()
    }


[<Fact>]
let ``auto can generate int16`` () =
    Property.check <| property {
        let! _ = GenX.auto<int16>
        ()
    }


[<Fact>]
let ``auto can generate uint16`` () =
    Property.check <| property {
        let! _ = GenX.auto<uint16>
        ()
    }


[<Fact>]
let ``auto can generate int`` () =
    Property.check <| property {
        let! _ = GenX.auto<int>
        ()
    }


[<Fact>]
let ``auto can generate uint32`` () =
    Property.check <| property {
        let! _ = GenX.auto<uint32>
        ()
    }


[<Fact>]
let ``auto can generate int64`` () =
    Property.check <| property {
        let! _ = GenX.auto<int64>
        ()
    }


[<Fact>]
let ``auto can generate uint64`` () =
    Property.check <| property {
        let! _ = GenX.auto<uint64>
        ()
    }


[<Fact>]
let ``auto can generate single`` () =
    Property.check <| property {
        let! _ = GenX.auto<single>
        ()
    }


[<Fact>]
let ``auto can generate float`` () =
    Property.check <| property {
        let! _ = GenX.auto<float>
        ()
    }


[<Fact>]
let ``auto can generate decimal`` () =
    Property.check <| property {
        let! _ = GenX.auto<decimal>
        ()
    }


[<Fact>]
let ``auto can generate bool`` () =
    Property.check <| property {
        let! _ = GenX.auto<bool>
        ()
    }


[<Fact>]
let ``auto can generate GUID`` () =
    Property.check <| property {
        let! _ = GenX.auto<Guid>
        ()
    }


[<Fact>]
let ``auto can generate char`` () =
    Property.check <| property {
        let! _ = GenX.auto<char>
        ()
    }


[<Fact>]
let ``auto can generate string`` () =
    Property.check <| property {
        let! _ = GenX.auto<string>
        ()
    }


[<Fact>]
let ``auto can generate DateTime`` () =
    Property.check <| property {
        let! _ = GenX.auto<DateTime>
        ()
    }


[<Fact>]
let ``auto can generate DateTimeOffset`` () =
    Property.check <| property {
        let! _ = GenX.auto<DateTimeOffset>
        ()
    }


type TypeWithoutAccessibleCtor private (state: int) =
    static member CustomConstructor (state) = TypeWithoutAccessibleCtor (state)
    member this.State = state


let myTypeGen = gen {
    let! state = Gen.int32 <| Range.exponentialBounded ()
    return TypeWithoutAccessibleCtor.CustomConstructor state
}


[<Fact>]
let ``auto can generate custom classes with no suitable constructors using overrides`` () =
  checkWith 1<tests> <| property {
      let config = GenX.defaults |> AutoGenConfig.addGenerator myTypeGen
      let! _ = GenX.autoWith<TypeWithoutAccessibleCtor> config
      ()
  }


let constantIntGen = Gen.constant 1


[<Fact>]
let ``auto uses specified overrides`` () =
  Property.check <| property {
      let config = GenX.defaults |> AutoGenConfig.addGenerator constantIntGen
      let! i = GenX.autoWith<int> config
      test <@ i = 1 @>
  }


module ShrinkTests =

  // We need to hit an error case in order to test shrinking. That error case may be uncommon.
  // We therefore run 1 million tests to increase the probability of hitting an error case.
  let render property =
    let config =
      PropertyConfig.defaultConfig
      |> PropertyConfig.withTests 1_000_000<tests>
    Property.reportWith config property
    |> Report.render

  type MyRecord =
    { String: string
      Int: int }

  [<Fact>]
  let ``auto of record shrinks correctly`` () =
    let property = property {
      let! value = GenX.auto<MyRecord>
      test <@ not (value.String.Contains('b')) @>
    }
    let rendered = render property
    test <@ rendered.Contains "{String = \"b\";\n Int = 0;}" @>


  type MyCliMutable() =
    let mutable myString = ""
    let mutable myInt = 0
    member _.String
      with get () = myString
      and set value = myString <- value
    member _.Int
      with get () = myInt
      and set value = myInt <- value
    override _.ToString() =
      "String = " + myString + "; Int = " + myInt.ToString()

  [<Fact>]
  let ``auto of CLI mutable shrinks correctly`` () =
    let property = property {
      let! value = GenX.auto<MyCliMutable>
      test <@ not (value.String.Contains('b')) @>
    }
    let rendered = render property
    test <@ rendered.Contains "String = b; Int = 0" @>


  [<RequireQualifiedAccess>]
  type MyDu =
    | Case1 of String * int

  [<Fact>]
  let ``auto of discriminated union shrinks correctly`` () =
    let property = property {
      let! MyDu.Case1(s, i) = GenX.auto<MyDu>
      test <@ not (s.Contains('b')) @>
    }
    let rendered = render property
    test <@ rendered.Contains "Case1 (\"b\",0)" @>


  [<Fact>]
  let ``auto of tuple shrinks correctly`` () =
    let property = property {
      let! (s, _) = GenX.auto<string * int>
      test <@ not (s.Contains('b')) @>
    }
    let rendered = render property
    test <@ rendered.Contains "(\"b\", 0)" @>

  [<Fact>]
  let ``shuffleCase shrinks correctly`` () =
    let property = property {
      let! value = GenX.shuffleCase "abcdefg"
      test <@ not (value.StartsWith "A") @>
    }
    let rendered = render property
    test <@ rendered.Contains "\"Abcdefg\"" @>

  [<Fact>]
  let ``shuffle shrinks correctly`` () =
    let property = property {
      let n = 10
      let nMinus1 = n - 1
      let! value =
        ()
        |> Seq.replicate n
        |> Seq.mapi (fun i _ -> i)
        |> Seq.toList
        |> GenX.shuffle
      test <@ nMinus1 <> value.Head @>
    }
    let rendered = render property
    test <@ rendered.Contains "[9; 0; 1; 2; 3; 4; 5; 6; 7; 8]" @>

  [<Fact>]
  let ``one-dimentional array shrinks correctly when empty allowed`` () =
    let property = property {
      let! array = GenX.auto<int []>
      test <@ array.Length = 0 @>
    }
    let rendered = render property
    test <@ rendered.Contains "[|0|]" @>

  [<Fact>]
  let ``one-dimentional array shrinks correctly when empty disallowed`` () =
    let property = property {
      let! array =
        { GenX.defaults with SeqRange = Range.constant 2 5 }
        |> GenX.autoWith<int []>
      test <@ 1 <> array.[0] @>
    }
    let rendered = render property
    test <@ rendered.Contains "[|1; 0" @>

  [<Fact>]
  let ``two-dimentional array shrinks correctly when empty allowed`` () =
    let property = property {
      let! array = GenX.auto<int [,]>
      test <@ array.Length = 0 @>
    }
    let rendered = render property
    test <@ rendered.Contains "[[0]]" @>

  [<Fact>]
  let ``two-dimentional array shrinks correctly when empty disallowed`` () =
    let property = property {
      let! array =
        { GenX.defaults with SeqRange = Range.constant 1 5 }
        |> GenX.autoWith<int [,]>
      test <@ 1 <> array.[0,0] @>
    }
    let rendered = render property
    test <@ rendered.Contains "[[1; 0" ||
            rendered.Contains "[[1]\n [0]" ||
            rendered.Contains "[[1]]"@>

  [<Fact>]
  let ``auto of ResizeArray shrinks correctly`` () =
    let property = property {
      let! resizeArray =
        { GenX.defaults with SeqRange = Range.constant 4 4 }
        |> GenX.autoWith<ResizeArray<int>>
      test <@ 1 <> resizeArray.[0] @>
    }
    let rendered = Property.render property
    test <@ rendered.Contains "[1; 0; 0; 0]" @>

  open System.Text.RegularExpressions
  [<Fact>]
  let ``auto of recursive ResizeArray shrinks correctly`` () =
    let property = property {
      let! rra = GenX.auto<RecResizeArray>
      test <@ not (rra.X.Count = 2) @>
    }
    let shrunkenLine = (render property).Split('\n').[1]
    let regex = {RecResizeArray.X = ResizeArray()} |> sprintf "%A" |> Regex.Escape |> Regex
    let matches = regex.Matches shrunkenLine
    test <@ 2 = matches.Count @>

[<Fact>]
let ``MultidimensionalArray.createWithGivenEntries works for 2x2`` () =
  let data = [ 0; 1; 2; 3 ]
  let lengths = [ 2; 2 ]

  let array : int [,] =
    GenX.MultidimensionalArray.createWithGivenEntries data lengths
    |> unbox

  <@
    array.[0, 0] = 0
    && array.[0, 1] = 1
    && array.[1, 0] = 2
    && array.[1, 1] = 3
  @>
  |> test
