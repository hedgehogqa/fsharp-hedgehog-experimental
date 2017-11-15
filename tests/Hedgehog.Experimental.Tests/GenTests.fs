module Hedgehog.Experimental.Tests.GenTests

open Xunit
open Swensen.Unquote
open Hedgehog

[<Fact>]
let ``notIn generates element that is not in list`` () =
    Property.check <| property {
        let! xs =
            Gen.int (Range.linearFrom 0 -100 100)
            |> Gen.list (Range.linear 1 10)
        let! x = Gen.int (Range.linearFrom 0 -100 100) |> GenX.notIn xs
        return not <| List.contains x xs
    }

[<Fact>]
let ``notContains generates list that does not contain element`` () =
    Property.check <| property {
        let! x = Gen.int (Range.linearFrom 0 -100 100)
        let! xs =
            Gen.int (Range.linearFrom 0 -100 100)
            |> Gen.list (Range.linear 1 10)
            |> GenX.notContains x
        return not <| List.contains x xs
    }

[<Fact>]
let ``addElement generates a list with the specified element`` () =
    Property.check <| property {
        let! x = Gen.int (Range.exponentialBounded ())
        let! xs = 
            Gen.int (Range.exponentialBounded ())
            |> Gen.list (Range.linear 0 10)
            |> GenX.addElement x
        return List.contains x xs
    }

[<Fact>]
let ``sorted2 generates a sorted 2-tuple`` () =
    Property.check <| property {
        let! x1, x2 =
            Gen.int (Range.exponentialBounded ())
            |> Gen.tuple
            |> GenX.sorted2
        x1 <=! x2
    }

[<Fact>]
let ``sorted3 generates a sorted 3-tuple`` () =
    Property.check <| property {
        let! x1, x2, x3 =
            Gen.int (Range.exponentialBounded ())
            |> Gen.tuple3
            |> GenX.sorted3
        x1 <=! x2
        x2 <=! x3
    }

[<Fact>]
let ``sorted4 generates a sorted 4-tuple`` () =
    Property.check <| property {
        let! x1, x2, x3, x4 =
            Gen.int (Range.exponentialBounded ())
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
            Gen.int (Range.exponentialBounded ())
            |> Gen.tuple
            |> GenX.distinct2
        [x1; x2] |> List.distinct =! [x1; x2]
    }

[<Fact>]
let ``distinct3 generates 3 non-equal elements`` () =
    Property.check <| property {
        let! x1, x2, x3 =
            Gen.int (Range.exponentialBounded ())
            |> Gen.tuple3
            |> GenX.distinct3
        [x1; x2; x3] |> List.distinct =! [x1; x2; x3]
    }

[<Fact>]
let ``distinct4 generates 4 non-equal elements`` () =
    Property.check <| property {
        let! x1, x2, x3, x4 =
            Gen.int (Range.exponentialBounded ())
            |> Gen.tuple4
            |> GenX.distinct4
        [x1; x2; x3; x4] |> List.distinct =! [x1; x2; x3; x4]
    }

[<Fact>]
let ``increasing2 generates a 2-tuple with strictly increasing elements`` () =
    Property.check <| property {
        let! x1, x2 =
            Gen.int (Range.exponentialBounded ())
            |> Gen.tuple
            |> GenX.increasing2
        x1 <! x2
    }

[<Fact>]
let ``increasing3 generates a 3-tuple with strictly increasing elements`` () =
    Property.check <| property {
        let! x1, x2, x3 =
            Gen.int (Range.exponentialBounded ())
            |> Gen.tuple3
            |> GenX.increasing3
        x1 <! x2
        x2 <! x3
    }

[<Fact>]
let ``increasing4 generates a 4-tuple with strictly increasing elements`` () =
    Property.check <| property {
        let! x1, x2, x3, x4 =
            Gen.int (Range.exponentialBounded ())
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
            Gen.int (Range.exponentialBounded ())
            |> Gen.list (Range.linear 1 50) 
            |> GenX.withMapTo Gen.alphaNum
        xs |> List.map f |> ignore // Should not throw.
    }

[<Fact>]
let ``withDistinctMapTo is defined for all elements in input list`` () =
    Property.check <| property {
        let! xs, f = 
            Gen.int (Range.exponentialBounded ())
            |> Gen.list (Range.linear 1 50) 
            |> GenX.withDistinctMapTo Gen.alphaNum
        xs |> List.map f |> ignore // Should not throw.
    }

[<Fact>]
let ``withDistinctMapTo guarantees that distinct input values map to distinct output values`` () =
    Property.check <| property {
        let! xs, f = 
            Gen.int (Range.exponentialBounded ())
            |> Gen.list (Range.linear 1 50) 
            |> GenX.withDistinctMapTo Gen.alphaNum
        let xsDistinct = xs |> List.distinct
        xsDistinct |> List.map f |> List.distinct |> List.length =! xsDistinct.Length
    }
