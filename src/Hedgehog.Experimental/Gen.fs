namespace Hedgehog

open System

module Gen =
    /// Shortcut for Gen.list (Range.exponential lower upper).
    let eList (lower : int) (upper : int) : (Gen<'a> -> Gen<List<'a>>) =
        Gen.list (Range.exponential lower upper)

    /// Shortcut for Gen.list (Range.linear lower upper).
    let lList (lower : int) (upper : int) : (Gen<'a> -> Gen<List<'a>>) =
        Gen.list (Range.linear lower upper)

    /// Shortcut for Gen.list (Range.constant lower upper).
    let cList (lower : int) (upper : int) : (Gen<'a> -> Gen<List<'a>>) =
        Gen.list (Range.constant lower upper)

    /// Shortcut for Gen.string (Range.exponential lower upper).
    let eString (lower : int) (upper : int) : (Gen<char> -> Gen<string>) =
        Gen.string (Range.exponential lower upper)

    /// Shortcut for Gen.string (Range.linear lower upper).
    let lString (lower : int) (upper : int) : (Gen<char> -> Gen<string>) =
        Gen.string (Range.linear lower upper)

    /// Shortcut for Gen.string (Range.constant lower upper).
    let cString (lower : int) (upper : int) : (Gen<char> -> Gen<string>) =
        Gen.string (Range.constant lower upper)

    /// Generates null part of the time.
    let withNull (g : Gen<'a>) : Gen<'a> =
        g |> Gen.option |> Gen.map (fun xOpt ->
            match xOpt with Some x -> x | None -> null)

    /// Generates a value that is not null.
    let noNull (g : Gen<'a>) : Gen<'a> =
        g |> Gen.filter (not << isNull)

    /// Generates a value that is not equal to another value.
    let notEqualTo (other : 'a) : (Gen<'a> -> Gen<'a>) =
        Gen.filter ((<>) other)

    /// Generates a value that is not equal to another option-wrapped value.
    let notEqualToOpt (other : 'a option) : (Gen<'a> -> Gen<'a>) =
        Gen.filter (fun x -> match other with Some o -> x <> o | None -> true)

    /// Generates a value that is not contained in the specified list.
    let notIn (list: 'a list) (g : Gen<'a>) : Gen<'a> =
        g |> Gen.filter (fun x -> not <| List.contains x list)

    /// Generates a list that does not contain the specified element.
    /// Shortcut for Gen.filter (not << List.contains x)
    let notContains (x: 'a) : (Gen<'a list> -> Gen<'a list>) =
      Gen.filter (not << List.contains x)

    /// Inserts the given element at a random place in the list
    let addElement (x : 'a) (g : Gen<'a list>) : Gen<'a list> =
        gen {
            let! xs = g
            let! i = Gen.integral (Range.constant 0 xs.Length)
            let l1, l2 = xs |> List.splitAt i
            return List.concat [l1; [x]; l2]
        }

    /// Generates a 2-tuple with sorted elements.
    let sorted2 (g : Gen<'a * 'a>) : Gen<'a * 'a> =
        g |> Gen.map (fun (x1, x2) ->
            let l = [x1; x2] |> List.sort
            (l.Item 0, l.Item 1))

    /// Generates a 3-tuple with sorted elements.
    let sorted3 (g : Gen<'a * 'a * 'a>) : Gen<'a * 'a * 'a> =
        g |> Gen.map (fun (x1, x2, x3) ->
            let l = [x1; x2; x3] |> List.sort
            (l.Item 0, l.Item 1, l.Item 2))

    /// Generates a 4-tuple with sorted elements.
    let sorted4 (g : Gen<'a * 'a * 'a * 'a>) : Gen<'a * 'a * 'a * 'a> =
        g |> Gen.map (fun (x1, x2, x3, x4) ->
            let l = [x1; x2; x3; x4] |> List.sort
            (l.Item 0, l.Item 1, l.Item 2, l.Item 3))

    /// Generates a 2-tuple with distinct elements.
    let distinct2 (g : Gen<'a * 'a>) : Gen<'a * 'a> =
        g |> Gen.filter (fun (x1, x2) -> x1 <> x2)

    /// Generates a 3-tuple with distinct elements.
    let distinct3 (g : Gen<'a * 'a * 'a>) : Gen<'a * 'a * 'a> =
        g |> Gen.filter (fun (x1, x2, x3) ->
            [x1; x2; x3] |> List.distinct = [x1; x2; x3])

    /// Generates a 4-tuple with distinct elements.
    let distinct4 (g : Gen<'a * 'a * 'a * 'a>) : Gen<'a * 'a * 'a * 'a> =
        g |> Gen.filter (fun (x1, x2, x3, x4) ->
            [x1; x2; x3; x4] |> List.distinct = [x1; x2; x3; x4])

    /// Generates a 2-tuple with strictly increasing elements.
    let increasing2 (g : Gen<'a * 'a>) : Gen<'a * 'a> =
        g |> sorted2 |> distinct2

    /// Generates a 3-tuple with strictly increasing elements.
    let increasing3 (g : Gen<'a * 'a * 'a>) : Gen<'a * 'a * 'a> =
        g |> sorted3 |> distinct3

    /// Generates a 4-tuple with strictly increasing elements.
    let increasing4 (g : Gen<'a * 'a * 'a * 'a>) : Gen<'a * 'a * 'a * 'a> =
        g |> sorted4 |> distinct4

    /// Generates a tuple of datetimes where the range determines the minimum
    /// and maximum number of days apart. Positive numbers means the datetimes
    /// will be in increasing order, and vice versa.
    let dateInterval (dayRange : Range<int>) : Gen<DateTime * DateTime> =
        gen {
            let! ticksApart =
                dayRange
                |> Range.map
                    (fun days ->
                        Operators.int64 days * TimeSpan.TicksPerDay)
                |> Gen.integral

            let! dt1 =
                Gen.dateTime
                |> Gen.filter
                    (fun dt ->
                        dt.Ticks + ticksApart > DateTime.MinValue.Ticks
                     && dt.Ticks + ticksApart < DateTime.MaxValue.Ticks)
            let dt2 =
                dt1.AddTicks ticksApart

            return dt1, dt2
        }

    /// Generates a list using inpGen together with a function that maps each
    /// of the distinct elements in the list to values generated by outGen.
    /// Distinct elements in the input list may map to the same output values.
    /// For example, [2; 3; 2] may map to ['A'; 'B'; 'A'] or ['A'; 'A'; 'A'],
    /// but never ['A'; 'B'; 'C']. The generated function throws if called with
    /// values not present in the input list.
    let withMapTo (outGen : Gen<'b>) (inpGen : Gen<'a list>)
            : Gen<'a list * ('a -> 'b)> =
        gen {
            let! inputs = inpGen
            let inputsDistinct = inputs |> List.distinct
            let! outputs = outGen |> Gen.list (Range.singleton inputsDistinct.Length)
            let inOutMap = List.zip inputsDistinct outputs |> Map.ofList
            return inputs, (fun x -> inOutMap.Item x)
        }

    /// Generates a list using inpGen together with a function that maps each
    /// of the distinct elements in the list to values generated by outGen.
    /// Distinct elements in the input list are guaranteed to map to distinct
    /// output values. For example, [2; 3; 2] may map to ['A'; 'B'; 'A'], but
    /// never ['A'; 'A'; 'A'] or ['A'; 'B'; 'C']. Only use this if the output
    /// space is large enough that the required number of distinct output values
    /// are likely to be generated. The generated function throws if called with
    /// values not present in the input list.
    let withDistinctMapTo (outGen : Gen<'b>) (inpGen : Gen<'a list>)
            : Gen<'a list * ('a -> 'b)> =
        gen {
            let rec distinctOutGen (xs : 'b list) (length : int) : Gen<'b list> =
                gen {
                    if xs.Length = length then return xs
                    else
                        let! x = outGen |> notIn xs
                        return! distinctOutGen (x::xs) length
                }

            let! inputs = inpGen
            let inputsDistinct = inputs |> List.distinct
            let! outputs = distinctOutGen [] inputsDistinct.Length
            let inOutMap = List.zip inputsDistinct outputs |> Map.ofList
            return inputs, (fun x -> inOutMap.Item x)
        }
