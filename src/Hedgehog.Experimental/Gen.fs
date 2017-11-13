module Hedgehog.Gen

    open Hedgehog.Gen

    /// Shortcut for Gen.list (Range.exponential lower upper).
    let eList (lower : int) (upper : int) : (Gen<'a> -> Gen<List<'a>>) =
        list (Range.exponential lower upper)

    /// Shortcut for Gen.list (Range.linear lower upper).
    let lList (lower : int) (upper : int) : (Gen<'a> -> Gen<List<'a>>) =
        list (Range.linear lower upper)

    /// Shortcut for Gen.list (Range.constant lower upper).
    let cList (lower : int) (upper : int) : (Gen<'a> -> Gen<List<'a>>) =
        list (Range.constant lower upper)

    /// Shortcut for Gen.string (Range.exponential lower upper).
    let eString (lower : int) (upper : int) : (Gen<char> -> Gen<string>) =
        string (Range.exponential lower upper)

    /// Shortcut for Gen.string (Range.linear lower upper).
    let lString (lower : int) (upper : int) : (Gen<char> -> Gen<string>) =
        string (Range.linear lower upper)

    /// Shortcut for Gen.string (Range.constant lower upper).
    let cString (lower : int) (upper : int) : (Gen<char> -> Gen<string>) =
        string (Range.constant lower upper)

    /// Generates a string that is not equal to another string using
    /// StringComparison.OrdinalIgnoreCase.
    let iNotEqualTo (str : string) : (Gen<string> -> Gen<string>) =
        filter <| fun s ->
            not <| str.Equals(s, System.StringComparison.OrdinalIgnoreCase)

    /// Generates a string that is not a substring of another string.
    let notSubstringOf (str : string) : (Gen<string> -> Gen<string>) =
      filter <| fun s -> not <| str.Contains s

    /// Generates a string that is not a substring of another string using
    /// StringComparison.OrdinalIgnoreCase.
    let iNotSubstringOf (str : string) : (Gen<string> -> Gen<string>) =
      filter <| fun s ->
          str.IndexOf(s, System.StringComparison.OrdinalIgnoreCase) = -1

    /// Generates a string that does not start with another string.
    let notStartsWith (str : string) : (Gen<string> -> Gen<string>) =
      filter <| fun s -> not <| s.StartsWith str

    /// Generates a string that does not start with another string using
    /// StringComparison.OrdinalIgnoreCase.
    let iNotStartsWith (str : string) : (Gen<string> -> Gen<string>) =
      filter <| fun s ->
        not <| s.StartsWith(str, System.StringComparison.OrdinalIgnoreCase)

    /// Generates null part of the time.
    let withNull (g : Gen<'a>) : Gen<'a> =
        g |> option |> map (fun xOpt ->
            match xOpt with Some x -> x | None -> null)

    /// Generates a value that is not null.
    let noNull (g : Gen<'a>) : Gen<'a> =
        g |> filter (not << isNull)

    /// Generates a value that is not equal to another value.
    let notEqualTo (other : 'a) : (Gen<'a> -> Gen<'a>) =
        filter ((<>) other)

    /// Generates a value that is not equal to another option-wrapped value.
    let notEqualToOpt (other : 'a option) : (Gen<'a> -> Gen<'a>) =
        filter (fun x -> match other with Some o -> x <> o | None -> true)

    /// Generates a value that is not contained in the specified list.
    let notIn (list: 'a list) (g : Gen<'a>) : Gen<'a> =
        g |> filter (fun x -> not <| List.contains x list)

    /// Generates a list that does not contain the specified element.
    /// Shortcut for Gen.filter (not << List.contains x)
    let notContains (x: 'a) : (Gen<'a list> -> Gen<'a list>) =
      filter (not << List.contains x)

    /// Inserts the given element at a random place in the list
    let addElement (x : 'a) (g : Gen<'a list>) : Gen<'a list> =
        gen {
          let! xs = g
          let! i = integral (Range.constant 0 xs.Length)
          let l1, l2 = xs |> List.splitAt i
          return List.concat [l1; [x]; l2]
        }

    /// Generates a 2-tuple with sorted elements.
    let sorted2 (g : Gen<'a * 'a>) : Gen<'a * 'a> =
        g |> map (fun (x1, x2) ->
            let l = [x1; x2] |> List.sort
            (l.Item 0, l.Item 1))

    /// Generates a 3-tuple with sorted elements.
    let sorted3 (g : Gen<'a * 'a * 'a>) : Gen<'a * 'a * 'a> =
        g |> map (fun (x1, x2, x3) ->
            let l = [x1; x2; x3] |> List.sort
            (l.Item 0, l.Item 1, l.Item 2))

    /// Generates a 4-tuple with sorted elements.
    let sorted4 (g : Gen<'a * 'a * 'a * 'a>) : Gen<'a * 'a * 'a * 'a> =
        g |> map (fun (x1, x2, x3, x4) ->
            let l = [x1; x2; x3; x4] |> List.sort
            (l.Item 0, l.Item 1, l.Item 2, l.Item 3))

    /// Generates a 2-tuple with distinct elements.
    let distinct2 (g : Gen<'a * 'a>) : Gen<'a * 'a> =
        g |> filter (fun (x1, x2) -> x1 <> x2)

    /// Generates a 3-tuple with distinct elements.
    let distinct3 (g : Gen<'a * 'a * 'a>) : Gen<'a * 'a * 'a> =
        g |> filter (fun (x1, x2, x3) ->
            [x1; x2; x3] |> List.distinct = [x1; x2; x3])

    /// Generates a 4-tuple with distinct elements.
    let distinct4 (g : Gen<'a * 'a * 'a * 'a>) : Gen<'a * 'a * 'a * 'a> =
        g |> filter (fun (x1, x2, x3, x4) ->
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
    let dateInterval (dayRange : Range<int>)
            : Gen<System.DateTime * System.DateTime> =
        gen {
            let tickRange =
                dayRange
                |> Range.map (fun days ->
                    Operators.int64 days * System.TimeSpan.TicksPerDay)
            let! ticksApart = integral tickRange
            let! dt1 = dateTime |> filter (fun dt ->
                dt.Ticks + ticksApart > System.DateTime.MinValue.Ticks
                && dt.Ticks + ticksApart < System.DateTime.MaxValue.Ticks)
            let dt2 = dt1.AddTicks ticksApart
            return dt1, dt2
        }
