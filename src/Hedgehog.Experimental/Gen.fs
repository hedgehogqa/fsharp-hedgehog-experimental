namespace Hedgehog

open TypeShape
open System

[<CLIMutable; Struct>]
type AutoGenConfig =
    { Byte : Gen<byte>
      Int16 : Gen<int16>
      Int : Gen<int>
      Int64 : Gen<int64>
      Double : Gen<double>
      Decimal : Gen<decimal>
      Bool : Gen<bool>
      Guid : Gen<System.Guid>
      Char : Gen<System.Char>
      String : Gen<System.String>
      DateTime : Gen<System.DateTime>
      DateTimeOffset : Gen<System.DateTimeOffset>
      SeqRange : Range<int> }

module GenX =
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

    let defaults =
        { Byte = Gen.byte <| Range.exponentialBounded ()
          Int16 = Gen.int16 <| Range.exponentialBounded ()
          Int = Gen.int <| Range.exponentialBounded ()
          Int64 = Gen.int64 <| Range.exponentialBounded ()
          Double = Gen.double <| Range.exponentialBounded ()
          Decimal = Gen.double <| Range.exponentialBounded () |> Gen.map decimal
          Bool = Gen.bool
          Guid = Gen.guid
          Char = Gen.latin1
          String = Gen.string (Range.linear 0 50) Gen.latin1
          DateTime = Gen.dateTime
          DateTimeOffset = Gen.dateTime |> Gen.map System.DateTimeOffset
          SeqRange = Range.exponential 0 50 }

    let rec auto'<'a> (config : AutoGenConfig) : Gen<'a> =
      let wrap (t : Gen<'b>) =
        unbox<Gen<'a>> t
  
      let mkRandomMember (shape : IShapeWriteMember<'DeclaringType>) = 
        shape.Accept {
          new IWriteMemberVisitor<'DeclaringType, Gen<'DeclaringType -> 'DeclaringType>> with
            member __.Visit(shape : ShapeWriteMember<'DeclaringType, 'Field>) = 
              let rf = auto'<'Field>(config)
              gen { let! f = rf
                    return fun dt -> shape.Inject dt f } }

      match TypeShape.Create<'a> () with
      | Shape.Byte -> wrap config.Byte
      | Shape.Int16 -> wrap config.Int16
      | Shape.Int32 -> wrap config.Int
      | Shape.Int64 -> wrap config.Int64
    
      | Shape.Double -> wrap config.Double
      | Shape.Decimal -> wrap config.Decimal
    
      | Shape.Bool -> wrap config.Bool
      | Shape.Guid -> wrap config.Guid
      | Shape.Char -> wrap config.Char
      | Shape.DateTime -> wrap config.DateTime

      | Shape.Unit -> wrap <| Gen.constant ()

      | Shape.String -> wrap config.String
      | Shape.DateTimeOffset -> wrap config.DateTimeOffset

      | Shape.FSharpOption s ->
        s.Accept {
          new IFSharpOptionVisitor<Gen<'a>> with
            member __.Visit<'a> () =
              auto'<'a> config |> Gen.option |> wrap }

      | Shape.Array s when s.Rank = 1 ->
        s.Accept { 
          new IArrayVisitor<Gen<'a>> with
            member __.Visit<'a> _ =
              auto'<'a> config |> Gen.array config.SeqRange |> wrap }

      | Shape.Array _ ->
        raise (System.NotSupportedException("Can only generate arrays of rank 1"))

      | Shape.FSharpList s ->
        s.Accept {
          new IFSharpListVisitor<Gen<'a>> with
            member __.Visit<'a> () =
              auto'<'a> config |> Gen.list config.SeqRange |> wrap }

      | Shape.FSharpSet s ->
        s.Accept {
          new IFSharpSetVisitor<Gen<'a>> with
            member __.Visit<'a when 'a : comparison> () =
              auto'<'a list> config
              |> Gen.map Set.ofList 
              |> wrap }

      | Shape.FSharpMap s ->
        s.Accept {
          new IFSharpMapVisitor<Gen<'a>> with
            member __.Visit<'k, 'v when 'k : comparison> () = 
              auto'<('k * 'v) list> config
              |> Gen.map Map.ofList
              |> wrap }

      | Shape.Tuple (:? ShapeTuple<'a> as shape) ->
        let eGens =
          shape.Elements
          |> Array.map mkRandomMember

        gen { 
          let mutable target = shape.CreateUninitialized ()
          for eg in eGens do
            let! u = eg
            target <- u target
          return target
        }

      | Shape.FSharpRecord (:? ShapeFSharpRecord<'a> as shape) ->
        let fieldGen =
          shape.Fields
          |> Array.map mkRandomMember

        gen { 
          let mutable target = shape.CreateUninitialized ()
          for eg in fieldGen do
            let! u = eg
            target <- u target
          return target
        }

      | Shape.FSharpUnion (:? ShapeFSharpUnion<'a> as shape) ->
        let caseFieldGen =
          shape.UnionCases
          |> Array.map (fun uc -> uc.Fields |> Array.map mkRandomMember)

        gen { 
          let! tag = Gen.integral <| Range.constant 0 (caseFieldGen.Length - 1)
          let mutable u = shape.UnionCases.[tag].CreateUninitialized ()
          for f in caseFieldGen.[tag] do
            let! uf = f
            u <- uf u
          return u
        }

      | Shape.CliMutable (:? ShapeCliMutable<'a> as shape) ->
        let propGen = shape.Properties |> Array.map mkRandomMember
        gen { 
          let mutable target = shape.CreateUninitialized ()
          for ep in propGen do
            let! up = ep
            target <- up target
          return target
        }

      | Shape.Poco (:? ShapePoco<'a> as shape) ->
        let bestCtor = 
          shape.Constructors
          |> Seq.filter  (fun c -> c.IsPublic)
          |> Seq.sortBy  (fun c -> c.Arity)
          |> Seq.tryHead

        match bestCtor with
        | None -> failwithf "Class %O lacking an appropriate ctor" typeof<'a>
        | Some ctor -> 
          ctor.Accept {
            new IConstructorVisitor<'a, Gen<'a>> with
              member __.Visit<'CtorParams> (ctor : ShapeConstructor<'a, 'CtorParams>) =
                let paramGen = auto'<'CtorParams> config
                gen { let! args = paramGen
                  return ctor.Invoke args } }

      | _ -> raise <| System.NotSupportedException ()

    let auto<'a> () = auto'<'a> defaults
