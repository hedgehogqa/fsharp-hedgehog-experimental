namespace Hedgehog

open System
open TypeShape.Core

[<CLIMutable; Struct>]
type AutoGenConfig = {
  Byte : Gen<byte>
  Int16 : Gen<int16>
  UInt16 : Gen<uint16>
  Int : Gen<int>
  UInt32 : Gen<uint32>
  Int64 : Gen<int64>
  UInt64 : Gen<uint64>
  Double : Gen<double>
  Decimal : Gen<decimal>
  Bool : Gen<bool>
  Guid : Gen<Guid>
  Char : Gen<Char>
  String : Gen<String>
  DateTime : Gen<DateTime>
  DateTimeOffset : Gen<DateTimeOffset>
  Uri : Gen<Uri>
  SeqRange : Range<int>
  RecursionDepth: int
}

module GenX =

  let private uriSchemeNonFirst = Gen.item (['a' .. 'z'] @ ['+'; '.'; '-'])

  let private uriScheme = gen {
    let! first = Gen.lower |> Gen.map string
    // It seems that length must be at least 2, becuase otherwise we might get
    // an implicit file:// scheme with the generated scheme as part of the path.
    let! rest = Gen.string (Range.linear 1 9) uriSchemeNonFirst
    return first + rest + ":"
  }


  let private uriUserInfo = gen {
    let! username = Gen.string (Range.linear 1 10) Gen.alphaNum
    let! password =
      Gen.frequency [
        5, Gen.constant None
        1, Gen.string (Range.linear 0 10) Gen.alphaNum |> Gen.map Some
      ]
    match password with
    | None -> return username + "@"
    | Some pwd -> return username + ":" + pwd + "@"
  }


  let private uriHost = gen {
    let domainGen =
      Gen.string (Range.linear 1 5) Gen.alpha
      |> Gen.list (Range.linear 1 5)
      |> Gen.map (String.concat ".")
    let ipv4Gen =
      Gen.int (Range.linear 0 255)
      |> Gen.list (Range.constant 4 4)
      |> Gen.map (List.map string >> String.concat ".")
    return!
      Gen.frequency [
        3, domainGen
        1, ipv4Gen
      ]
  }


  let private uriPort =
    Gen.int (Range.constant 1 65535)
    |> Gen.map (fun i -> ":" + string i)


  let private uriAuthority = gen {
    let! userinfo =
      Gen.frequency [
        3, Gen.constant None
        1, uriUserInfo |> Gen.map Some
      ]
    let! host = uriHost
    let! port =
      Gen.frequency [
        3, Gen.constant None
        1, uriPort |> Gen.map Some
      ]
    return "//" + (userinfo |> Option.defaultValue "") + host + (port |> Option.defaultValue "")
  }


  let private uriPath =
    Gen.string (Range.exponential 0 10) Gen.alphaNum
    |> Gen.list (Range.linear 0 5)
    |> Gen.map (String.concat "/")


  let private uriQuery =
    Gen.string (Range.exponential 1 10) Gen.alphaNum
    |> Gen.tuple
    |> Gen.list (Range.linear 1 5)
    |> Gen.map (
        List.map (fun (k, v) -> k + "=" + v)
        >> String.concat "&"
    )
    |> Gen.map (fun s -> "?" + s)


  let private uriFragment =
    Gen.string (Range.exponential 1 10) Gen.alphaNum
    |> Gen.map (fun s -> "#" + s)

  /// Generates a random URI.
  let uri = gen {
    let! scheme = uriScheme
    let! authority = uriAuthority
    let! path = uriPath
    let! query = uriQuery |> Gen.option
    let! fragment = uriFragment |> Gen.option
    let path = if path = "" then path else "/" + path
    return Uri(scheme + authority + path + (query |> Option.defaultValue "") + (fragment |> Option.defaultValue ""))
  }

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

  /// Generates a permutation of the given list.
  // "Inside-out" algorithm of Fisher-Yates shuffle from https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle#The_%22inside-out%22_algorithm
  let shuffle (xs: 'a list) =
    gen {
      let shuffled = Array.zeroCreate<'a>(xs.Length)
      for i = 0 to xs.Length - 1 do
        let! j = Gen.integral (Range.constant 0 i)
        if i <> j then shuffled.[i] <- shuffled.[j]
        shuffled.[j] <- xs.[i]
      return shuffled |> Array.toList
    }

  /// Shuffles the case of the given string.
  let shuffleCase (s: string) =
    gen {
     let sb = Text.StringBuilder()
     for i = 0 to s.Length - 1 do
       let! b = Gen.bool
       let f = if b then Char.ToUpperInvariant else Char.ToLowerInvariant
       sb.Append (f s.[i]) |> ignore
     return sb.ToString()
    }

  /// Generates a string that is not equal to another string using
  /// StringComparison.OrdinalIgnoreCase.
  let iNotEqualTo (str : string) : (Gen<string> -> Gen<string>) =
    Gen.filter <| fun s ->
      not <| str.Equals(s, StringComparison.OrdinalIgnoreCase)

  /// Generates a string that is not a substring of another string.
  let notSubstringOf (str : string) : (Gen<string> -> Gen<string>) =
   Gen.filter <| fun s -> not <| str.Contains s

  /// Generates a string that is not a substring of another string using
  /// StringComparison.OrdinalIgnoreCase.
  let iNotSubstringOf (str : string) : (Gen<string> -> Gen<string>) =
   Gen.filter <| fun s ->
     str.IndexOf(s, StringComparison.OrdinalIgnoreCase) = -1

  /// Generates a string that does not start with another string.
  let notStartsWith (str : string) : (Gen<string> -> Gen<string>) =
   Gen.filter <| fun s -> not <| s.StartsWith str

  /// Generates a string that does not start with another string using
  /// StringComparison.OrdinalIgnoreCase.
  let iNotStartsWith (str : string) : (Gen<string> -> Gen<string>) =
   Gen.filter <| fun s ->
    not <| s.StartsWith(str, StringComparison.OrdinalIgnoreCase)

  /// Generates null part of the time.
  let withNull (g : Gen<'a>) : Gen<'a> =
    g |> Gen.option |> Gen.map (Option.defaultValue null)

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
        |> Range.map (fun days -> int64 days * TimeSpan.TicksPerDay)
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

  let defaults = {
    Byte = Gen.byte <| Range.exponentialBounded ()
    Int16 = Gen.int16 <| Range.exponentialBounded ()
    UInt16 = Gen.uint16 <| Range.exponentialBounded ()
    Int = Gen.int <| Range.exponentialBounded ()
    UInt32 = Gen.uint32 <| Range.exponentialBounded ()
    Int64 = Gen.int64 <| Range.exponentialBounded ()
    UInt64 = Gen.uint64 <| Range.exponentialBounded ()
    Double = Gen.double <| Range.exponentialBounded ()
    Decimal = Gen.double <| Range.exponentialBounded () |> Gen.map decimal
    Bool = Gen.bool
    Guid = Gen.guid
    Char = Gen.latin1
    String = Gen.string (Range.linear 0 50) Gen.latin1
    DateTime = Gen.dateTime
    DateTimeOffset = Gen.dateTime |> Gen.map DateTimeOffset
    Uri = uri
    SeqRange = Range.exponential 0 50
    RecursionDepth = 1
  }

  let rec private autoInner<'a> (config : AutoGenConfig) (recursionDepths: Map<string, int>) : Gen<'a> =

    let canRecurse (t: Type) =
      match recursionDepths.TryFind t.AssemblyQualifiedName with
      | Some x -> config.RecursionDepth > x
      | None -> config.RecursionDepth > 0

    let incrementRecursionDepth (t: Type) =
      match recursionDepths.TryFind t.AssemblyQualifiedName with
      | Some x -> recursionDepths.Add(t.AssemblyQualifiedName, x+1)
      | None -> recursionDepths.Add(t.AssemblyQualifiedName, 1)

    let wrap (t : Gen<'b>) =
      unbox<Gen<'a>> t

    let mkRandomMember (shape : IShapeMember<'DeclaringType>) =
      shape.Accept {
        new IMemberVisitor<'DeclaringType, Gen<'DeclaringType -> 'DeclaringType>> with
        member __.Visit(shape : ShapeMember<'DeclaringType, 'Field>) =
          let rf = autoInner<'Field> config recursionDepths
          gen { let! f = rf
            return fun dt -> shape.Set dt f } }

    match TypeShape.Create<'a> () with
    | Shape.Byte -> wrap config.Byte
    | Shape.Int16 -> wrap config.Int16
    | Shape.UInt16 -> wrap config.UInt16
    | Shape.Int32 -> wrap config.Int
    | Shape.UInt32 -> wrap config.UInt32
    | Shape.Int64 -> wrap config.Int64
    | Shape.UInt64 -> wrap config.UInt64

    | Shape.Double -> wrap config.Double
    | Shape.Decimal -> wrap config.Decimal

    | Shape.Bool -> wrap config.Bool
    | Shape.Guid -> wrap config.Guid
    | Shape.Char -> wrap config.Char
    | Shape.DateTime -> wrap config.DateTime
    | Shape.Uri -> wrap config.Uri

    | Shape.Unit -> wrap <| Gen.constant ()

    | Shape.String -> wrap config.String
    | Shape.DateTimeOffset -> wrap config.DateTimeOffset

    | Shape.FSharpOption s ->
        s.Element.Accept {
          new ITypeVisitor<Gen<'a>> with
          member __.Visit<'a> () =
            if canRecurse typeof<'a> then
              autoInner<'a> config (incrementRecursionDepth typeof<'a>) |> Gen.option |> wrap
            else
              Gen.constant (None: 'a option) |> wrap}

    | Shape.Array s when s.Rank = 1 ->
        s.Element.Accept {
          new ITypeVisitor<Gen<'a>> with
          member __.Visit<'a> () =
            if canRecurse typeof<'a> then
              autoInner<'a> config (incrementRecursionDepth typeof<'a>) |> Gen.array config.SeqRange |> wrap
            else
              Gen.constant ([||]: 'a array) |> wrap}

    | Shape.Array _ ->
        raise (NotSupportedException("Can only generate arrays of rank 1"))

    | Shape.FSharpList s ->
        s.Element.Accept {
          new ITypeVisitor<Gen<'a>> with
          member __.Visit<'a> () =
            if canRecurse typeof<'a> then
              autoInner<'a> config (incrementRecursionDepth typeof<'a>) |> Gen.list config.SeqRange |> wrap
            else
              Gen.constant ([]: 'a list) |> wrap}

    | Shape.FSharpSet s ->
        s.Accept {
          new IFSharpSetVisitor<Gen<'a>> with
          member __.Visit<'a when 'a : comparison> () =
            autoInner<'a list> config recursionDepths
            |> Gen.map Set.ofList
            |> wrap}

    | Shape.FSharpMap s ->
        s.Accept {
          new IFSharpMapVisitor<Gen<'a>> with
          member __.Visit<'k, 'v when 'k : comparison> () =
            autoInner<('k * 'v) list> config recursionDepths
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

    | Shape.Enum _ ->
        let values = Enum.GetValues(typeof<'a>)
        gen {
          let! index = Gen.integral <| Range.constant 0 (values.Length - 1)
          return values.GetValue index |> unbox
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
          |> Seq.filter (fun c -> c.IsPublic)
          |> Seq.sortBy (fun c -> c.Arity)
          |> Seq.tryHead

        match bestCtor with
        | None -> failwithf "Class %O lacking an appropriate ctor" typeof<'a>
        | Some ctor ->
          ctor.Accept {
          new IConstructorVisitor<'a, Gen<'a>> with
            member __.Visit<'CtorParams> (ctor : ShapeConstructor<'a, 'CtorParams>) =
              let paramGen = autoInner<'CtorParams> config recursionDepths
              gen { 
                let! args = paramGen
                return ctor.Invoke args
              }
          }

    | _ -> raise <| NotSupportedException (sprintf "Unable to auto-generate %s" typeof<'a>.FullName)

  let auto<'a> = autoInner<'a> defaults Map.empty

  let autoWith<'a> config = autoInner<'a> config Map.empty
