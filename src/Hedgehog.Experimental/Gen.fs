namespace Hedgehog

open System
open TypeShape.Core

[<Struct>]
type GeneratorCollection = private GeneratorCollection of Map<string, Gen<obj>>


module GeneratorCollection =

  let internal unwrap (GeneratorCollection map) = map
  let internal map f = unwrap >> f >> GeneratorCollection


[<CLIMutable>]
type AutoGenConfig = {
  SeqRange : Range<int>
  RecursionDepth: int
  Generators: GeneratorCollection
}


module AutoGenConfig =

  let private mapGenerators f config =
    { config with Generators = config.Generators |> f }

  let addGenerator (gen: Gen<'a>) =
    gen |> Gen.map box |> Map.add typeof<'a>.FullName |> GeneratorCollection.map |> mapGenerators


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
      Gen.int32 (Range.linear 0 255)
      |> Gen.list (Range.constant 4 4)
      |> Gen.map (List.map string >> String.concat ".")
    return!
      Gen.frequency [
        3, domainGen
        1, ipv4Gen
      ]
  }


  let private uriPort =
    Gen.int32 (Range.constant 1 65535)
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
    xs
    |> List.mapi (fun i _ -> Gen.integral (Range.constant 0 i))
    |> ListGen.sequence
    |> Gen.map (fun list ->
      let shuffled = Array.zeroCreate<'a>(xs.Length)
      list
      |> List.zip xs
      |> List.iteri (fun i (a, j) ->
        shuffled.[i] <- shuffled.[j]
        shuffled.[j] <- a)
      shuffled |> Array.toList)

  /// Shuffles the case of the given string.
  let shuffleCase (s: string) =
    Gen.bool
    |> List.replicate s.Length
    |> ListGen.sequence
    |> Gen.map (fun bs ->
      let sb = Text.StringBuilder ()
      bs
      |> List.iteri (fun i b ->
        let f = if b then Char.ToUpperInvariant else Char.ToLowerInvariant
        sb.Append (f s.[i]) |> ignore)
      sb.ToString())

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

  /// Generates a tuple of datetimes where dayRange determines the minimum
  /// and maximum number of days apart. Positive numbers means the datetimes
  /// will be in increasing order, and vice versa.
  let dateInterval (dayRange : Range<int>) : Gen<DateTime * DateTime> =
    gen {
      let! ticksApart =
        dayRange
        |> Range.map (fun days -> int64 days * TimeSpan.TicksPerDay)
        |> Gen.integral
      let dateTimeRange =
        Range.exponentialFrom
          (DateTime(2000, 1, 1)).Ticks
          DateTime.MinValue.Ticks
          DateTime.MaxValue.Ticks
        |> Range.map DateTime

      let! dt1 =
        dateTimeRange
        |> Gen.dateTime
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
    let dateTimeRange =
      Range.exponentialFrom
        (DateTime(2000, 1, 1)).Ticks
        DateTime.MinValue.Ticks
        DateTime.MaxValue.Ticks
      |> Range.map DateTime
    {
      SeqRange = Range.exponential 0 50
      RecursionDepth = 1
      Generators = GeneratorCollection Map.empty
    }
    |> AutoGenConfig.addGenerator (Gen.byte <| Range.exponentialBounded ())
    |> AutoGenConfig.addGenerator (Gen.int16 <| Range.exponentialBounded ())
    |> AutoGenConfig.addGenerator (Gen.uint16 <| Range.exponentialBounded ())
    |> AutoGenConfig.addGenerator (Gen.int32 <| Range.exponentialBounded ())
    |> AutoGenConfig.addGenerator (Gen.uint32 <| Range.exponentialBounded ())
    |> AutoGenConfig.addGenerator (Gen.int64 <| Range.exponentialBounded ())
    |> AutoGenConfig.addGenerator (Gen.uint64 <| Range.exponentialBounded ())
    |> AutoGenConfig.addGenerator (Gen.double (Range.exponentialFrom 0. (float Single.MinValue) (float Single.MaxValue)) |> Gen.map single)
    |> AutoGenConfig.addGenerator (Gen.double <| Range.exponentialBounded ())
    |> AutoGenConfig.addGenerator (Gen.double (Range.exponentialFrom 0. (float Decimal.MinValue) (float Decimal.MaxValue)) |> Gen.map decimal)
    |> AutoGenConfig.addGenerator Gen.bool
    |> AutoGenConfig.addGenerator Gen.guid
    |> AutoGenConfig.addGenerator Gen.latin1
    |> AutoGenConfig.addGenerator (Gen.string (Range.linear 0 50) Gen.latin1)
    |> AutoGenConfig.addGenerator (Gen.dateTime dateTimeRange)
    |> AutoGenConfig.addGenerator (Gen.dateTime dateTimeRange |> Gen.map DateTimeOffset)
    |> AutoGenConfig.addGenerator uri

  module internal MultidimensionalArray =

    let createWithDefaultEntries<'a> (lengths: int list) =
      let array = lengths |> Array.ofList
      Array.CreateInstance (typeof<'a>, array)

    let createWithGivenEntries<'a> (data: 'a seq) lengths =
      let array = createWithDefaultEntries<'a> lengths
      let currentIndices = Array.create (List.length lengths) 0
      use en = data.GetEnumerator ()
      let rec loop currentDimensionIndex = function
        | [] ->
            en.MoveNext () |> ignore
            array.SetValue(en.Current, currentIndices)
        | currentLength :: remainingLengths ->
            for i in 0..currentLength - 1 do
              currentIndices.[currentDimensionIndex] <- i
              loop (currentDimensionIndex + 1) remainingLengths
      loop 0 lengths
      array

  module internal InternalGen =
    let list<'a> canRecurse autoInner config incrementRecursionDepth =
      if canRecurse typeof<'a> then
        autoInner config (incrementRecursionDepth typeof<'a>) |> Gen.list config.SeqRange
      else
        Gen.constant ([]: 'a list)

  let rec private autoInner<'a> (config : AutoGenConfig) (recursionDepths: Map<string, int>) : Gen<'a> =

    let addGenMsg = "You can use 'GenX.defaults |> AutoGenConfig.addGenerator myGen |> GenX.autoWith' to generate types not inherently supported by GenX.auto."
    let unsupportedTypeException = NotSupportedException (sprintf "Unable to auto-generate %s. %s" typeof<'a>.FullName addGenMsg)

    let genPoco (shape: ShapePoco<'a>) =
      let bestCtor =
        shape.Constructors
        |> Seq.filter (fun c -> c.IsPublic)
        |> Seq.sortBy (fun c -> c.Arity)
        |> Seq.tryHead

      match bestCtor with
      | None -> failwithf "Class %O lacks a public constructor" typeof<'a>
      | Some ctor ->
        ctor.Accept {
        new IConstructorVisitor<'a, Gen<(unit -> 'a)>> with
          member __.Visit<'CtorParams> (ctor : ShapeConstructor<'a, 'CtorParams>) =
            autoInner config recursionDepths
            |> Gen.map (fun args ->
                let delayedCtor () =
                  try
                    ctor.Invoke args
                  with
                    | ex ->
                      ArgumentException(sprintf "Cannot construct %O with the generated argument(s): %O. %s" typeof<'a> args addGenMsg, ex)
                      |> raise
                delayedCtor
            )
        }

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

    let memberSetterGenerator (shape: IShapeMember<'DeclaringType>) =
      shape.Accept {
        new IMemberVisitor<'DeclaringType, Gen<'DeclaringType -> 'DeclaringType>> with
        member _.Visit(shape: ShapeMember<'DeclaringType, 'MemberType>) =
          autoInner<'MemberType> config recursionDepths
          |> Gen.map (fun mtValue -> fun dt ->
            try
              shape.Set dt mtValue
            with
              | ex ->
                ArgumentException(sprintf "Cannot set the %s property of %O to the generated value of %O. %s" shape.Label dt mtValue addGenMsg, ex)
                |> raise
          )
      }

    match config.Generators |> GeneratorCollection.unwrap |> Map.tryFind typeof<'a>.FullName with
    | Some gen -> gen |> Gen.map unbox<'a>
    | None ->

        let typeShape = TypeShape.Create<'a> ()
        match typeShape with

        | Shape.Unit -> wrap <| Gen.constant ()

        | Shape.FSharpOption s ->
            s.Element.Accept {
              new ITypeVisitor<Gen<'a>> with
              member __.Visit<'a> () =
                if canRecurse typeof<'a> then
                  autoInner<'a> config (incrementRecursionDepth typeof<'a>) |> Gen.option |> wrap
                else
                  Gen.constant (None: 'a option) |> wrap}

        | Shape.Array s ->
            s.Element.Accept {
              new ITypeVisitor<Gen<'a>> with
              member __.Visit<'a> () =
                if canRecurse typeof<'a> then
                  gen {
                    let! lengths =
                      config.SeqRange
                      |> Gen.integral
                      |> List.replicate s.Rank
                      |> ListGen.sequence
                    let elementCount = lengths |> List.fold (*) 1
                    let! data =
                      autoInner<'a> config (incrementRecursionDepth typeof<'a>)
                      |> Gen.list (Range.singleton elementCount)
                    return MultidimensionalArray.createWithGivenEntries<'a> data lengths |> unbox
                  }
                else
                  0
                  |> List.replicate s.Rank
                  |> MultidimensionalArray.createWithDefaultEntries<'a>
                  |> unbox
                  |> Gen.constant }

        | Shape.FSharpList s ->
            s.Element.Accept {
              new ITypeVisitor<Gen<'a>> with
              member __.Visit<'a> () =
                InternalGen.list<'a> canRecurse autoInner config incrementRecursionDepth |> wrap }

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
            shape.Elements
            |> Seq.toList
            |> ListGen.traverse memberSetterGenerator
            |> Gen.map (fun fs -> fs |> List.fold (|>) (shape.CreateUninitialized ()))

        | Shape.FSharpRecord (:? ShapeFSharpRecord<'a> as shape) ->
            shape.Fields
            |> Seq.toList
            |> ListGen.traverse memberSetterGenerator
            |> Gen.map (fun fs -> fs |> List.fold (|>) (shape.CreateUninitialized ()))

        | Shape.FSharpUnion (:? ShapeFSharpUnion<'a> as shape) ->
            let cases =
              shape.UnionCases
              |> Array.map (fun uc ->
                 uc.Fields
                 |> Seq.toList
                 |> ListGen.traverse memberSetterGenerator)
            gen {
              let! caseIdx = Gen.integral <| Range.constant 0 (cases.Length - 1)
              let! fs = cases.[caseIdx]
              return fs |> List.fold (|>) (shape.UnionCases.[caseIdx].CreateUninitialized ())
            }

        | Shape.Enum _ ->
            let values = Enum.GetValues(typeof<'a>)
            gen {
              let! index = Gen.integral <| Range.constant 0 (values.Length - 1)
              return values.GetValue index |> unbox
            }

        | Shape.Nullable s ->
            s.Accept {
              new INullableVisitor<Gen<'a>> with
                member __.Visit<'a when 'a : (new : unit -> 'a) and 'a :> ValueType and 'a : struct> () =
                  if canRecurse typeof<'a> then
                    autoInner<'a> config (incrementRecursionDepth typeof<'a>)
                    |> Gen.option
                    |> Gen.map Option.toNullable
                    |> wrap
                  else
                    Nullable () |> unbox |> Gen.constant }

        | Shape.Collection s ->
            s.Accept {
              new ICollectionVisitor<Gen<'a>> with
              member _.Visit<'collection, 'element when 'collection :> System.Collections.Generic.ICollection<'element>> () =
                match typeShape with
                | Shape.Poco (:? ShapePoco<'a> as shape) ->
                  gen {
                    let! collectionCtor = genPoco shape
                    let! elements = InternalGen.list canRecurse autoInner config incrementRecursionDepth
                    let collection = collectionCtor () |> unbox<System.Collections.Generic.ICollection<'element>>
                    for e in elements do
                      collection.Add e
                    return collection |> unbox<'a>
                  }
                | _ -> raise unsupportedTypeException
              }

        | Shape.CliMutable (:? ShapeCliMutable<'a> as shape) ->
            shape.Properties
            |> Seq.toList
            |> ListGen.traverse memberSetterGenerator
            |> Gen.map (fun fs -> fs |> List.fold (|>) (shape.CreateUninitialized ()))

        | Shape.Poco (:? ShapePoco<'a> as shape) -> genPoco shape |> Gen.map (fun x -> x ())

        | _ -> raise unsupportedTypeException

  let auto<'a> = autoInner<'a> defaults Map.empty

  let autoWith<'a> config = autoInner<'a> config Map.empty
