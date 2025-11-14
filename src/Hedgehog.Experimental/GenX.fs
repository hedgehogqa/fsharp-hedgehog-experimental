namespace Hedgehog

open System
open Hedgehog
open Hedgehog.Experimental
open TypeShape.Core

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
    AutoGenConfig.defaults
    |> AutoGenConfig.addGenerators<DefaultGenerators>
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
              currentIndices[currentDimensionIndex] <- i
              loop (currentDimensionIndex + 1) remainingLengths
      loop 0 lengths
      array

  let rec private autoInner<'a> (config : AutoGenConfig) (recursionDepths: Map<string, int>) : Gen<'a> =

    let addGenMsg = "You can use 'GenX.defaults |> AutoGenConfig.addGenerator myGen |> GenX.autoWith' to generate types not inherently supported by GenX.auto."
    let unsupportedTypeException = NotSupportedException (sprintf "Unable to auto-generate %s. %s" typeof<'a>.FullName addGenMsg)

    // Prevent auto-generating AutoGenConfig itself - it should only be passed as a parameter
    if typeof<'a> = typeof<AutoGenConfig> then
      raise (NotSupportedException "Cannot auto-generate AutoGenConfig type. It should be provided as a parameter to generator methods.")

    let currentTypeRecursionLevel =
      recursionDepths.TryFind typeof<'a>.AssemblyQualifiedName |> Option.defaultValue 0

    // Check if we can recurse for the current type
    // This tells the container generator whether it should generate elements or return empty
    let canRecurseForElements = currentTypeRecursionLevel < AutoGenConfig.recursionDepth config

    if currentTypeRecursionLevel > AutoGenConfig.recursionDepth config then
      Gen.delay (fun () ->
        raise (InvalidOperationException(
          sprintf "Recursion depth limit %d exceeded for type %s. " (AutoGenConfig.recursionDepth config) typeof<'a>.FullName +
          "To fix this, add a RecursionContext parameter to your generator method and use recursionContext.CanRecurse to control recursion.")))
    else

      // Increment recursion depth for this type before generating element types
      let newRecursionDepths = recursionDepths.Add(typeof<'a>.AssemblyQualifiedName, currentTypeRecursionLevel + 1)

      // Check recursion depth at the beginning
      let canRecurse = currentTypeRecursionLevel < AutoGenConfig.recursionDepth config


      let genPoco (shape: ShapePoco<'a>) =
        let bestCtor =
          shape.Constructors
          |> Seq.filter _.IsPublic
          |> Seq.sortBy _.Arity
          |> Seq.tryHead

        match bestCtor with
        | None -> failwithf "Class %O lacks a public constructor" typeof<'a>
        | Some ctor ->
          ctor.Accept {
          new IConstructorVisitor<'a, Gen<(unit -> 'a)>> with
            member __.Visit<'CtorParams> (ctor : ShapeConstructor<'a, 'CtorParams>) =
              autoInner config newRecursionDepths
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


      let wrap (t : Gen<'b>) = unbox<Gen<'a>> t

      let memberSetterGenerator (shape: IShapeMember<'DeclaringType>) =
        shape.Accept {
          new IMemberVisitor<'DeclaringType, Gen<'DeclaringType -> 'DeclaringType>> with
          member _.Visit(shape: ShapeMember<'DeclaringType, 'MemberType>) =
            autoInner<'MemberType> config newRecursionDepths
            |> Gen.map (fun mtValue -> fun dt ->
              try
                shape.Set dt mtValue
              with
                | ex ->
                  ArgumentException(sprintf "Cannot set the %s property of %O to the generated value of %O. %s" shape.Label dt mtValue addGenMsg, ex)
                  |> raise
            )
        }

      let typeShape = TypeShape.Create<'a> ()

      // Check if there is a registered generator factory for a given requested generator.
      // Fallback to the default heuristics if no factory is found.
      match config.generators |> GeneratorCollection.tryFindFor typeof<'a> with
      | Some (registeredType, (args, factory)) ->

        let factoryArgs =
          match typeShape with
          | GenericShape (_, typeArgs) ->
            // If the type is generic, we need to find the actual types to use.
            // We match generic parameters by their GenericParameterPosition property,
            // which tells us their position in the method's generic parameter declaration.

            // The registeredType contains the method's generic parameters as they appear in the return type.
            // For example:
            // - Id<'a> has 'a at position 0 in the type
            // - Or<'A, 'A> has 'A at positions 0 and 1 in the type (but GenericParameterPosition=0 for both)
            // - Foo<'A, 'A, 'B, 'C> has 'A at 0,1 (GenericParameterPosition=0), 'B at 2 (GenericParameterPosition=1), 'C at 3 (GenericParameterPosition=2)

            let registeredGenArgs =
                if registeredType.IsGenericType
                then registeredType.GetGenericArguments()
                else Array.empty

            // Build a mapping from method generic parameter position to concrete type
            // by finding where each method parameter first appears in the registered type
            let methodGenParamCount =
                registeredGenArgs
                |> Array.filter _.IsGenericParameter
                |> Array.map _.GenericParameterPosition
                |> Array.distinct
                |> Array.length

            let genericTypes = Array.zeroCreate methodGenParamCount

            // For each position in registeredType, if it's a generic parameter,
            // map it to the corresponding concrete type from typeArgs
            for i = 0 to registeredGenArgs.Length - 1 do
                let regArg = registeredGenArgs.[i]
                if regArg.IsGenericParameter then
                    let paramPosition = regArg.GenericParameterPosition
                    // Only set it if we haven't seen this parameter position before (use first occurrence)
                    if genericTypes[paramPosition] = null
                    then genericTypes[paramPosition] <- box typeArgs.[i].argType

            let genericTypes = genericTypes |> Array.map unbox<Type>

            // Build argumentTypes: substitute generic parameters with concrete types
            let argTypes =
                args
                |> Array.map (fun arg ->
                    if arg.IsGenericParameter then
                      // Find where this parameter first appears in the registered type
                      let paramPosition = arg.GenericParameterPosition
                      let firstOccurrenceIndex =
                          registeredGenArgs
                          |> Array.findIndex (fun t -> t.IsGenericParameter && t.GenericParameterPosition = paramPosition)
                      typeArgs[firstOccurrenceIndex].argType
                    else arg)

            {| genericTypes = genericTypes; argumentTypes = argTypes |}

          | _ -> {| genericTypes = Array.empty; argumentTypes = args |}

        // and if the factory takes parameters, recurse and find generators for them
        let targetArgs =
          factoryArgs.argumentTypes
          |> Array.map (fun t ->
            // Check if this is AutoGenConfig type
            if t = typeof<AutoGenConfig> then
              box config
            // Check if this is RecursionContext type
            elif t = typeof<RecursionContext> then
              box (RecursionContext(canRecurseForElements))
            else
              // Otherwise, generate a value for this type
              let ts = TypeShape.Create(t)
              ts.Accept { new ITypeVisitor<obj> with
                member __.Visit<'b> () = autoInner<'b> config newRecursionDepths |> box
              })

        let resGen = factory factoryArgs.genericTypes targetArgs
        resGen |> unbox<Gen<'a>>

      | None ->
          match typeShape with

          | Shape.Unit -> wrap <| Gen.constant ()

          | Shape.Array s ->
              s.Element.Accept {
                new ITypeVisitor<Gen<'a>> with
                member __.Visit<'a> () =
                  if canRecurse then
                    gen {
                      let! lengths =
                        config
                        |> AutoGenConfig.seqRange
                        |> Gen.integral
                        |> List.replicate s.Rank
                        |> ListGen.sequence
                      let elementCount = lengths |> List.fold (*) 1
                      let! data = autoInner<'a> config newRecursionDepths |> Gen.list (Range.singleton elementCount)
                      return MultidimensionalArray.createWithGivenEntries<'a> data lengths |> unbox
                    }
                  else
                    0
                    |> List.replicate s.Rank
                    |> MultidimensionalArray.createWithDefaultEntries<'a>
                    |> unbox
                    |> Gen.constant }

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
                let! fs = cases[caseIdx]
                return fs |> List.fold (|>) (shape.UnionCases[caseIdx].CreateUninitialized ())
              }

          | Shape.Enum _ ->
              let values = Enum.GetValues(typeof<'a>)
              gen {
                let! index = Gen.integral <| Range.constant 0 (values.Length - 1)
                return values.GetValue index |> unbox
              }

          | Shape.Collection s ->
              s.Accept {
                new ICollectionVisitor<Gen<'a>> with
                member _.Visit<'collection, 'element when 'collection :> System.Collections.Generic.ICollection<'element>> () =
                  match typeShape with
                  | Shape.Poco (:? ShapePoco<'a> as shape) ->
                    gen {
                      let! collectionCtor = genPoco shape
                      let! elements =
                        if canRecurse
                        then autoInner<'element> config newRecursionDepths |> Gen.list (AutoGenConfig.seqRange config)
                        else Gen.constant []
                      let collection = collectionCtor () |> unbox<System.Collections.Generic.ICollection<'element>>
                      for e in elements do collection.Add e
                      return collection |> unbox<'a>
                    }
                  | _ -> raise unsupportedTypeException
                }

          | Shape.CliMutable (:? ShapeCliMutable<'a> as shape) ->
              let getDepth (sm: IShapeMember<_>) =
                let rec loop (t: Type) depth =
                  if t = null
                  then depth
                  else loop t.BaseType (depth + 1)
                loop sm.MemberInfo.DeclaringType 0
              shape.Properties
              |> Array.toList
              |> List.groupBy _.MemberInfo.Name
              |> List.map (snd >> function
                                  | [p] -> p
                                  | ps -> ps |> List.sortByDescending getDepth |> List.head)
              |> ListGen.traverse memberSetterGenerator
              |> Gen.map (fun fs -> fs |> List.fold (|>) (shape.CreateUninitialized ()))

          | Shape.Poco (:? ShapePoco<'a> as shape) -> genPoco shape |> Gen.map (fun x -> x ())

          | _ -> raise unsupportedTypeException

  let auto<'a> = autoInner<'a> defaults Map.empty

  let autoWith<'a> config = autoInner<'a> config Map.empty
