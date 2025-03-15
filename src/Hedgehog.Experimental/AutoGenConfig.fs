namespace Hedgehog

open System
open System.Reflection

type AutoGenConfig internal (seqRange: Range<int> option, recursionDepth: int option, generators: GeneratorCollection) =
  let defaultSeqRange = Range.exponential 0 50
  let defaultRecursionDepth = 1

  member this.SeqRange = seqRange |> Option.defaultValue defaultSeqRange
  member this.RecursionDepth = recursionDepth |> Option.defaultValue defaultRecursionDepth
  member internal this.Generators = generators

  member this.WithSeqRange(range: Range<int>) = AutoGenConfig(Some range, recursionDepth, generators)
  member this.WithRecursionDepth(depth: int) = AutoGenConfig(seqRange, Some depth, generators)

  member internal this.IsSeqRangeDefined = Option.isSome seqRange
  member internal this.IsRecursionDepthDefined = Option.isSome recursionDepth

  member internal this.MapGenerators f = AutoGenConfig(seqRange, recursionDepth, f generators)

  member this.Merge(other: AutoGenConfig) =
    AutoGenConfig(
        (if other.IsSeqRangeDefined then Some other.SeqRange else seqRange),
        (if other.IsRecursionDepthDefined then Some other.RecursionDepth else recursionDepth),
        GeneratorCollection.merge this.Generators other.Generators
    )

module AutoGenConfig =

  let private mapGenerators f (config: AutoGenConfig) = config.MapGenerators f

  let addGenerator (gen: Gen<'a>) =
    mapGenerators (GeneratorCollection.map _.SetItem(typeof<'a>, ([||], fun _ _ -> gen)))

  /// Add generators from a given type.
  /// The type is expected to have static methods that return Gen<_>.
  /// These methods can have parameters which are required to be of type Gen<_>.
  let addGenerators<'a> (config: AutoGenConfig) =
      let isGen (t: Type) =
          t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<Gen<_>>

      let tryUnwrapGenParameters (methodInfo: MethodInfo) : Option<Type[]> =
          methodInfo.GetParameters()
          |> Array.fold (fun acc param ->
              match acc, isGen param.ParameterType with
              | Some types, true ->
                  Some (Array.append types [| param.ParameterType.GetGenericArguments().[0] |])
              | _ -> None
          ) (Some [||])

      typeof<'a>.GetMethods(BindingFlags.Static ||| BindingFlags.Public)
      |> Seq.choose (fun methodInfo ->
          match isGen methodInfo.ReturnType, tryUnwrapGenParameters methodInfo with
          | true, Some typeArray ->
              let targetType = methodInfo.ReturnType.GetGenericArguments().[0]
              let factory: Type[] -> obj[] -> obj = fun types gens ->
                  let methodToCall =
                      if Array.isEmpty types then methodInfo
                      else methodInfo.MakeGenericMethod(types)
                  methodToCall.Invoke(null, gens)
              Some (targetType, typeArray, factory)
          | _ -> None)
      |> Seq.fold (fun cfg (targetType, typeArray, factory) ->
          cfg |> mapGenerators (GeneratorCollection.addGenerator targetType typeArray factory))
          config
