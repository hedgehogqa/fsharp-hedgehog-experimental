namespace Hedgehog

open System
open System.Reflection

type AutoGenConfig = internal {
  seqRange: Range<int> option
  recursionDepth: int option
  generators: GeneratorCollection
}

module AutoGenConfig =

  let private defaultSeqRange = Range.exponential 0 50
  let private defaultRecursionDepth = 1

  let defaults = {
    seqRange = Some defaultSeqRange
    recursionDepth = Some defaultRecursionDepth
    generators = GeneratorCollection.empty
  }

  let private mapGenerators f (config: AutoGenConfig) =
    { config with generators = f config.generators }

  let seqRange (config: AutoGenConfig) = config.seqRange |> Option.defaultValue defaultSeqRange
  let setSeqRange (range: Range<int>) (config: AutoGenConfig) =
    { config with seqRange = Some range }

  let recursionDepth (config: AutoGenConfig) = config.recursionDepth |> Option.defaultValue defaultRecursionDepth
  let setRecursionDepth (depth: int) (config: AutoGenConfig) =
    { config with recursionDepth = Some depth }

  /// Merge two configurations.
  /// Values from the second configuration take precedence when they are set.
  let merge (baseConfig: AutoGenConfig) (extraConfig: AutoGenConfig) =
    {
       seqRange = extraConfig.seqRange |> Option.orElse baseConfig.seqRange
       recursionDepth = extraConfig.recursionDepth |> Option.orElse baseConfig.recursionDepth
       generators = GeneratorCollection.merge baseConfig.generators extraConfig.generators
    }

  /// Add a generator to the configuration.
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
