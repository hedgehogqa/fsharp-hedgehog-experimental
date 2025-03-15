namespace Hedgehog

open System
open System.Collections.Immutable

// A generator factory which can be backed by a generic method.
// It takes an array of genetic type parameters, and an array of arguments to create the generator.
type GeneratorFactory = Type[] -> obj[] -> obj

[<Struct>]
type GeneratorCollection =
  // A dictionary of generators.
  // The key is a 'required' generator type
  // The value is a tuple of:
  // 1. An array types of arguments for the generator factory
  // 2. A generator factory, which can be backed by a generic method,
  //    so it takes an array of genetic type parameters,
  //    and an array of arguments to create the generator.
  internal GeneratorCollection of ImmutableDictionary<Type, Type[] * GeneratorFactory>

module GeneratorCollection =

  let internal unwrap (GeneratorCollection map) = map
  let internal map f = unwrap >> f >> GeneratorCollection

  let internal merge (GeneratorCollection gens1) (GeneratorCollection gens2) =
    GeneratorCollection (gens1.SetItems(gens2))

  let internal addGenerator (targetType: Type) (paramTypes: Type[]) (factory: Type[] -> obj[] -> obj) =
        map _.SetItem(targetType, (paramTypes, factory))

  // Find a generator that can satisfy the given requited type.
  // It also takes care of finding 'generic' generators (like Either<'a, 'b>)
  // to satisfy specific types (like Either<int, string>).
  let internal tryFindFor (targetType: Type) =
    unwrap
    >> Seq.tryFind (fun (KeyValue (t, _)) -> t |> TypeUtils.satisfies targetType)
    >> Option.map (fun (KeyValue (_, v)) -> v)
