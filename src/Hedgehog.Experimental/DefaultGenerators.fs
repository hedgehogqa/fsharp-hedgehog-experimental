namespace Hedgehog.Experimental

open System.Collections.Generic
open System.Linq
open Hedgehog
open System
open System.Collections.Immutable

type DefaultGenerators =
    static member Byte() : Gen<byte> = Gen.byte <| Range.exponentialBounded ()
    static member Int16() : Gen<int16> = Gen.int16 <| Range.exponentialBounded ()
    static member UInt16() : Gen<uint16> = Gen.uint16 <| Range.exponentialBounded ()
    static member Int32() : Gen<int32> = Gen.int32 <| Range.exponentialBounded ()
    static member UInt32() : Gen<uint32> = Gen.uint32 <| Range.exponentialBounded ()
    static member Int64() : Gen<int64> = Gen.int64 <| Range.exponentialBounded ()
    static member UInt64() : Gen<uint64> = Gen.uint64 <| Range.exponentialBounded ()
    static member Single() : Gen<single> = Gen.double (Range.exponentialFrom 0. (float Single.MinValue) (float Single.MaxValue)) |> Gen.map single
    static member Double() : Gen<double> = Gen.double <| Range.exponentialBounded ()
    static member Decimal() : Gen<decimal> = Gen.double (Range.exponentialFrom 0. (float Decimal.MinValue) (float Decimal.MaxValue)) |> Gen.map decimal
    static member Bool() : Gen<bool> = Gen.bool
    static member Guid() : Gen<Guid> = Gen.guid
    static member Char() : Gen<char> = Gen.latin1
    static member String() : Gen<string> = Gen.string (Range.linear 0 50) Gen.latin1

    static member DateTime() : Gen<DateTime> = DefaultGenerators.DateTimeOffset() |> Gen.map _.DateTime
    static member DateTimeOffset() : Gen<DateTimeOffset> =
        let dateTimeRange =
            Range.exponentialFrom
                (DateTime(2000, 1, 1)).Ticks
                DateTime.MinValue.Ticks
                DateTime.MaxValue.Ticks
            |> Range.map DateTime
        Gen.dateTime dateTimeRange |> Gen.map DateTimeOffset

    static member ImmutableList<'a>(config: AutoGenConfig, recursionContext: RecursionContext, valueGen: Gen<'a>) : Gen<ImmutableList<'a>> =
        if recursionContext.CanRecurse then
            valueGen |> Gen.list (AutoGenConfig.seqRange config) |> Gen.map ImmutableList.CreateRange
        else
            Gen.constant (ImmutableList<'a>.Empty)

    static member IImmutableList<'a>(config: AutoGenConfig, recursionContext: RecursionContext, valueGen: Gen<'a>) : Gen<IImmutableList<'a>> =
        if recursionContext.CanRecurse then
            DefaultGenerators.ImmutableList(config, recursionContext, valueGen) |> Gen.map (fun x -> x :> IImmutableList<'a>)
        else
            Gen.constant (ImmutableList<'a>.Empty :> IImmutableList<'a>)

    static member ImmutableArray<'a>(config: AutoGenConfig, recursionContext: RecursionContext, valueGen: Gen<'a>) : Gen<ImmutableArray<'a>> =
        if recursionContext.CanRecurse then
            valueGen |> Gen.array (AutoGenConfig.seqRange config) |> Gen.map ImmutableArray.CreateRange
        else
            Gen.constant (ImmutableArray<'a>.Empty)

    static member ImmutableHashSet<'a when 'a : equality>(config: AutoGenConfig, recursionContext: RecursionContext, valueGen: Gen<'a>) : Gen<ImmutableHashSet<'a>> =
        if recursionContext.CanRecurse then
            valueGen |> Gen.list (AutoGenConfig.seqRange config) |> Gen.map ImmutableHashSet.CreateRange
        else
            Gen.constant (ImmutableHashSet<'a>.Empty)

    static member ImmutableSet<'a when 'a : comparison>(config: AutoGenConfig, recursionContext: RecursionContext, valueGen: Gen<'a>) : Gen<ImmutableSortedSet<'a>> =
        if recursionContext.CanRecurse then
            valueGen |> Gen.list (AutoGenConfig.seqRange config) |> Gen.map ImmutableSortedSet.CreateRange
        else
            Gen.constant (ImmutableSortedSet<'a>.Empty)

    static member IImmutableSet<'a when 'a : equality>(config: AutoGenConfig, recursionContext: RecursionContext, valueGen: Gen<'a>) : Gen<IImmutableSet<'a>> =
        if recursionContext.CanRecurse then
            DefaultGenerators.ImmutableHashSet(config, recursionContext, valueGen) |> Gen.map (fun x -> x :> IImmutableSet<'a>)
        else
            Gen.constant (ImmutableHashSet<'a>.Empty :> IImmutableSet<'a>)

    static member Dictionary<'k, 'v when 'k: equality>(config: AutoGenConfig, recursionContext: RecursionContext, keyGen: Gen<'k>, valueGen: Gen<'v>): Gen<Dictionary<'k, 'v>> =
        if recursionContext.CanRecurse then
            gen {
                let! kvps = Gen.zip keyGen valueGen |> Gen.list (AutoGenConfig.seqRange config)
                return Dictionary(dict kvps)
            }
        else
            Gen.constant (Dictionary<'k, 'v>())

    static member IDictionary<'k, 'v when 'k: equality>(config: AutoGenConfig, recursionContext: RecursionContext, keyGen: Gen<'k>, valueGen: Gen<'v>): Gen<IDictionary<'k, 'v>> =
        if recursionContext.CanRecurse then
            gen {
                let! kvps = Gen.zip keyGen valueGen |> Gen.list (AutoGenConfig.seqRange config)
                return Dictionary(dict kvps)
            }
        else
            Gen.constant (Dictionary<'k, 'v>() :> IDictionary<'k, 'v>)

    static member FSharpList<'a>(config: AutoGenConfig, recursionContext: RecursionContext, valueGen: Gen<'a>) : Gen<'a list> =
        if recursionContext.CanRecurse then
            valueGen |> Gen.list (AutoGenConfig.seqRange config)
        else
            Gen.constant []

    static member List<'a>(config: AutoGenConfig, recursionContext: RecursionContext, valueGen: Gen<'a>) : Gen<List<'a>> =
        DefaultGenerators.FSharpList(config, recursionContext, valueGen) |> Gen.map _.ToList()

    static member IList<'a>(config: AutoGenConfig, recursionContext: RecursionContext, valueGen: Gen<'a>) : Gen<IList<'a>> =
        DefaultGenerators.FSharpList(config, recursionContext, valueGen) |> Gen.map _.ToList()

    static member Seq<'a>(config: AutoGenConfig, recursionContext: RecursionContext, valueGen: Gen<'a>) : Gen<seq<'a>> =
        DefaultGenerators.FSharpList(config, recursionContext, valueGen) |> Gen.map Seq.ofList

    static member Option<'a>(recursionContext: RecursionContext, valueGen: Gen<'a>) : Gen<'a option> =
        if recursionContext.CanRecurse then Gen.option valueGen
        else Gen.constant None

    static member Nullable<'a when 'a : struct and 'a : (new : unit -> 'a) and 'a :> ValueType>(recursionContext: RecursionContext, valueGen: Gen<'a>): Gen<Nullable<'a>> =
        if recursionContext.CanRecurse
        then valueGen |> Gen.option |> Gen.map Option.toNullable
        else Gen.constant (Nullable<'a>())

    static member Set<'a when 'a : comparison>(config: AutoGenConfig, recursionContext: RecursionContext, valueGen: Gen<'a>) : Gen<Set<'a>> =
        if recursionContext.CanRecurse then
            valueGen |> Gen.list (AutoGenConfig.seqRange config) |> Gen.map Set.ofList
        else
            Gen.constant Set.empty

    static member Map<'k, 'v when 'k : comparison>(config: AutoGenConfig, recursionContext: RecursionContext, keyGen: Gen<'k>, valueGen: Gen<'v>) : Gen<Map<'k, 'v>> =
        if recursionContext.CanRecurse then
            gen {
                let! kvps = Gen.zip keyGen valueGen |> Gen.list (AutoGenConfig.seqRange config)
                return Map.ofList kvps
            }
        else
            Gen.constant Map.empty<'k, 'v>
