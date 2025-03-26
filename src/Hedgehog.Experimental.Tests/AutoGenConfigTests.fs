module Hedgehog.Experimental.Tests.AutoGenConfigTests

open Xunit
open Swensen.Unquote
open Hedgehog

[<Fact>]
let ``merging AutoGenConfig preserves set values``() =
  let expectedRange = Range.exponential 2 6
  let expectedDepth = 2
  let config1 =
    AutoGenConfig.defaults
    |> AutoGenConfig.setSeqRange expectedRange
    |> AutoGenConfig.setRecursionDepth expectedDepth
    |> AutoGenConfig.addGenerator (Gen.int32 (Range.exponentialBounded()))
  let config2 = AutoGenConfig.defaults |> AutoGenConfig.addGenerator Gen.bool
  let merged = AutoGenConfig.merge config1 config2
  test <@ AutoGenConfig.recursionDepth merged = expectedDepth @>

  let property = property {
    let! array = merged |> GenX.autoWith<(int * bool)[]>
    test <@ Array.length array >= 2 && Array.length array <= 6 @>
  }

  Property.check property

[<Fact>]
let ``merging AutoGenConfig overrides values``() =
  let previousRange = Range.exponential 10 15
  let expectedRange = Range.exponential 2 6
  let expectedDepth = 2
  let config1 = AutoGenConfig.defaults |> AutoGenConfig.setSeqRange previousRange |> AutoGenConfig.setRecursionDepth 1
  let config2 =
    AutoGenConfig.defaults
    |> AutoGenConfig.setSeqRange expectedRange
    |> AutoGenConfig.setRecursionDepth expectedDepth
    |> AutoGenConfig.addGenerator (Gen.int32 (Range.exponentialBounded()))

  let merged = AutoGenConfig.merge config1 config2
  test <@ AutoGenConfig.recursionDepth merged = expectedDepth @>

  let property = property {
    let! array = merged |> GenX.autoWith<int[]>
    test <@ Array.length array >= 2 && Array.length array <= 6 @>
  }

  Property.check property
