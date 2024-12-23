using System.Linq;
using System;
using Xunit;
using static Hedgehog.Linq.Property;

namespace Hedgehog.Linq.Tests;

public sealed record Uuid(Guid Value);

public sealed record Name(string Value);

public sealed record Id<T>(Guid Value);

public abstract record Either<TLeft, TRight>
{
  public sealed record Left(TLeft Value) : Either<TLeft, TRight>;

  public sealed record Right(TRight Value) : Either<TLeft, TRight>;
}

public abstract record Maybe<T>
{
  public sealed record Just(T Value) : Maybe<T>;

  public sealed record Nothing : Maybe<T>;
}

public sealed record OuterRecord(Maybe<Guid> Value);

public sealed class OuterClass
{
  public OuterClass(Maybe<Guid> value) => Value = value;
  public Maybe<Guid> Value { get; set; }
}

public sealed class GenericTestGenerators
{
  public static Gen<Guid> Guid() =>
    Gen.Byte(Range.ConstantBoundedByte())
      .Array(Range.FromValue(12))
      .Select(bytes => new byte[4].Concat(bytes).ToArray())
      .Select(bytes => new Guid(bytes));

  public static Gen<Id<T>> IdGen<T>(Gen<Guid> gen) =>
    gen.Select(value => new Id<T>(value));

  public static Gen<Uuid> UuidGen() =>
    Guid().Select(value => new Uuid(value));

  public static Gen<Name> NameGen(Gen<string> gen) =>
    gen.Select(value => new Name("Name: " + value));

  public static Gen<Maybe<T>> AlwaysJust<T>(Gen<T> gen) =>
    gen.Select(Maybe<T> (value) => new Maybe<T>.Just(value));

  public static Gen<Either<TLeft, TRight>> AlwaysLeft<TLeft, TRight>(Gen<TRight> genB, Gen<TLeft> genA) =>
    genA.Select(Either<TLeft, TRight> (value) => new Either<TLeft, TRight>.Left(value));
}

public class GenericGenTests
{
  private static bool IsCustomGuid(Guid guid) =>
    new Span<byte>(guid.ToByteArray(), 0, 4).ToArray().All(b => b == 0);

  [Fact]
  public void ShouldGenerateValueWithPhantomGenericType_Id()
  {
    var config = GenX.defaults.WithGenerators<GenericTestGenerators>();
    var prop = from x in ForAll(GenX.autoWith<Id<string>>(config))
               select IsCustomGuid(x.Value);

    prop.Check();
  }

  [Fact]
  public void ShouldGenerateGenericValueForUnionType_Either()
  {
    var config = GenX.defaults.WithGenerators<GenericTestGenerators>();
    var prop = from x in ForAll(GenX.autoWith<Either<int, string>>(config))
               select x is Either<int, string>.Left;
    prop.Check();
  }

  [Fact]
  public void ShouldGenerateGenericValueForUnionType_Maybe()
  {
    var config = GenX.defaults.WithGenerators<GenericTestGenerators>();
    var prop = from x in ForAll(GenX.autoWith<Maybe<string>>(config))
               select x is Maybe<string>.Just;
    prop.Check();
  }

  [Fact]
  public void ShouldGenerateValueUsingGeneratorWithoutParameters_Uuid()
  {
    var config = GenX.defaults.WithGenerators<GenericTestGenerators>();
    var prop = from x in ForAll(GenX.autoWith<Uuid>(config))
               select IsCustomGuid(x.Value);
    prop.Check();
  }

  [Fact]
  public void ShouldGenerateValueUsingGeneratorWithParameters_Name()
  {
    var config = GenX.defaults.WithGenerators<GenericTestGenerators>();
    var prop = from x in ForAll(GenX.autoWith<Name>(config))
               select x.Value.StartsWith("Name: ");
    prop.Check();
  }

  [Fact]
  public void ShouldGenerateOuterFSharpRecordWithGenericTypeInside()
  {
    var config = GenX.defaults.WithGenerators<GenericTestGenerators>();
    var prop = from x in ForAll(GenX.autoWith<OuterRecord>(config))
      select x.Value switch
      {
        Maybe<Guid>.Just(var v) => IsCustomGuid(v),
        Maybe<Guid>.Nothing => false,
        _ => throw new InvalidOperationException("C# cannot do exhaustive matching")
      };

    prop.Check();
  }

  [Fact]
  public void ShouldGenerateOuterClassWithGenericTypeInside()
  {
    var config = GenX.defaults.WithGenerators<GenericTestGenerators>();
    var prop = from x in ForAll(GenX.autoWith<OuterClass>(config))
      select x.Value switch
      {
        Maybe<Guid>.Just(var v) => IsCustomGuid(v),
        Maybe<Guid>.Nothing => false,
        _ => throw new InvalidOperationException("C# cannot do exhaustive matching")
      };
    prop.Check();
  }
}
