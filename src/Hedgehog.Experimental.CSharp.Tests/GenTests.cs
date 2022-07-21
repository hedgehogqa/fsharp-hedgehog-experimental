using System.Collections.Generic;
using Xunit;
using FluentAssertions;

using static Hedgehog.Linq.Property;

namespace Hedgehog.Linq.Tests;

public record NameAge(string Name, int Age);

public class GenTests
{
  [Fact]
  public void ShouldUseGenConfig()
  {
    var onlyString = "singleton string";
    var config = GenX.defaults.WithGenerator(Gen.FromValue(onlyString));

    var list = GenX.autoWith<NameAge>(config).Sample(123, 3);

    _ = list.Should().HaveCount(3);
    _ = list.Should().AllSatisfy(x => x.Name.Should().Be(onlyString));
  }

  [Fact]
  public void ShouldGenerateWithNull()
  {
    var values = Gen.FromValue("a").WithNull().Sample(1, 1000);
    _ = values.Should().Contain(x => x == null);
  }

  [Fact]
  public void ShouldNotGenerateWithNull()
  {
    var values = Gen
      .FromValue("a")
      .WithNull()
      .WithoutNull()
      .Sample(1, 1000);
    _ = values.Should().NotContainNulls();
  }

  [Fact]
  public void ShouldAddElementToList()
  {
    var prop =
      from x in ForAll(Gen.Int32(Range.ExponentialBoundedInt32()))
      from xs in ForAll(Gen
        .Int32(Range.ExponentialBoundedInt32())
        .List(Range.LinearInt32(0, 10))
        .WithElement(x))
      select xs.Contains(x);

    prop.Check();
  }

  [Fact]
  public void ShouldSupportIEnumerable() =>
    GenX.auto<IEnumerable<int>>()
      .Sample(1, 5)
      .Should()
      .AllSatisfy(x => x.Should().NotBeNull());
}
