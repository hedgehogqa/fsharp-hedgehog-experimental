using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using static Hedgehog.Linq.Property;
using Xunit;

namespace Hedgehog.Linq.Tests;

public sealed class DefaultGeneratorsTests
{
    private AutoGenConfig _config = GenX.defaults.WithCollectionRange(Range.FromValue(5));

    [Fact]
    public void ShouldGenerateImmutableSet() =>
        ForAll(GenX.autoWith<ImmutableHashSet<int>>(_config)).Select(x => x.Count > 0).Check();

    [Fact]
    public void ShouldGenerateIImmutableSet() =>
        ForAll(GenX.autoWith<IImmutableSet<int>>(_config)).Select(x => x.Count > 0).Check();

    [Fact]
    public void ShouldGenerateImmutableSortedSet() =>
        ForAll(GenX.autoWith<ImmutableSortedSet<int>>(_config)).Select(x => x.Count > 0).Check();

    [Fact]
    public void ShouldGenerateImmutableList() =>
        ForAll(GenX.autoWith<ImmutableList<int>>(_config)).Select(x => x.Count == 5).Check();

    [Fact]
    public void ShouldGenerateIImmutableList() =>
        ForAll(GenX.autoWith<IImmutableList<int>>(_config)).Select(x => x.Count == 5).Check();

    [Fact]
    public void ShouldGenerateImmutableArray() =>
        ForAll(GenX.autoWith<ImmutableArray<int>>(_config)).Select(x => x.Length == 5).Check();

    [Fact]
    public void ShouldGenerateDictionary() =>
        ForAll(GenX.autoWith<Dictionary<int, string>>(_config)).Select(x => x.Count > 0).Check();

    [Fact]
    public void ShouldGenerateIDictionary() =>
        ForAll(GenX.autoWith<IDictionary<int, string>>(_config)).Select(x => x.Count > 0).Check();

    [Fact]
    public void ShouldGenerateList() =>
        ForAll(GenX.autoWith<List<int>>(_config)).Select(x => x.Count == 5).Check();

    // [Fact]
    // public void ShouldGenerateIList() =>
    //     ForAll(GenX.autoWith<IList<int>>(_config)).Select(x => x.Count == 5).Check();

    [Fact]
    public void StressTest() =>
        ForAll(GenX.autoWith<List<List<List<int>>>>(_config))
            .Select(x => x.Count == 5 && x.All(inner => inner.Count == 5 && inner.All(innerMost => innerMost.Count == 5)))
            .Check();

    [Fact]
    public void ShouldGenerateRecursiveTreeWithImmutableList()
    {
        // Tree node with ImmutableList of children - tests recursive generation with generic types
        var config = GenX.defaults
            .WithCollectionRange(Range.FromValue(2))
            .WithRecursionDepth(1);

        ForAll(GenX.autoWith<TreeNode<int>>(config))
            .Select(tree =>
            {
                // At depth 1, should have children
                // At depth 2, children's children should be empty (recursion limit)
                return tree.Children.Count == 2 &&
                       tree.Children.All(child => child.Children.Count == 0);
            })
            .Check();
    }
}

// Recursive data structure for testing
public record TreeNode<T>
{
    public T Value { get; init; }
    public List<TreeNode<T>> Children { get; init; } = [];

    public override string ToString()
    {
        if (Children.Count == 0)
            return $"Node({Value})";

        var childrenStr = string.Join(", ", Children.Select(c => c.ToString()));
        return $"Node({Value}, [{childrenStr}])";
    }
}
