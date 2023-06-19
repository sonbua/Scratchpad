using System.Collections.Generic;
using System.Linq;
using BenchmarkDotNet.Attributes;
using BenchmarkDotNet.Running;

namespace Scratchpad.Tests;

public class ForeachBenchmarkTest
{
    [Fact(Skip = "Benchmark")]
    public void RunBenchmark() => BenchmarkRunner.Run<ForeachOnArrayVsListVsEnumerable>();

    [MemoryDiagnoser]
    public class ForeachOnArrayVsListVsEnumerable
    {
        private readonly int[] _array;
        private readonly List<int> _list;
        private readonly IEnumerable<int> _enumerable;

        public ForeachOnArrayVsListVsEnumerable()
        {
            _array = Enumerable.Range(1, 10).ToArray();
            _list = _array.ToList();
            _enumerable = _list;
        }

        [Benchmark]
        public void ForeachOnArray()
        {
            foreach (var item in _array)
            {
            }
        }

        [Benchmark]
        public void ForeachOnList()
        {
            foreach (var item in _list)
            {
            }
        }

        [Benchmark]
        public void ForeachOnEnumerable()
        {
            foreach (var item in _enumerable)
            {
            }
        }
    }
}
