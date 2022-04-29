using System.Collections.Generic;
using System.Linq;
using System.Text;
using BenchmarkDotNet.Attributes;
using BenchmarkDotNet.Running;
using Xunit;

namespace Scratchpad.Tests;

public class StringConcatenationAndAllocation
{
    [Fact]
    public void EnsureStringBuilderApproachReturnsSameSqlCommand()
    {
        var concatApproach = StringConcatApproach.Run();
        var builderApproach = StringBuilderApproach.Run();

        Assert.Equal(concatApproach, builderApproach);
    }

    [Fact]
    public void RunBenchmarks() => BenchmarkRunner.Run<Benchmarks>();

    [MemoryDiagnoser]
    public class Benchmarks
    {
        [Benchmark]
        public void RunStringConcatApproach() => StringConcatApproach.Run();

        [Benchmark]
        public void RunStringBuilderApproach() => StringBuilderApproach.Run();
    }

    private class StringConcatApproach
    {
        public static string Run()
        {
            var inputValue = MakeSafeSearchFilter("some ' search -- Value ;");
            inputValue = !string.IsNullOrEmpty(inputValue) ? "%" + inputValue + "%" : inputValue;

            var whereClause = BuildWhereClause(inputValue);

            if (IsCart())
            {
                var excludedCartNames = new List<string> { "some item" };
                if (excludedCartNames.Any())
                {
                    if (!string.IsNullOrEmpty(whereClause))
                    {
                        whereClause += " AND";
                    }

                    whereClause +=
                        $" OrderGroup.[Name] NOT IN ({string.Join(",", excludedCartNames.Select(x => $"'{x}'"))})";
                }
            }

            if (IsPurchaseOrder())
            {
                var metaClause = string.Empty;
                if (!string.IsNullOrEmpty(inputValue))
                {
                    var marketClause = BuildMarketClause();
                    var statusClause = BuildStatusClause();
                    if (!string.IsNullOrEmpty(whereClause))
                    {
                        whereClause += " OR";
                    }

                    metaClause = $" OrderGroup.[OrderGroupId] In (SELECT DISTINCT U.[Key]" +
                        $" FROM (SELECT Meta.ObjectId AS 'Key' FROM OrderGroup_PurchaseOrder Meta WHERE Meta.[TrackingNumber] LIKE N'{inputValue}') U)" +
                        $"{marketClause}" +
                        $"{statusClause}";
                }

                whereClause += metaClause;
            }

            return whereClause;
        }

        private static string MakeSafeSearchFilter(string searchValue)
        {
            var retVal = searchValue;
            if (!string.IsNullOrEmpty(retVal))
            {
                retVal = retVal.Trim();
                retVal = retVal.Replace("'", "''");
                retVal = retVal.Replace("--", "");
                retVal = retVal.Replace(";", "");
            }

            return retVal;
        }

        private static bool IsCart() => true;

        private static bool IsPurchaseOrder() => true;

        private static string BuildWhereClause(string searchValue)
        {
            var whereClause = string.Empty;

            if (!string.IsNullOrEmpty(searchValue))
            {
                whereClause =
                    $"(OrderGroup.[CustomerId] IN (SELECT [ContactId] FROM [cls_Contact] WHERE [Email] LIKE N'{searchValue}')" +
                    $" OR OrderGroup.[CustomerName] LIKE N'{searchValue}')";
            }

            var marketClause = BuildMarketClause();
            var statusClause = BuildStatusClause();

            whereClause += marketClause + statusClause;

            return whereClause;
        }

        private static string BuildMarketClause()
        {
            var marketClause = string.Empty;
            var marketIds = Enumerable.Range(1, 10).ToList();

            marketClause +=
                $" AND OrderGroup.[MarketId] IN({string.Join(",", marketIds.Select(x => $"'{x}'"))})";

            return marketClause;
        }

        private static string BuildStatusClause()
        {
            var statusClause = string.Empty;
            var statuses = Enumerable.Range(1, 10).ToList();

            statusClause +=
                $" AND OrderGroup.[Status] IN({string.Join(",", statuses.Select(x => $"'{x}'"))})";

            return statusClause;
        }
    }

    private class StringBuilderApproach
    {
        public static string Run()
        {
            var builder = new StringBuilder();

            var inputValue = MakeSafeSearchFilter("some ' search -- Value ;");

            if (!string.IsNullOrEmpty(inputValue))
            {
                inputValue = $"%{inputValue}%";
            }

            BuildWhereClause(inputValue, builder);

            if (IsCart())
            {
                var excludedCartNames = new List<string> { "some item" };
                if (excludedCartNames.Any())
                {
                    if (builder.Length > 1)
                    {
                        builder.Append(" AND");
                    }

                    builder.Append(
                        $" OrderGroup.[Name] NOT IN ({string.Join(",", excludedCartNames.Select(x => $"'{x}'"))})");
                }
            }

            if (IsPurchaseOrder())
            {
                if (!string.IsNullOrEmpty(inputValue))
                {
                    if (builder.Length > 1)
                    {
                        builder.Append(" OR");
                    }

                    builder.Append(
                        $" OrderGroup.[OrderGroupId] In (SELECT DISTINCT U.[Key] FROM (SELECT Meta.ObjectId AS 'Key' FROM OrderGroup_PurchaseOrder Meta WHERE Meta.[TrackingNumber] LIKE N'{inputValue}') U)");
                    BuildMarketClause(builder);

                    BuildStatusClause(builder);
                }
            }

            return builder.ToString();
        }

        private static string MakeSafeSearchFilter(string searchValue)
        {
            if (string.IsNullOrEmpty(searchValue))
            {
                return searchValue;
            }

            // Currently there is no Span<T>.Replace() alternative yet
            var retVal = searchValue.Trim();
            retVal = retVal.Replace("'", "''");
            retVal = retVal.Replace("--", "");

            return retVal.Replace(";", "");
        }

        private static bool IsCart() => true;

        private static bool IsPurchaseOrder() => true;

        private static void BuildWhereClause(string searchValue, StringBuilder builder)
        {
            if (!string.IsNullOrEmpty(searchValue))
            {
                builder.Append(
                    $"(OrderGroup.[CustomerId] IN (SELECT [ContactId] FROM [cls_Contact] WHERE [Email] LIKE N'{searchValue}') OR OrderGroup.[CustomerName] LIKE N'{searchValue}')");
            }

            BuildMarketClause(builder);
            BuildStatusClause(builder);
        }

        private static void BuildMarketClause(StringBuilder builder)
        {
            var marketIds = Enumerable.Range(1, 10).ToList();

            builder.Append($" AND OrderGroup.[MarketId] IN({string.Join(",", marketIds.Select(x => $"'{x}'"))})");
        }

        private static void BuildStatusClause(StringBuilder builder)
        {
            var statuses = Enumerable.Range(1, 10).ToList();

            builder.Append($" AND OrderGroup.[Status] IN({string.Join(",", statuses.Select(x => $"'{x}'"))})");
        }
    }
}
