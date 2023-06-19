using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using FizzWare.NBuilder;
using Xunit.Abstractions;

namespace Scratchpad.Tests;

public class WritePerformanceTest
{
    private readonly ITestOutputHelper _testOutputHelper;

    public WritePerformanceTest(ITestOutputHelper testOutputHelper)
    {
        _testOutputHelper = testOutputHelper;
    }

    [Fact]
    public void WhenWriting50kLines_ShouldExecuteUnderOneSecond()
    {
        // Arrange

        // Act
        var stopwatch = new Stopwatch();
        stopwatch.Start();
        File.WriteAllLines("output.txt", BuildContent(50_000));
        stopwatch.Stop();

        // Assert
        _testOutputHelper.WriteLine(stopwatch.Elapsed.ToString());
        Assert.True(stopwatch.Elapsed < TimeSpan.FromSeconds(1));
    }

    private static IEnumerable<string> BuildContent(int numberOfLines)
    {
        yield return "Id,OperatorCallId,FileName,WarningLevel,Emotion,AutoTag,Caller,StartTime,EndTime,Duration,Service,AgentId,AgentName,Provider,Comment";

        foreach (var item in Builder<CallSearchModel>.CreateListOfSize(numberOfLines).Build())
        {
            var builder = new StringBuilder();

            builder.Append($"{item.Code},");
            builder.Append($"{item.Metadata?.OperatorCallId},");
            builder.Append($"{item.FileName},");
            builder.Append($"{item.WarningLevel},");
            builder.Append($"{item.Emotion},");
            builder.Append($"{item.Voice?.Tag},");
            builder.Append($"{item.Metadata?.Caller},");
            builder.Append($"{item.Metadata?.StartTime:yyyy-MM-dd HH:mm:ss},");
            builder.Append(
                $"{(item.Metadata != null && item.Metadata.EndTime.HasValue ? item.Metadata.EndTime.Value.ToString("yyyy-MM-dd HH:mm:ss") : null)},");
            builder.Append($"{item.Metadata?.Duration ?? 0},");
            builder.Append($"{item.Metadata?.Hotline},");
            builder.Append($"{item.Metadata?.AgentId},");
            builder.Append($"{item.Metadata?.AgentName?.Replace(",", "")},");
            builder.Append($"{item.Metadata?.Provider?.Replace(",", "")},");
            builder.AppendLine(
                $"{(item.Comments != null ? string.Join(';', item.Comments?.Select(x => x.Content.Replace(",", " "))) : null)}");

            yield return builder.ToString();
        }
    }

    private class CallSearchModel
    {
        public string Code { get; set; }
        public Metadata Metadata { get; set; }
        public string FileName { get; set; }
        public int WarningLevel { get; set; }
        public string Emotion { get; set; }
        public Voice Voice { get; set; }
        public IEnumerable<Comment> Comments { get; set; }
    }

    private class Comment
    {
        public string Content { get; set; }
    }

    private class Voice
    {
        public string Tag { get; set; }
    }

    private class Metadata
    {
        public int OperatorCallId { get; set; }
        public string Caller { get; set; }
        public DateTime StartTime { get; set; }
        public DateTime? EndTime { get; set; }
        public long Duration { get; set; }
        public string Hotline { get; set; }
        public int AgentId { get; set; }
        public string AgentName { get; set; }
        public string Provider { get; set; }
    }
}