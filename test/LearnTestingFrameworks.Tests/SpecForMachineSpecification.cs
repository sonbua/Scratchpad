using System.Collections.Generic;
using FluentAssertions;
using Machine.Specifications;

namespace LearnTestingFrameworks.Tests;

public class SpecForMachineSpecification
{
    Establish context = () => { collector.Add("arrange"); };

    Because of = () => { collector.Add("act"); };

    It should_follow_bdd_style = () =>
    {
        collector.Add("assert");

        collector.Should().BeEquivalentTo(new[] { "arrange", "act", "assert" }, o => o.WithStrictOrdering());
    };

    protected static List<string> collector = new();
}