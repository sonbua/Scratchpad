using System.Collections.Generic;
using FluentAssertions;
using Machine.Specifications;

namespace LearnTestingFrameworks.Tests;

class MSpecAnatomy
{
    Establish context = () => { collector.Add("given (arrange)"); };

    Because of = () => { collector.Add("when (act)"); };

    It should_run = () => collector.Should().NotBeNullOrEmpty();

    It should_run_in_correct_order_ie_given_when_then = () =>
    {
        collector.Add("then (assert)");

        collector.Should().BeEquivalentTo(
            new[] { "given (arrange)", "when (act)", "then (assert)" },
            o => o.WithStrictOrdering());
    };

    protected static List<string> collector = new();
}
