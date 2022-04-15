using System;
using System.Threading;
using FluentAssertions;
using Machine.Specifications;

namespace LearnTestingFrameworks.Tests;

class MSpecContextLifetimeSpec
{
    Establish context = () =>
    {
        initial = 0;
        Console.WriteLine($"counter: {Interlocked.Increment(ref counter)}");
    };

    class Scenario1
    {
        Establish context = () => initial++;

        It should_not_be_interfered_by_siblings = () => initial.Should().Be(1);
    }

    class Scenario2
    {
        Establish context = () => initial++;

        It should_not_be_interfered_by_siblings = () => initial.Should().Be(1);
    }

    class Scenario3
    {
        Establish context = () => initial++;

        It should_not_be_interfered_by_siblings = () => initial.Should().Be(1);
    }

    protected static int initial;
    static int counter;
}

class InheritedContextSpec : MSpecContextLifetimeSpec
{
    Establish context = () => initial++;

    It should_not_be_interfered_by_other_branches = () => initial.Should().Be(1);
}

class InheritedContextWithNestedContextSpec : MSpecContextLifetimeSpec
{
    Establish context = () => initial++;

    class NestedContext
    {
        Establish context = () => initial++;

        It should_not_be_interfered_by_other_branches = () => initial.Should().Be(2);
    }
}
