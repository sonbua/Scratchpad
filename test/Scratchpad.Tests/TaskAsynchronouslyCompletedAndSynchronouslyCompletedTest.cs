using System.Globalization;
using System.Threading;
using System.Threading.Tasks;

namespace Scratchpad.Tests;

public class TaskAsynchronouslyCompletedAndSynchronouslyCompletedTest
{
    CultureInfo De { get; }
    CultureInfo Sv { get; }

    public TaskAsynchronouslyCompletedAndSynchronouslyCompletedTest()
    {
        De = CultureInfo.CreateSpecificCulture("de");
        Sv = CultureInfo.CreateSpecificCulture("sv");
    }

    async Task ProcessSomethingInGermanCultureAsync()
    {
        Assert.Equal(Sv, CultureInfo.CurrentCulture);

        CultureInfo.CurrentCulture = De;

        await Task.Yield();

        Assert.Equal(De, CultureInfo.CurrentCulture);
    }

    [Fact]
    public async Task WhenAwaitingAsynchronousTaskReturningMethod_ShouldRestoreExecutionContext()
    {
        CultureInfo.CurrentCulture = Sv;

        await ProcessSomethingInGermanCultureAsync().ConfigureAwait(false);

        Assert.Equal(Sv, CultureInfo.CurrentCulture);
    }

    Task ProcessSomethingInGermanCulture()
    {
        Assert.Equal(Sv, CultureInfo.CurrentCulture);

        CultureInfo.CurrentCulture = De;

        return Task.CompletedTask;
    }

    [Fact]
    public async Task WhenAwaitingSynchronousTaskReturningMethod_ShouldNotRestoreExecutionContext()
    {
        CultureInfo.CurrentCulture = Sv;

        await ProcessSomethingInGermanCulture().ConfigureAwait(false);

        Assert.Equal(De, CultureInfo.CurrentCulture);
    }

    [Fact]
    public async Task WhenAwaitingSynchronousTaskReturningMethod_AndRestoringContext_ShouldNotChangeExecutionContext()
    {
        CultureInfo.CurrentCulture = Sv;

        var ec = ExecutionContext.Capture();
        await ProcessSomethingInGermanCulture();
        ExecutionContext.Restore(ec!);

        Assert.Equal(Sv, CultureInfo.CurrentCulture);
    }

    [Fact]
    public async Task WhenAwaitingSynchronousTaskReturningMethod_AndFlowSuppressed_ShouldNotChangeExecutionContext()
    {
        CultureInfo.CurrentCulture = Sv;

        // BAD! We should not do this, since we don't know
        // where the continuation runs
        using (ExecutionContext.SuppressFlow())
        {
            await ProcessSomethingInGermanCulture();

            // If the continuation is on a thread other than
            // where it originated, it throws an exception saying
            // "AsyncFlowControl object must be used on the thread where it was created."
            // E.g.
            // await ProcessSomethingInGermanCultureAsync();
        }

        Assert.Equal(De, CultureInfo.CurrentCulture);
    }

    async Task ProcessSomethingInGermanCultureWrapperAsync() => await ProcessSomethingInGermanCulture();

    [Fact]
    public async Task WhenAwaitingAsynchronousTaskReturningMethod_AndRestoreContext_ShouldNotChangeExecutionContext()
    {
        CultureInfo.CurrentCulture = Sv;

        await ProcessSomethingInGermanCultureWrapperAsync();

        Assert.Equal(Sv, CultureInfo.CurrentCulture);
    }
}
