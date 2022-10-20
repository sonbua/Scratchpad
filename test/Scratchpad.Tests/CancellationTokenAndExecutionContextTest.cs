using System.Globalization;
using System.Threading;
using Microsoft.Extensions.Primitives;
using Xunit;

namespace Scratchpad.Tests;

public class CancellationTokenAndExecutionContextTest
{
    [Fact]
    public void RegisterChangeCallback_ShouldCallbackBeingRunOnTheExecutionContextWhichSignalsTheChange()
    {
        // Arrange
        CultureInfo.CurrentCulture = new CultureInfo("de");

        var cts = new CancellationTokenSource();
        var cancellationChangeToken = new CancellationChangeToken(cts.Token);

        // Act
        cancellationChangeToken.RegisterChangeCallback(
            // Assert
            // This is run on the ExecutionContext, on which the change is signaled
            callback: _ => Assert.Equal("sv", CultureInfo.CurrentCulture.Name),
            state: null);

        CultureInfo.CurrentCulture = new CultureInfo("sv");
        cts.Cancel();
    }

    [Fact]
    public void Register_ShouldCallbackBeingRunOnTheCapturedExecutionContext()
    {
        // Arrange
        CultureInfo.CurrentCulture = new CultureInfo("de");

        var cts = new CancellationTokenSource();

        // Act
        cts.Token.Register(
            // Assert
            // This is run on the captured ExecutionContext, where the callback is registered
            callback: () => Assert.Equal("de", CultureInfo.CurrentCulture.Name));

        CultureInfo.CurrentCulture = new CultureInfo("sv");
        cts.Cancel();
    }
}
