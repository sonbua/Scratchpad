using System;
using System.Threading;
using System.Threading.Tasks;

namespace Scratchpad.Tests;

public class SemaphoreSlimTest
{
    [Fact]
    public async Task GetHandleBeforeTimeout_ShouldEnter()
    {
        // Arrange
        var semaphore = new SemaphoreSlim(initialCount: 1);

        // Act
        var entered = await semaphore.WaitAsync(TimeSpan.FromSeconds(1));

        // Assert
        entered.Should().BeTrue();
    }

    [Fact]
    public async Task TimedOut_ShouldNotEnter()
    {
        // Arrange
        var semaphore = new SemaphoreSlim(initialCount: 0);

        // Act
        var entered = await semaphore.WaitAsync(timeout: TimeSpan.FromSeconds(1));

        // Assert
        entered.Should().BeFalse();

        semaphore.Release();
        semaphore.CurrentCount.Should().Be(1);
    }

    [Fact]
    public async Task CancelledBeforeTimeout_ShouldThrowOperationCanceledException()
    {
        // Arrange
        var semaphore = new SemaphoreSlim(0);
        var longTimeout = TimeSpan.FromMinutes(1);
        var shortCancellationToken = new CancellationTokenSource(TimeSpan.FromSeconds(1)).Token;
        bool? entered = null;

        // Act
        var action = async () => entered = await semaphore.WaitAsync(longTimeout, shortCancellationToken);

        // Assert
        await action.Should().ThrowAsync<OperationCanceledException>();
        entered.Should().BeNull();
    }
}
