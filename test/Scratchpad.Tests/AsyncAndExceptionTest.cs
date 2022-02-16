using System;
using System.Threading;
using System.Threading.Tasks;
using FluentAssertions;
using Xunit;
using Xunit.Sdk;

namespace Scratchpad.Tests;

public class AsyncAndExceptionTest
{
    [Fact]
    public async Task WhenAsyncVoidMethodThrows_ShouldNotThrowToTheCaller_AndRaiseOnActiveSynchronizationContext()
    {
        var result = AsyncAndException.InvokeAsyncVoid();

        result.Should().Be("try branch");

        var synchronizationContext = SynchronizationContext.Current as AsyncTestSyncContext;
        var exception = await synchronizationContext!.WaitForCompletionAsync();

        exception.Should().NotBeNull()
            .And.BeOfType<NotSupportedException>()
            .Which.Message.Should().Be("async void");
    }

    [Fact]
    public async Task WhenAsyncTaskMethodThrows_ShouldBeCaught()
    {
        var result = await AsyncAndException.InvokeAsyncTask();

        result.Should().Be("catch branch");
    }
}