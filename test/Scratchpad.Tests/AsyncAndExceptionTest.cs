using System;
using System.Threading;
using System.Threading.Tasks;
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

    [Fact]
    public void Result_OnException_ShouldThrowAggregateException()
    {
        var action = () => FaultedMethodAsync().Result;

        var exception = Assert.Throws<AggregateException>(action);
        Assert.IsType<ArgumentException>(exception.InnerException);
    }

    [Fact]
    public void GetAwaiterGetResult_OnException_ShouldThrowRealException()
    {
        var action = () => FaultedMethodAsync().GetAwaiter().GetResult();

        Assert.Throws<ArgumentException>(action);
    }

#pragma warning disable CS1998
    static async Task<string> FaultedMethodAsync()
#pragma warning restore CS1998
    {
        throw new ArgumentException();
    }
}
