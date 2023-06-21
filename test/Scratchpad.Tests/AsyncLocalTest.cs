using System.Threading;
using System.Threading.Tasks;

namespace Scratchpad.Tests;

public class AsyncLocalTest
{
    [Fact]
    public void ShouldFlowAcrossTaskRun()
    {
        Accessor.Context.Value = "hello";

        Task.Run(() => Assert.Equal("hello", Accessor.Context.Value)).Wait();
    }

    [Fact]
    public void ShouldBeRestoredEvenIfBeingTampered()
    {
        Accessor.Context.Value = "hello";

        Task.Run(
                () =>
                {
                    Assert.Equal("hello", Accessor.Context.Value);

                    Accessor.Context.Value = "tampered";
                })
            .Wait();

        Assert.Equal("hello", Accessor.Context.Value);
    }

    static class Accessor
    {
        public static readonly AsyncLocal<string> Context = new AsyncLocal<string>();
    }
}
