using System.Threading.Tasks;

namespace Scratchpad.Tests;

public class TaskCancellingAndException
{
    [Fact]
    public async Task AwaitCanceledTask_ShouldThrowTaskCanceledException()
    {
        var tcs = new TaskCompletionSource();
        tcs.SetCanceled();

        var action = async () => await tcs.Task;

        await action.Should().ThrowAsync<TaskCanceledException>();
    }
}
