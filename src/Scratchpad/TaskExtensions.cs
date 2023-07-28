using System.Threading.Tasks;

namespace Scratchpad;

public static class TaskExtensions
{
    /// <summary>
    /// Ignores the completion and results of the given <paramref name="task"/>. Also ignores exceptions.
    /// </summary>
    /// <param name="task">The task to ignore.</param>
    public static async void Ignore(this Task task)
    {
        try
        {
            await task.ConfigureAwait(false);
        }
        catch
        {
            // ignored
        }
    }
}
