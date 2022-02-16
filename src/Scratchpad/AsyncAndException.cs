using System;
using System.Threading.Tasks;

namespace Scratchpad;

public static class AsyncAndException
{
    public static string InvokeAsyncVoid()
    {
        try
        {
            AsyncVoid();

            return "try branch";
        }
        catch (NotSupportedException)
        {
            return "catch branch";
        }
    }

#pragma warning disable CS1998
    private static async void AsyncVoid()
#pragma warning restore CS1998
    {
        await Task.Yield();

        throw new NotSupportedException("async void");
    }

    public static async Task<string> InvokeAsyncTask()
    {
        try
        {
            await AsyncTask();

            return "try branch";
        }
        catch (NotImplementedException)
        {
            return "catch branch";
        }
    }

    private static async Task AsyncTask()
    {
        await Task.Yield();

        throw new NotImplementedException();
    }
}