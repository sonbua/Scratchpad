using System;

namespace Reusables;

public static class Piper
{
    public static Func<T, TReturn2> Pipe<T, TReturn1, TReturn2>(
        this Func<T, TReturn1> func1,
        Func<TReturn1, TReturn2> func2)
    {
        return x => func2(func1(x));
    }

    public static Func<T, TReturn> Pipe<T, TReturn>(Func<T, TReturn> func) => func;
}