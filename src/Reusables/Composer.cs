using System;

namespace Reusables;

public static class Composer
{
    public static Func<T, TReturn2> Compose<T, TReturn1, TReturn2>(
        this Func<TReturn1, TReturn2> func1,
        Func<T, TReturn1> func2)
    {
        return x => func1(func2(x));
    }

    public static Func<T, TReturn> Compose<T, TReturn>(Func<T, TReturn> func) => func;
}