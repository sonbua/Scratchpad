using System;

namespace WebApplication.Configuration;

public class SomeOptions : IDisposable
{
    // For testing
    internal static bool? StaticConfigured;

    public SomeOptions()
    {
        Configured = false;
    }

    public bool Configured { get; set; }

    public void Dispose()
    {
        StaticConfigured = null;
    }
}