using System;
using System.Net.Http;
using Microsoft.AspNetCore.Mvc.Testing;

namespace WebApplication.IntegrationTests.TestSetup;

public class SiteFixture
{
    public SiteFixture()
    {
        var appFactory = new WebApplicationFactory<Program>();

        Services = appFactory.Services;
        Client = appFactory.CreateDefaultClient();
    }

    public IServiceProvider Services { get; }

    public HttpClient Client { get; }
}
