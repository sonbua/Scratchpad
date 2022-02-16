using Microsoft.AspNetCore.Mvc.Testing;

namespace WebApplication.IntegrationTests.TestSetup;

public class ApiServiceFixture<TStartup> : WebApplicationFactory<TStartup>
    where TStartup : class
{
}