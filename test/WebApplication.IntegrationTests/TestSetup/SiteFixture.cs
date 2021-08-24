using System;
using System.Net.Http;

namespace WebApplication.IntegrationTests.TestSetup
{
    public class SiteFixture
    {
        public SiteFixture()
        {
            ServiceFixture = new ApiServiceFixture<Startup>();
            Services = ServiceFixture.Services;
            Client = ServiceFixture.CreateDefaultClient();
        }

        public ApiServiceFixture<Startup> ServiceFixture { get; }

        public IServiceProvider Services { get; }

        public HttpClient Client { get; }
    }
}