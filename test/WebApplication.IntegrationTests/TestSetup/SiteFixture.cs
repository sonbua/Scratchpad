using System.Net.Http;

namespace WebApplication.IntegrationTests.TestSetup
{
    public class SiteFixture
    {
        public SiteFixture()
        {
            ServiceFixture = new ApiServiceFixture<Startup>();
            Client = ServiceFixture.CreateDefaultClient();
        }

        public ApiServiceFixture<Startup> ServiceFixture { get; }

        public HttpClient Client { get; }
    }
}