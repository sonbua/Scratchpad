using System.Threading.Tasks;
using WebApplication.Configuration;
using WebApplication.Controllers;
using WebApplication.IntegrationTests.TestSetup;
using Xunit;

namespace WebApplication.IntegrationTests
{
    [Collection(IntegrationTestCollection.Name)]
    public class OptionsTest
    {
        private readonly SiteFixture _fixture;

        public OptionsTest(SiteFixture fixture)
        {
            _fixture = fixture;
        }

        [Fact]
        public async Task Configure_WhenConsumingIOptions_ShouldRunConfigure()
        {
            const string endpoint = OptionsConsumerController.RoutePrefix;
            var postConfigured = await _fixture.Client.GetStringAsync(endpoint);

            Assert.Equal(true.ToString(), postConfigured);
        }
    }

    [Collection(OtherIntegrationTestCollection.Name)]
    public class OtherOptionsTest
    {
        private readonly SiteFixture _fixture;

        public OtherOptionsTest(SiteFixture fixture)
        {
            _fixture = fixture;
        }

        [Fact]
        public async Task Configure_WhenNotConsumingIOptions_ShouldNotRunConfigure()
        {
            const string endpoint = NoOptionsConsumerController.RoutePrefix;
            var response = await _fixture.Client.GetAsync(endpoint);

            Assert.True(response.IsSuccessStatusCode);
            Assert.Null(SomeOptions.StaticConfigured);
        }
    }
}