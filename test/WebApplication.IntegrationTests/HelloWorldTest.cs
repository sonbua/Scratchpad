using System.Net;
using System.Threading.Tasks;
using WebApplication.IntegrationTests.TestSetup;
using Xunit;

namespace WebApplication.IntegrationTests;

[Collection(IntegrationTestCollection.Name)]
public class HelloWorldTest
{
    private readonly SiteFixture _fixture;

    public HelloWorldTest(SiteFixture fixture)
    {
        _fixture = fixture;
    }

    [Fact]
    public async Task WhenRequestRootPath_ShouldReturnResponse()
    {
        var response = await _fixture.Client.GetAsync("/");

        Assert.Equal(HttpStatusCode.OK, response.StatusCode);

        var content = await response.Content.ReadAsStringAsync();

        Assert.Equal("Hello World!", content);
    }
}