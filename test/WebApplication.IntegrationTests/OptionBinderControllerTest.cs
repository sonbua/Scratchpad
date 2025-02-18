using System.Net;
using System.Net.Http.Json;
using System.Text.Encodings.Web;
using System.Text.Json;
using System.Threading.Tasks;
using WebApplication.Controllers;
using WebApplication.IntegrationTests.TestSetup;

namespace WebApplication.IntegrationTests;

[Collection(IntegrationTestCollection.Name)]
public class OptionBinderControllerTest
{
    private const string RoutePrefix = OptionBinderController.RoutePrefix;

    private readonly SiteFixture _fixture;

    public OptionBinderControllerTest(SiteFixture fixture)
    {
        _fixture = fixture;
    }

    [Fact]
    public async Task GivenOptionsAsJson_WhenHttpPost_ShouldReturnBoundOptions()
    {
        // Requires options having default value at first
        var response = await _fixture.Client.GetAsync(RoutePrefix);
        var options = await response.Content.ReadFromJsonAsync<OptionsModelResponseOptions>();

        options.Should().BeEquivalentTo(new { StringProp = "default value" });

        // Configures new value for options
        var optionsToBeBound = new { StringProp = "some string" };
        var optionsString = JsonSerializer.Serialize(optionsToBeBound);
        optionsString = UrlEncoder.Default.Encode(optionsString);

        response = await _fixture.Client.PostAsync($"{RoutePrefix}?json={optionsString}", null);

        response.StatusCode.Should().Be(HttpStatusCode.OK);

        options = await response.Content.ReadFromJsonAsync<OptionsModelResponseOptions>();
        options.Should().BeEquivalentTo(optionsToBeBound);

        // Ensures new value for options has been set
        response = await _fixture.Client.GetAsync(RoutePrefix);
        options = await response.Content.ReadFromJsonAsync<OptionsModelResponseOptions>();

        options.Should().BeEquivalentTo(optionsToBeBound);
    }

    internal class OptionsModelResponseOptions
    {
        public string StringProp { get; set; }
    }
}
