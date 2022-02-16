using System.Collections.Immutable;
using System.Linq;
using System.Net;
using System.Net.Http.Json;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Mvc;
using WebApplication.Controllers;
using WebApplication.IntegrationTests.TestSetup;
using Xunit;

namespace WebApplication.IntegrationTests;

[Collection(IntegrationTestCollection.Name)]
public class ValidationControllerTest
{
    private readonly SiteFixture _fixture;

    public ValidationControllerTest(SiteFixture fixture)
    {
        _fixture = fixture;
    }

    [Fact]
    public async Task Get_WhenValidatingModel_WhichFails_ShouldReturnBadRequest()
    {
        var response = await _fixture.Client.GetAsync(ValidationController.Route + "?firstName=john&lastName=doe");

        Assert.Equal(HttpStatusCode.BadRequest, response.StatusCode);

        var problem = await response.Content.ReadFromJsonAsync<ValidationProblemDetails>();
        var errors = problem!.Errors.Values.SelectMany(x => x).ToImmutableArray();

        Assert.Single(errors, "Error from ShortCircuitAttribute");
    }
}