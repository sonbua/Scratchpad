﻿using System.Threading.Tasks;
using WebApplication.Controllers;
using WebApplication.IntegrationTests.TestSetup;

namespace WebApplication.IntegrationTests;

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
        var isConfigured = await _fixture.Client.GetStringAsync(endpoint);

        Assert.Equal(true.ToString(), isConfigured);
    }
}
