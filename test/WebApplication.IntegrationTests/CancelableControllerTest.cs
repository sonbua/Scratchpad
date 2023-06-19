using System;
using System.Diagnostics;
using System.Threading;
using System.Threading.Tasks;
using WebApplication.IntegrationTests.TestSetup;

namespace WebApplication.IntegrationTests;

[Collection(IntegrationTestCollection.Name)]
public class CancelableControllerTest
{
    const string RoutePrefix = "Cancelable/";

    readonly SiteFixture _fixture;

    public CancelableControllerTest(SiteFixture fixture)
    {
        _fixture = fixture;
    }

    [Fact]
    public async Task NoCancellation_ReturnsAfter3Seconds()
    {
        var stopwatch = new Stopwatch();
        stopwatch.Start();

        var response = await _fixture.Client.GetAsync(RoutePrefix);

        stopwatch.Stop();
        var elapsedTime = stopwatch.Elapsed;

        elapsedTime.Should().BeGreaterOrEqualTo(TimeSpan.FromSeconds(3));
    }

    [Fact]
    public async Task CancelAfter1Second_ShouldAlsoSignalCancellationOnServerSide()
    {
        var cancellationToken = new CancellationTokenSource(TimeSpan.FromSeconds(1)).Token;

        var responseAction = async () => await _fixture.Client.GetAsync(RoutePrefix, cancellationToken);

        // Ensure client-side has canceled the request
        await responseAction.Should().ThrowAsync<OperationCanceledException>();

        var response = await _fixture.Client.GetAsync(RoutePrefix + "lastRequestCancelled");
        var cancelled = await response.Content.ReadAsStringAsync();

        // Ensure server-side has received the signal and cancel the on-going request
        cancelled.Should().Be("true");
    }
}
