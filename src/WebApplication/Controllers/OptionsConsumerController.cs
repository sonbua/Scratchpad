using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Options;
using WebApplication.Configuration;

namespace WebApplication.Controllers;

public class OptionsConsumerController : ControllerBase
{
    internal const string RoutePrefix = "optionsConsumer/";

    private readonly IOptions<SomeOptions> _options;

    public OptionsConsumerController(IOptions<SomeOptions> options)
    {
        _options = options;
    }

    [Route(RoutePrefix)]
    public IActionResult Get()
    {
        return Ok(_options.Value.Configured.ToString());
    }
}

public class NoOptionsConsumerController : ControllerBase
{
    internal const string RoutePrefix = "NoOptionsConsumer/";

    [Route(RoutePrefix)]
    public IActionResult Get()
    {
        return Ok();
    }
}