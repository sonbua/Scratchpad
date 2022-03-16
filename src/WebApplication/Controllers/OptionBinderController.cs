using System;
using System.IO;
using System.Text;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.Configuration.Json;
using Microsoft.Extensions.Options;
using Reusables;
using WebApplication.Configuration;

namespace WebApplication.Controllers;

[ApiController]
public class OptionBinderController : ControllerBase
{
    internal const string RoutePrefix = "OptionBinder";

    private readonly IOptions<OptionsModelOptions> _options;

    public OptionBinderController(IOptions<OptionsModelOptions> options)
    {
        _options = options;
    }

    [HttpGet(RoutePrefix)]
    public IActionResult Get()
    {
        return Ok(_options.Value);
    }

    [HttpPost(RoutePrefix)]
    public IActionResult Parse(string json)
    {
        var configuration = ConfigurationFactory(json);

        configuration.Bind(_options.Value);

        return Ok(_options.Value);
    }

    private static Func<string, ConfigurationRoot> ConfigurationFactory =>
        Piper.Pipe((string json) => Encoding.UTF8.GetBytes(json))
            .Pipe(bytes => new MemoryStream(bytes))
            .Pipe(stream => new JsonStreamConfigurationSource { Stream = stream })
            .Pipe(source => new JsonStreamConfigurationProvider(source))
            .Pipe(provider => new ConfigurationRoot(new IConfigurationProvider[] { provider }));
}