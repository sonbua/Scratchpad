using System.IO;
using System.Text;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.Configuration.Json;
using Microsoft.Extensions.Options;
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
        var configuration = ConfigurationFromJson(json);

        configuration.Bind(_options.Value);

        return Ok(_options.Value);
    }

    private static ConfigurationRoot ConfigurationFromJson(string json)
    {
        var optionsStream = new MemoryStream(Encoding.UTF8.GetBytes(json));
        var configurationSource = new JsonStreamConfigurationSource { Stream = optionsStream };
        var configurationProvider = new JsonStreamConfigurationProvider(configurationSource);

        return new ConfigurationRoot(new IConfigurationProvider[] { configurationProvider });
    }
}