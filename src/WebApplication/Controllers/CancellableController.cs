using System;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Mvc;

namespace WebApplication.Controllers;

[ApiController]
public class CancelableController : ControllerBase
{
    private static bool lastRequestCancelled;
    private const string RoutePrefix = "Cancelable/";

    [Route(RoutePrefix)]
    [HttpGet]
    public async Task<IActionResult> Index(int delayInSeconds = 3)
    {
        try
        {
            var requestAbortedToken = HttpContext.RequestAborted;

            await Task.Delay(TimeSpan.FromSeconds(delayInSeconds), requestAbortedToken);

            return Ok();
        }
        catch (OperationCanceledException)
        {
            lastRequestCancelled = true;

            return BadRequest();
        }
    }

    [Route(RoutePrefix + "lastRequestCancelled")]
    [HttpGet]
    public IActionResult LastRequestCancelled() => Ok(lastRequestCancelled);
}
