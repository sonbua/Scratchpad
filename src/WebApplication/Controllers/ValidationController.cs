using System;
using System.ComponentModel.DataAnnotations;
using Microsoft.AspNetCore.Mvc;

namespace WebApplication.Controllers;

[ApiController]
public class ValidationController : ControllerBase
{
    // TODO: Make internal
    public const string Route = "validation/";

    [Route(Route)]
    [HttpGet]
    public IActionResult Get([FromQuery] [ShortCircuit] Person person) => throw new NotImplementedException();

    public record Person(string FirstName, string LastName);
}

[AttributeUsage(AttributeTargets.Parameter)]
public class ShortCircuitAttribute : ValidationAttribute
{
    protected override ValidationResult IsValid(object? value, ValidationContext validationContext)
    {
        return new ValidationResult("Error from ShortCircuitAttribute");
    }
}