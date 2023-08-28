using System.Diagnostics.CodeAnalysis;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using WebApplication.Configuration;

var builder = Microsoft.AspNetCore.Builder.WebApplication.CreateBuilder(args);
var services = builder.Services;

services.AddMvc();

services.AddOptions<SomeOptions>();
services.Configure<SomeOptions>(options => options.Configured = true);

var app = builder.Build();

if (app.Environment.IsDevelopment())
{
    app.UseDeveloperExceptionPage();
}

app.MapGet("/", async context => { await context.Response.WriteAsync("Hello World!"); });
app.MapControllers();

app.Run();

// ReSharper disable UnusedMember.Global
[SuppressMessage("Design", "CA1050:Declare types in namespaces")]
public partial class Program
{
    // Expose the Program class for use with WebApplicationFactory<T>
}
