using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Http;

namespace Scratchpad.Tests;

public class HttpContextAccessorTest
{
    [Fact]
    public async Task SetHttpContextToBeNull_ShouldHttpContextOnOtherThreadsBeNull()
    {
        var accessor = new HttpContextAccessor { HttpContext = new DefaultHttpContext() };

        var task1 = FlowHttpContextHolderThenSetHttpContextNull(accessor);
        var task2 = ConsumeHttpContextWithinAGivenRequest(accessor);

        var tasks = new List<Task> { task1, task2 };

        await Task.WhenAll(tasks);

        accessor.HttpContext.Should()
            .BeNull(because: $"it has been restored in {nameof(FlowHttpContextHolderThenSetHttpContextNull)} method");
    }

    static Task FlowHttpContextHolderThenSetHttpContextNull(HttpContextAccessor accessor) =>
        Task.Run(
            async () =>
            {
                accessor.HttpContext.Should().NotBeNull(because: "it is flowed with AsyncLocal");

                using (new NoHttpContextScope(accessor))
                {
                    accessor.HttpContext.Should().BeNull();
                    await Task.Delay(500);
                    accessor.HttpContext.Should().BeNull();
                }

                accessor.HttpContext.Should().NotBeNull(because: "it is restored after `using`-block");
            });

    static async Task ConsumeHttpContextWithinAGivenRequest(HttpContextAccessor accessor)
    {
        await Task.Delay(100);

        accessor.HttpContext.Should()
            .BeNull(because: "HttpContext object trapped in AsyncLocals has been cleared in another place");

        await Task.Delay(1000);

        accessor.HttpContext.Should()
            .BeNull(because: "HttpContext object trapped in AsyncLocals has been cleared in another place");
    }

    // from Johan Björnfot
    public class NoHttpContextScope : IDisposable
    {
        readonly IHttpContextAccessor _httpContextAccessor;
        readonly HttpContext _context;

        public NoHttpContextScope(IHttpContextAccessor httpContextAccessor)
        {
            _httpContextAccessor = httpContextAccessor;
            _context = httpContextAccessor.HttpContext;
            httpContextAccessor.HttpContext = null;
        }

        public void Dispose() => _httpContextAccessor.HttpContext = _context;
    }
}
