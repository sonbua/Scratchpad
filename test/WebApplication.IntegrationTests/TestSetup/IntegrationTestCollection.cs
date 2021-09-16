using Xunit;

namespace WebApplication.IntegrationTests.TestSetup
{
    [CollectionDefinition(Name, DisableParallelization = true)]
    public class IntegrationTestCollection : ICollectionFixture<SiteFixture>
    {
        public const string Name = "WebApplication integration tests";
    }

    [CollectionDefinition(Name, DisableParallelization = true)]
    public class OtherIntegrationTestCollection : ICollectionFixture<SiteFixture>
    {
        public const string Name = "Just another WebApplication integration tests";
    }
}