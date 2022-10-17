using System.Linq;
using FluentAssertions;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.DependencyInjection.Extensions;
using Xunit;

namespace Scratchpad.Tests;

public class ServiceCollectionExtensionsTest
{
    [Fact]
    public void Add_SecondRegistration_ShouldOverrideFirstRegistration()
    {
        // Arrange
        var services = new ServiceCollection();
        services.AddSingleton<IService, FirstComponent>();

        // Act
        services.AddSingleton<IService, SecondComponent>();

        // Assert
        var serviceProvider = services.BuildServiceProvider();
        serviceProvider.GetService<IService>().Should().BeOfType<SecondComponent>();
    }

    [Fact]
    public void TryAdd_SecondRegistration_ShouldNotOverrideFirstRegistration()
    {
        // Arrange
        var services = new ServiceCollection();
        services.AddSingleton<IService, FirstComponent>();

        // Act
        services.TryAddSingleton<IService, SecondComponent>();

        // Assert
        var serviceProvider = services.BuildServiceProvider();
        serviceProvider.GetService<IService>().Should().BeOfType<FirstComponent>();
    }

    [Fact]
    public void TryAddEnumerable_SecondRegistration_ShouldAddInOrder()
    {
        // Arrange
        var services = new ServiceCollection();
        services.TryAddEnumerable(ServiceDescriptor.Singleton<IService, FirstComponent>());

        // Act
        services.TryAddEnumerable(ServiceDescriptor.Singleton<IService, SecondComponent>());

        // Assert
        var serviceProvider = services.BuildServiceProvider();
        serviceProvider.GetServices<IService>()
            .Select(x => x.GetType())
            .Should().ContainInOrder(typeof(FirstComponent), typeof(SecondComponent));
    }

    interface IService
    {
    }

    class FirstComponent : IService
    {
    }

    class SecondComponent : IService
    {
    }
}
