using System;
using Newtonsoft.Json;
using Xunit;

namespace Scratchpad.Tests
{
    public class ModelBinderTest
    {
        [Fact]
        public void Bind_GivenConflictedDataType_ShouldThrowJsonReaderException()
        {
            // Arrange
            // SortOrder is defined as Integer but passed in as Boolean
            var json = "{editSettings:{sortOrder:true}}";

            // Act
            Action bind = () => ModelBinder.Bind<ExternalContentType>(json);

            // Assert
            var exception = Assert.Throws<JsonReaderException>(bind);
            Assert.Equal(
                "Unexpected character encountered while parsing value: t. Path 'editSettings.sortOrder', line 1, position 26.",
                exception.Message);
        }
    }
}