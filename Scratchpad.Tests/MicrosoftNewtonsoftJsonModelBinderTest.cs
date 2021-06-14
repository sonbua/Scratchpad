using System;
using Xunit;

namespace Scratchpad.Tests
{
    public class MicrosoftNewtonsoftJsonModelBinderTest
    {
        [Fact]
        public void Bind_GivenConflictedDataType_ShouldThrowException()
        {
            // Arrange
            // SortOrder is defined as Integer but passed in as Boolean
            var json = "{editSettings:{sortOrder:true}}";

            // Act
            Action bind = () => MicrosoftNewtonsoftJsonModelBinder.Bind<ExternalContentType>(json);

            // Assert
            var exception = Assert.Throws<Exception>(bind);
            Assert.Equal(
                "{\"editSettings.sortOrder\":\"Unexpected character encountered while parsing value: t. Path 'editSettings.sortOrder', line 1, position 26.\",\"editSettings.sortOrder.editSettings\":\"Unexpected character encountered while parsing value: r. Path 'editSettings.sortOrder', line 1, position 26.\"}",
                exception.Message);
        }
    }
}