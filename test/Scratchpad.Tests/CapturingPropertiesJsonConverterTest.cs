using System;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using Newtonsoft.Json;
using Xunit;

namespace Scratchpad.Tests
{
    [SuppressMessage("ReSharper", "ConvertToLocalFunction")]
    public class CapturingPropertiesJsonConverterTest
    {
        private readonly JsonSerializerSettings _serializerSettings;

        public CapturingPropertiesJsonConverterTest()
        {
            _serializerSettings = new JsonSerializerSettings
            {
                Converters = new List<JsonConverter> { new CapturingPropertiesJsonConverter() }
            };
        }

        [Theory]
        [MemberData(nameof(NoMatchingProperty))]
        public void GivenNoMatchingProperty_ShouldCapturedListBeEmpty(string json)
        {
            var model = JsonConvert.DeserializeObject<Model>(json, _serializerSettings);

            Assert.NotNull(model);
            Assert.Empty(model.Matches);
        }

        public static TheoryData NoMatchingProperty => new TheoryData<string>
        {
            "{}",
            "{\"randomProperty\":5}",
        };

        [Theory]
        [MemberData(nameof(WithMatchingProperty))]
        public void GivenMatchingProperty_ShouldCapture(string json)
        {
            var model = JsonConvert.DeserializeObject<Model>(json, _serializerSettings);

            Assert.NotNull(model);
            Assert.Contains(nameof(Model.MatchingProp1), model.Matches);
        }

        public static TheoryData WithMatchingProperty => new TheoryData<string>
        {
            "{\"matchingProp1\":\"some-value\"}",
            "{\"matchingProp1\":\"some-value\",\"randomProperty\":\"any-value\"}",
        };

        [Theory]
        [MemberData(nameof(InvalidJsonFormats))]
        public void GivenInvalidJsonFormat_ShouldThrow(string json, Type exceptionType)
        {
            Action deserializeWithDefault = () => JsonConvert.DeserializeObject<Model>(json);

            var exception = Assert.Throws(exceptionType, deserializeWithDefault);

            Action deserializeWithConverters = () => JsonConvert.DeserializeObject<Model>(json, _serializerSettings);

            var customException = Assert.Throws(exceptionType, deserializeWithConverters);
            Assert.Equal(exception.Message, customException.Message);
        }

        public static TheoryData InvalidJsonFormats = new TheoryData<string, Type>
        {
            {
                "This is",
                typeof(JsonReaderException)
            },
            {
                "this is",
                typeof(JsonReaderException)
            },
        };

        [Theory]
        [MemberData(nameof(InvalidPropertyDataTypes))]
        public void GivenInvalidValueTypeForProperty_ShouldThrow(string json, Type exceptionType)
        {
            Action deserializeWithDefault = () => JsonConvert.DeserializeObject<Model>(json);

            var exception = Assert.Throws(exceptionType, deserializeWithDefault);

            Action deserializeWithConverters = () => JsonConvert.DeserializeObject<Model>(json, _serializerSettings);

            var customException = Assert.Throws(exceptionType, deserializeWithConverters);
            Assert.Equal(exception.Message, customException.Message);
        }

        public static TheoryData InvalidPropertyDataTypes => new TheoryData<string, Type>
        {
            {
                "{\"boolProperty\":\"invalidBoolean\"}",
                typeof(JsonReaderException)
            },
        };

        [Theory]
        [MemberData(nameof(EmptyValues))]
        public void GivenEmptyValue_ShouldReturnNull(string emptyJson)
        {
            var model = JsonConvert.DeserializeObject<Model>(emptyJson, _serializerSettings);

            Assert.Null(model);
        }

        public static TheoryData EmptyValues => new TheoryData<string>
        {
            "",
            "null",
        };

        [Theory(Skip = "This case hasn't been handled by the custom converter yet")]
        [MemberData(nameof(EdgeCase))]
        public void GivenEdgeCase_ShouldThrowGenericException(string json, Type exceptionType)
        {
            Action deserializeWithDefault = () => JsonConvert.DeserializeObject<Model>(json);

            var exception = Assert.Throws(exceptionType, deserializeWithDefault);

            Action deserializeWithConverters = () => JsonConvert.DeserializeObject<Model>(json, _serializerSettings);

            var customException = Assert.Throws(exceptionType, deserializeWithConverters);
            Assert.Equal(exception.Message, customException.Message);
        }

        public static TheoryData EdgeCase = new TheoryData<string, Type>
        {
            {
                "\"this is\"",
                typeof(JsonSerializationException)
                // $"Error converting value \"this is\" to type '{typeof(Model).FullName}'. Path '', line 1, position 9."
            },
        };
    }
}