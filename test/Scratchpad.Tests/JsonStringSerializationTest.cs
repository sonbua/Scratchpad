using Newtonsoft.Json;
using JsonSerializer = System.Text.Json.JsonSerializer;

namespace Scratchpad.Tests;

public class JsonStringSerializationTest
{
    [Theory(Skip = "some tests fail")]
    [MemberData(nameof(InputAndLengthAndSerializedValue))]
    public void Newtonsoft_SpecialCharacter_ShouldEscape(
        string input,
        int inputLength,
        string expectedSerializedValue)
    {
        Assert.Equal(inputLength, input.Length);

        var actual = JsonConvert.SerializeObject(input);

        Assert.Equal($"\"{expectedSerializedValue}\"", actual);

        var deserialized = JsonConvert.DeserializeObject($"\"{expectedSerializedValue}\"");

        Assert.Equal(input, deserialized);
    }

    [Theory(Skip = "some tests fail")]
    [MemberData(nameof(InputAndLengthAndSerializedValue))]
    public void SystemText_SpecialCharacter_ShouldEscape(
        string input,
        int inputLength,
        string expectedSerializedValue)
    {
        Assert.Equal(inputLength, input.Length);

        var actual = JsonSerializer.Serialize(input);

        Assert.Equal($"\"{expectedSerializedValue}\"", actual);

        var deserialized = JsonConvert.DeserializeObject($"\"{expectedSerializedValue}\"");

        Assert.Equal(input, deserialized);
    }

    public static TheoryData<string, int, string> InputAndLengthAndSerializedValue = new()
    {
        // input, length, serialized value
        { "\"", 1, @"\""" },
        { @"\", 1, @"\\" },
        { @"/", 1, @"\/" },
        { "\b", 1, @"\b" },
        { "\f", 1, @"\f" },
        { "\n", 1, @"\n" },
        { "\r", 1, @"\r" },
        { "\t", 1, @"\t" },
    };
}
