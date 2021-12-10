using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;

namespace Scratchpad
{
    public class CapturingPropertiesJsonConverter : JsonConverter<Model>
    {
        private static readonly IImmutableSet<string> CapturingProperties =
            ImmutableHashSet.Create(StringComparer.OrdinalIgnoreCase, new[]
            {
                nameof(Model.MatchingProp1)
            });

        public override void WriteJson(JsonWriter writer, Model? value, JsonSerializer serializer)
        {
            throw new NotSupportedException();
        }

        public override Model? ReadJson(
            JsonReader reader,
            Type objectType,
            Model? existingValue,
            bool hasExistingValue,
            JsonSerializer serializer)
        {
            if (reader.TokenType == JsonToken.Null)
            {
                return null;
            }

            var jToken = JToken.ReadFrom(reader);
            var model = new Model();

            var jsonProperties = jToken.Children()
                .Select(x => x.Path)
                .ToHashSet(StringComparer.OrdinalIgnoreCase);

            foreach (var capturingProperty in CapturingProperties)
            {
                if (jsonProperties.Contains(capturingProperty))
                {
                    model.Matches.Add(capturingProperty);
                }
            }

            serializer.Populate(jToken.CreateReader(), model);

            return model;
        }
    }

    public class Model
    {
        public Model()
        {
            MatchingProp1 = string.Empty;
            Matches = new HashSet<string>();
        }

        public string MatchingProp1 { get; set; }

        public bool BoolProperty { get; set; }

        public ISet<string> Matches { get; }
    }
}