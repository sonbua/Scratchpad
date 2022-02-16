using System;
using System.Linq;
using Microsoft.AspNetCore.Mvc.Formatters;
using Microsoft.AspNetCore.Mvc.ModelBinding;
using Newtonsoft.Json;
using Newtonsoft.Json.Serialization;

namespace Scratchpad
{
    // Tries to mimic the behavior of `NewtonsoftJsonInputFormatter` in ASP.NET Core
    // Reference: https://github.com/dotnet/aspnetcore/blob/release/5.0/src/Mvc/Mvc.NewtonsoftJson/src/NewtonsoftJsonInputFormatter.cs
    // (son.nguyen@episerver.com)
    public static class MicrosoftNewtonsoftJsonModelBinder
    {
        public static T? Bind<T>(string json)
        {
            var modelStateDictionary = new ModelStateDictionary();

            var serializerSettings = new JsonSerializerSettings { Error = ErrorHandler };
            var model = JsonConvert.DeserializeObject<T>(json, serializerSettings);

            if (modelStateDictionary.Any())
            {
                var message = modelStateDictionary.ToErrorMessage();
                throw new Exception(message);
            }

            return model;

            // Extracted with small modification from
            // https://github.com/dotnet/aspnetcore/blob/29cd71c8f7af272cdced7e941c3cd45a28861428/src/Mvc/Mvc.NewtonsoftJson/src/NewtonsoftJsonInputFormatter.cs#L225
            // (son.nguyen@episerver.com)
            void ErrorHandler(object? sender, ErrorEventArgs e)
            {
                // When ErrorContext.Path does not include ErrorContext.Member, add Member to form full path.
                var path = e.ErrorContext.Path;
                var member = e.ErrorContext.Member?.ToString();
                var addMember = !string.IsNullOrEmpty(member);
                if (addMember)
                {
                    // Path.Member case (path.Length < member.Length) needs no further checks.
                    if (path.Length == member?.Length)
                    {
                        // Add Member in Path.Memb case but not for Path.Path.
                        addMember = !string.Equals(path, member, StringComparison.Ordinal);
                    }
                    else if (path.Length > member?.Length)
                    {
                        // Finally, check whether Path already ends with Member.
                        if (member?[0] == '[')
                        {
                            addMember = !path.EndsWith(member, StringComparison.Ordinal);
                        }
                        else
                        {
                            addMember = !path.EndsWith("." + member, StringComparison.Ordinal)
                                && !path.EndsWith("['" + member + "']", StringComparison.Ordinal)
                                && !path.EndsWith("[" + member + "]", StringComparison.Ordinal);
                        }
                    }
                }

                if (addMember)
                {
                    path = ModelNames.CreatePropertyModelName(path, member);
                }

                // Handle path combinations such as ""+"Property", "Parent"+"Property", or "Parent"+"[12]".
                var key = ModelNames.CreatePropertyModelName(prefix: string.Empty, propertyName: path);

                var modelStateException = WrapExceptionForModelState(e.ErrorContext.Error);

                // In the original code, the exception is put into the `context.ModelState` object of type
                // `ModelStateDictionary`. Since there is no context here, it is put into a `ModelStateDictionary` object.
                // (son.nguyen@episerver.com)
                modelStateDictionary.TryAddModelException(key, modelStateException);

                // Error must always be marked as handled
                // Failure to do so can cause the exception to be rethrown at every recursive level and
                // overflow the stack for x64 CLR processes
                e.ErrorContext.Handled = true;
            }
        }

        private static string ToErrorMessage(this ModelStateDictionary modelStateDictionary)
        {
            var shortErrorMessages =
                modelStateDictionary.ToDictionary(x => x.Key, x => x.Value!.Errors.First().ErrorMessage);

            return JsonConvert.SerializeObject(shortErrorMessages);
        }

        private static Exception WrapExceptionForModelState(Exception exception)
        {
            // It's not known that Json.NET currently ever raises error events with exceptions
            // other than these two types, but we're being conservative and limiting which ones
            // we regard as having safe messages to expose to clients
            if (exception is JsonReaderException || exception is JsonSerializationException)
            {
                // InputFormatterException specifies that the message is safe to return to a client, it will
                // be added to model state.
                return new InputFormatterException(exception.Message, exception);
            }

            // Not a known exception type, so we're not going to assume that it's safe.
            return exception;
        }

        /// <summary>
        /// Static class for helpers dealing with model names.
        /// Reference: https://github.com/dotnet/aspnetcore/blob/release/5.0/src/Mvc/Mvc.Core/src/ModelBinding/ModelNames.cs
        /// </summary>
        private static class ModelNames
        {
            /// <summary>
            /// Create an property model name with a prefix.
            /// </summary>
            /// <param name="prefix">The prefix to use.</param>
            /// <param name="propertyName">The property name.</param>
            /// <returns>The property model name.</returns>
            public static string CreatePropertyModelName(string prefix, string? propertyName)
            {
                if (string.IsNullOrEmpty(prefix))
                {
                    return propertyName ?? string.Empty;
                }

                if (string.IsNullOrEmpty(propertyName))
                {
                    return prefix ?? string.Empty;
                }

                if (propertyName.StartsWith("[", StringComparison.Ordinal))
                {
                    // The propertyName might represent an indexer access, in which case combining
                    // with a 'dot' would be invalid. This case occurs only when called from ValidationVisitor.
                    return prefix + propertyName;
                }

                return prefix + "." + propertyName;
            }
        }
    }
}