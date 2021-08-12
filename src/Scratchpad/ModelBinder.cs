using Newtonsoft.Json;

namespace Scratchpad
{
    public static class ModelBinder
    {
        public static T Bind<T>(string json)
        {
            return JsonConvert.DeserializeObject<T>(json);
        }
    }
}