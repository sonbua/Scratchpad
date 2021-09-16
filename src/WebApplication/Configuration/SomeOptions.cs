namespace WebApplication.Configuration
{
    public class SomeOptions
    {
        // For testing
        internal static bool? StaticConfigured;

        public SomeOptions()
        {
            Configured = false;
        }

        public bool Configured { get; set; }
    }
}