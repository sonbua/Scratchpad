using System.Collections.ObjectModel;
using System.Linq;
using FluentAssertions;
using Machine.Specifications;

namespace LearnTestingFrameworks.Tests;

[Subject(typeof(ObservableCollection<>), nameof(ObservableCollection<object>.CollectionChanged))]
class ObservableCollectionContext
{
    Establish context = () =>
    {
        raised = false;
        collection = new ObservableCollection<int>();

        collection.CollectionChanged += (_, _) => { raised = true; };
    };

    protected static ObservableCollection<int> collection;
    protected static bool raised;
}

class when_enumerating_observablecollection : ObservableCollectionContext
{
    Because of = () => _ = collection.AsEnumerable().ToList();

    It should_not_raise_collectionchanged_event = () => raised.Should().BeFalse();
}

class when_invoke_clear_on_the_collection : ObservableCollectionContext
{
    Because of = () => collection.Clear();

    It should_raise_collectionchanged_event = () => raised.Should().BeTrue();
}
