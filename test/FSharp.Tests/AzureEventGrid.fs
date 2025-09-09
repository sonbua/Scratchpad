module Azure

open FsToolkit.ErrorHandling

module EventGrid =

    open Azure
    open Azure.Identity
    open Azure.ResourceManager
    open Azure.ResourceManager.EventGrid
    open Azure.ResourceManager.Resources
    open Fake.Core

    [<CLIMutable>]
    type AzureOptions =
        { TenantId: string
          ClientId: string
          ClientSecret: string
          SubscriptionId: string
          ResourceGroup: string }

    module ArmClient =
        let create = EnvironmentCredential() |> ArmClient

    module ResourceGroupResource =
        let getEventGridTopic topicName (resourceGroup: ResourceGroupResource) =
            async {
                let! response = resourceGroup.GetEventGridTopicAsync(topicName) |> Async.AwaitTask

                if response.HasValue then
                    return Ok response.Value
                else
                    return Error $"Topic %s{topicName} does not exist."
            }

    module EventGridTopicResource =
        let cleanupSubscriptions (topic: EventGridTopicResource) =
            async {
                let subscriptions = topic.GetTopicEventSubscriptions().GetAll()

                for subscription in subscriptions do
                    do! subscription.DeleteAsync(WaitUntil.Completed) |> Async.AwaitTask |> Async.Ignore
            }

        let delete (topic: EventGridTopicResource) =
            async { do! topic.DeleteAsync(WaitUntil.Completed) |> Async.AwaitTask |> Async.Ignore }

    let deleteTopic azureOptions topicName =
        asyncResult {
            Environment.setEnvironVar "AZURE_TENANT_ID" azureOptions.TenantId
            Environment.setEnvironVar "AZURE_CLIENT_ID" azureOptions.ClientId
            Environment.setEnvironVar "AZURE_CLIENT_SECRET" azureOptions.ClientSecret

            let armClient = ArmClient.create

            let! response =
                ResourceGroupResource.CreateResourceIdentifier(azureOptions.SubscriptionId, azureOptions.ResourceGroup)
                |> armClient.GetResourceGroupResource
                |> _.GetAsync()
                |> Async.AwaitTask

            let resourceGroup = response.Value

            let! topic = resourceGroup |> ResourceGroupResource.getEventGridTopic topicName

            do! topic |> EventGridTopicResource.cleanupSubscriptions
            do! topic |> EventGridTopicResource.delete
        }
