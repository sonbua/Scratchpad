module Azure

open Azure
open Azure.ResourceManager.Resources
open Fake.Core
open FsToolkit.ErrorHandling

module ArmClient =
    open Azure.Identity
    open Azure.ResourceManager

    let create = EnvironmentCredential() |> ArmClient

[<CLIMutable>]
type AzureOptions =
    { TenantId: string
      ClientId: string
      ClientSecret: string
      SubscriptionId: string
      ResourceGroup: string }

module ResourceGroupResource =
    let createResourceIdentifier subscriptionId resourceGroupName =
        ResourceGroupResource.CreateResourceIdentifier(subscriptionId, resourceGroupName)

module EventGrid =
    open Azure.ResourceManager.EventGrid

    module ResourceGroupResource =
        let getEventGridTopic topicName (resourceGroup: ResourceGroupResource) =
            async {
                let! response = resourceGroup.GetEventGridTopicAsync(topicName) |> Async.AwaitTask

                if response.HasValue then
                    return Ok response.Value
                else
                    return Error $"Topic %s{topicName} does not exist."
            }

        let createResourceIdentifier subscriptionId resourceGroupName =
            ResourceGroupResource.CreateResourceIdentifier(subscriptionId, resourceGroupName)

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
                ResourceGroupResource.createResourceIdentifier azureOptions.SubscriptionId azureOptions.ResourceGroup
                |> armClient.GetResourceGroupResource
                |> _.GetAsync()
                |> Async.AwaitTask

            let resourceGroup = response.Value

            let! topic = resourceGroup |> ResourceGroupResource.getEventGridTopic topicName

            do! topic |> EventGridTopicResource.cleanupSubscriptions
            do! topic |> EventGridTopicResource.delete
        }

module StorageAccount =
    open Azure.ResourceManager.Storage

    module ResourceGroupResource =
        let getStorageAccount accountName (resourceGroup: ResourceGroupResource) =
            async {
                let! response = resourceGroup.GetStorageAccountAsync(accountName) |> Async.AwaitTask

                if response.HasValue then
                    return Ok response.Value
                else
                    return Error $"Storage account %s{accountName} does not exist."
            }

    module StorageAccountResource =
        let delete (storageAccount: StorageAccountResource) =
            async {
                do!
                    storageAccount.DeleteAsync(WaitUntil.Completed)
                    |> Async.AwaitTask
                    |> Async.Ignore
            }

    let delete azureOptions accountName =
        asyncResult {
            Environment.setEnvironVar "AZURE_TENANT_ID" azureOptions.TenantId
            Environment.setEnvironVar "AZURE_CLIENT_ID" azureOptions.ClientId
            Environment.setEnvironVar "AZURE_CLIENT_SECRET" azureOptions.ClientSecret

            let armClient = ArmClient.create

            let! response =
                ResourceGroupResource.createResourceIdentifier azureOptions.SubscriptionId azureOptions.ResourceGroup
                |> armClient.GetResourceGroupResource
                |> _.GetAsync()
                |> Async.AwaitTask

            let resourceGroup = response.Value

            let! storageAccount = resourceGroup |> ResourceGroupResource.getStorageAccount accountName

            do! storageAccount |> StorageAccountResource.delete
        }
