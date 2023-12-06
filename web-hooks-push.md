# Push Replication Using Web Hooks and the RESO EntityEvent Resource

| **RCP** | 28 |
| :--- | :--- |
| **Version** | **1.0.0** |
| **Authors** | Joshua Darnell ([RESO](mailto:josh@reso.org))<br />Paul Stusiak ([Falcon Technologies](pstusiak@falcontechnologies.com)) | 
| **Status** | **APPROVED** |
| **Date Submitted** | April 2019 |
| **Date Approved** | December 2023 |
| **Dependencies** | [Data Dictionary 2.0 EntityEvent Resource](https://ddwiki.reso.org/display/DDW20/EntityEvent+Resource) |
| **Related Links** | [RCP-027](https://github.com/RESOStandards/transport/blob/45-migrate-rcp-027-from-confluence/entity-events.md)[Web Hooks](https://en.wikipedia.org/wiki/Webhook) |

The RESO _EntityEvent Resource_ provides an efficient and streamlined way to replicate data by using the interface of an append-only log.

This proposal adds the ability to push events from the EntityEvent Resource using web hooks.

# Synopsis
A mechanism for Producers pushing EntityEvents to Consumers using the EntityEvents Resource proposed in [RCP-027](https://raw.githubusercontent.com/RESOStandards/transport/45-migrate-rcp-027-from-confluence/entity-events.md).


# Rationale
In addition to allowing Consumers to be able to poll on the EntityEvents Resource, 
it's useful for the purposes of replication to allow Producers of resource data to be able to _push_ data about those events to 
Consumers using an API endpoint and token provided by the Consumer.

# Proposal
Section 2.6.2 - Push Replication of EntityEvents using Web Hooks
When a (EntityEventSequenceNumeric, ResourceName, ResourceRecordKey, ResourceRecordKeyNumeric, ResourceRecordURL) tuple has been published to the EntityEvents resource, Producers MAY "push" these data to Consumers as events for the purposes of replication. In doing so, the replication process on the Consumer is made much simpler, as the Consumer need only to listen for EntityEvents being received from the Producer, subsequently picking up the data corresponding to the EntityEvent from the Producer's Web API. In general, this is preferable to having to continuously poll the Producer's API for new records. This proposal is intended to be optional at the moment.

## Requirements
There are a few things required of Consumers and Producers before communication may proceed:
* Consumers MUST have implemented an API endpoint that can receive EntityEvents and have made it available to the Producer.
* The Consumer MUST have provided the Producer with a long-lived token that will allow the Producer to POST EntityEvents to the Consumer's API.
* Producers will use Web Hooks for the push replication mechanism along with the EntityEvent payload.

## Polite Behavior
It's important that Consumers and Producers follow the best practices outlined in the section of the replication document entitled Polite Behavior:
* If the Producer decides that the Consumer is taking "too long" to process a request, 
the Producer may choose to abandon the POST request and call back at a later time.
* If the Producer is sending data to the Consumer too quickly, the Consumer may respond with an `HTTP 429` response code signifying that the Producer halt the transfer of data to the Consumer.
* The Consumer may also provide a `RETRY-AFTER` header, either specifying a Date or duration in Seconds, after which the Producer will attempt to resume transfer of data to the Consumer.
If the Producer fails after some number of retries, to be determined by the Producer, then they MAY stop the flow of data to the Consumer. To resume push from the Producer, the Consumer MAY have to POST data to the Producer's endpoint.

Polite Behavior is especially important when the Consumer is initializing a new replication feed, 
as there may be a large number of EntityEvents to process and additional work to be done on the Consumer before subsequent 
EntityEvents may be processed. 

## Consumer Labels for EntityEvent Sources
Optionally, Consumers may choose to provide a custom identifier that will allow them to distinguish between feeds 
from multiple sources from the same Producer. 

This identifier will be passed in the headers as something like "ConsumerLabel," if present, and is assumed to be String (255).

# Impact
Producers MAY push events from the _EntityEvent Resource_ specified in RCP-027 using RESTful API calls to the Consumer's API using Web Hooks. The expected authentication mechanism for doing so will be long-lived bearer tokens provided to the Producer by the Consumer. Consumers are expected to maintain APIs that can respond in a "reasonable" amount of time, to be determined by the Producer and Consumer in each case.  

The technologies used for this proposal are:
* RESTful API calls
* Long-lived Bearer tokens

As such, this proposal doesn't introduce any new technologies.

# Compatibility
Compatible with Web API v1.0.2+ and any JSON Web API.

# Certification Impact
As this proposal is optional, there is no certification impact at this time. 

# Original Proposal
[**Download PDF**](https://github.com/RESOStandards/transport/files/9862869/RESOWebAPIRCP-RCP.-.WEBAPI-028.Push.Replication.from.the.EntityEvent.Resource-251022-181630.pdf)

