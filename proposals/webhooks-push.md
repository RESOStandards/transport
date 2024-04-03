# Push Replication Using Webhooks and the RESO EntityEvent Resource

| **RCP** | 28 |
| :--- | :--- |
| **Version** | **1.0.1** |
| **Authors** | Joshua Darnell ([RESO](mailto:josh@reso.org))<br />Paul Stusiak ([Falcon Technologies](pstusiak@falcontechnologies.com)) | 
| **Status** | **RATIFIED** |
| **Date Submitted** | April 2019 |
| **Date Approved** | December 2023 |
| **Dependencies** | [Data Dictionary 2.0 EntityEvent Resource](https://ddwiki.reso.org/display/DDW20/EntityEvent+Resource) <br />[RESO Common Format](reso-common-format.md) |
| **Related Links** | [RCP-027](./entity-events.md) <br />[Webhooks](https://en.wikipedia.org/wiki/Webhook) |

# Synopsis
The RESO _EntityEvent Resource_ provides an efficient and streamlined way to replicate data using the interface of an append-only log.

This proposal adds the ability to push events from the EntityEvent Resource using webhooks.

# Rationale
Rather than polling on the EntityEvent Resource for updates, it's useful for data producers to push events to consumer APIs. 

# Proposal
When an _EntityEventSequenceNumeric_, _ResourceName_, _ResourceRecordKey_ tuple has been published to the EntityEvent resource, providers MAY "push" events to data consumers for replication or other event sourcing applications. 

In doing so, the replication process becomes simpler for the consumer, as they only need to receive EntityEvents from the producer and pick up the corresponding records from provider APIs. In general, this is preferable to having to poll continuously for new records.

# Examples
The following examples show using the EntityEvent model along with [RESO Common Format](reso-common-format.md) , [OAuth2 bearer tokens](https://oauth.net/2/bearer-tokens/), and HTTP POST requests.

RESO Common Format supports both single and multi-valued payloads.

## 1. Producer Sends a Single EntityEvent to the Consumer API

**REQUEST**
```
POST https://example.api.com/webhooks
  Content-Type: application/json
  Authorization: Bearer <token>
```
```json
{
  "@reso.context": "urn:reso:metadata:2.0:resource:entityevent",
  "EntityEventSequence": 1101,
  "ResourceName": "Showing",
  "ResourceRecordKey": "235021"
}
```

**RESPONSE**
```
HTTP/2 200
Content-Type: application/json
```

## 2. Producer Sends a Batch of EntityEvent Records to the Consumer API

**REQUEST**
```
POST https://example.api.com/webhooks
  Content-Type: application/json
  Authorization: Bearer <token>
```
```json
{
  "@reso.context": "urn:reso:metadata:2.0:resource:entityevent",
  "value": [{
    "EntityEventSequence": 1101,
    "ResourceName": "Property",
    "ResourceRecordKey": "235021"
  }, {
    "EntityEventSequence": 1111,
    "ResourceName": "Media",
    "ResourceRecordKey": "ABC123"
  }]
}
```

**RESPONSE**
```
HTTP/2 200
Content-Type: application/json
```

## 3. Producer Sends a Batch of EntityEvent Records to a Consumer API With Retry-After

**REQUEST**
```
POST https://example.api.com/webhooks
  Content-Type: application/json
  Authorization: Bearer <token>
```
```json
{
  "@reso.context": "urn:reso:metadata:2.0:resource:entityevent",
  "value": [{
    "EntityEventSequence": 1101,
    "ResourceName": "Property",
    "ResourceRecordKey": "235021"
  }, {
    "EntityEventSequence": 1111,
    "ResourceName": "Media",
    "ResourceRecordKey": "ABC123"
  }]
}
```

**RESPONSE**
```
HTTP/2 429
Content-Type: application/json
Retry-After: 3600
```

## 4. Producer Sends a Batch of EntityEvent Records to a Consumer API with Optional EntityEventSource Header

**REQUEST**
```
POST https://example.api.com/webhooks
  Content-Type: application/json
  Authorization: Bearer <token>
  EntityEventSource: IDX Feed 123
```
```json
{
  "@reso.context": "urn:reso:metadata:2.0:resource:entityevent",
  "value": [{
    "EntityEventSequence": 1101,
    "ResourceName": "Property",
    "ResourceRecordKey": "235021"
  }, {
    "EntityEventSequence": 1111,
    "ResourceName": "Media",
    "ResourceRecordKey": "ABC123"
  }]
}
```

**RESPONSE**
```
HTTP/2 200
Content-Type: application/json
```


## Requirements
* Consumers MUST implement an API endpoint that can receive EntityEvents.
* Consumers MUST establish authorization with the provider, using an OAuth2 bearer token.
* Producers MUST use an HTTP POST request to the consumer's API with an EntityEvent payload and the given credentials.

## Polite Behavior
It's important that consumers and producers practice 'polite behavior':
* If the producer decides the consumer is taking "too long" to process a request, they MAY choose to abandon it and call back at a later time.
* If the producer is sending data to the consumer too quickly, the consumer MAY respond with an `HTTP 429` response code, indicating the producer should halt the transfer of data to the consumer.
* The consumer MAY provide a [`Retry-After` header](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Retry-After), either specifying a date or duration in seconds, after which the producer will attempt to resume transfer of data to the consumer.
* If the producer fails after some number of retries, to be determined by the producer, they MAY stop the flow of data to the consumer. To resume push from the producer, the consumer MAY have to POST data to the producer's endpoint to resume the feed.

Polite Behavior is especially important when the consumer is initializing a new replication feed, as there may be a large number of EntityEvents to process and additional work to be done on the consumer before subsequent EntityEvents may be processed. Consumers SHOULD accept and queue events rather than blocking and trying to process them as they arrive.

## Consumer Labels for EntityEvent Sources
Optionally, consumers may choose to provide a custom identifier that will allow them to distinguish between feeds from multiple sources from the same producer. 

This identifier will be passed in the headers as "EntityEventSource," if present, and is assumed to be String (255).

# Impact
Producers MAY push events from the _EntityEvent Resource_, specified in RCP-027, using POST requests to the consumer's API. 

The expected authorization mechanism for doing so will be long-lived bearer tokens provided to the producer by the consumer. Consumers are expected to maintain APIs that can respond in a "reasonable" amount of time.

This proposal doesn't introduce any new technologies since the RESO Web API already uses OAuth2 bearer tokens and HTTP requests.

# Compatibility
Compatible with Data Dictionary 2.0+ and JSON Web API. Providers SHOULD also host an EntityEvent Resource so consumers may replay events as needed.

# Certification Impact
As this proposal is optional, there is no certification impact at this time. 

# Original Proposal
[**Download PDF**](https://github.com/RESOStandards/transport/files/9862869/RESOWebAPIRCP-RCP.-.WEBAPI-028.Push.Replication.from.the.EntityEvent.Resource-251022-181630.pdf)
