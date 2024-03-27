# RESO EntityEvent Resource and Replication Model

| **RCP** | 27                                                                                                                       |
| :--- |:-------------------------------------------------------------------------------------------------------------------------|
| **Version** | **2.0.1**                                                                                                                |
| **Authors** | Joshua Darnell ([RESO](mailto:josh@reso.org))<br />Paul Stusiak ([Falcon Technologies](pstusiak@falcontechnologies.com)) | 
| **Status** | **RATIFIED**                                                                                                             |
| **Date Submitted** | January 2019                                                                                                             |
| **Date Approved** | December 2023                                                                                                            |
| **Dependencies** | [Data Dictionary 2.0 EntityEvent Resource](https://ddwiki.reso.org/display/DDW20/EntityEvent+Resource)                   |
| **Related Links** | [Push Events using Web Hooks](./web-hooks-push.md)                                                                       |

The RESO _EntityEvent_ Resource provides an efficient and streamlined way to replicate data by using the interface of an append-only log.

# Synopsis
A new standard resource called _EntityEvent_ and method for replication is described. 

# Rationale
One of the most common business cases for real estate data is the replication of listing and other data between producer and consumer sources. 

The current state of the art is to use modification timestamps, long polling and state inference to synchronize data between two servers. 
This scheme places an additional burden on the consumer of data to know the state of the producer and coordinate timestamps across vendors. 

This results in a need to frequently resynchronize the data on a because it is difficult or impossible to resolve timestamps or record state. 

This proposal addresses some of the problems with the current scheme by replacing timestamps with an event log and providing an _EntityEvent Resource_ 
that exposes the event log.

# Proposal
A new Data Dictionary resource called _EntityEvent_ will be created, as mentioned in the corresponding Data Dictionary proposal. 

This resource will provide a logical timestamp of events called _EntityEventSequence_ and the _ResourceName_ and _ResourceRecordKey_ of the 
item that was created, updated, or deleted.

The logical timestamp is an event identifier that denotes that a business event occurred. 

Business events are rules or actions that represent the business logic of the resources of the system. 

Examples of business events could be a listing price change, a listing status change, an phone number change for an agent and the 
addition or deletion of a photo to any other resource that has media object. These business events and the associated logic exists 
in current systems implementing the Web API today. By representing these events in a consistent way, eventual record state consistency can be achieved.

A new well-known name, _EntityEventSequence_ will be added. _EntityEventSequence_ is part of an event record that describes all events for all 
resource in order. This event record is a compact representation of an event that occurred. 

_EntityEventSequence_ is a durable, immutable, monotonic identifier that preserves the order that events occur in a system. It can only increase in value. 

A value for _EntityEventSequence_ that is arithmetically less than another value, b, is defined such that event a occurs **_earlier_** in time than event b. 

For example, An event with EntityEventSequence=200 (a), compared with another event with EntityEventSequence=1001 (b), 
satisfies the condition that _event a_ occurred before _event b_.

An event record combines the _EntityEventSequence_, the _ResourceName_ and the _ResourceRecordKey_ to indicate that a business event has occurred 
on a system. 

The event record is part of a well-known resource _EntityEvent_ that represents all events that have occurred on a system.

## Entity Events
The _EntityEvent_ resource provides a service that returns event records as defined in this proposal. 
* A producer MUST provide a Resource named _EntityEvent_.
* An EntityEvent record has three fields, the _EntityEventSequence_, the _ResourceName_, and _ResourceRecordKey_ that uniquely identify the given record.
* The data type of _EntityEventSequence_ is the positive portion of int64.
* The data type of _ResourceName_ is string. It is the well-known name for the resource type.
* The data type of _ResourceRecordKey_ is the unique identifier for a specific record within the given resource and must be a string.
* The _EntityEventSequence_ MUST conform to the property that is is immutable and monotonic. 
* _EntityEventSequence_ numbers MAY be hidden from feeds as long as at least one later record contains a final record for that resource name and key.


The _EntityEvent_ resource MAY contain all events that occur in a system or may contain events limited to those events or resources 
that a consumer has permission to view. 

The existence of an identified resource in _EntityEvent_ does not change the visibility of the resource that generated the underlying record. 
That is controlled by the producer and the permission model they have implemented.

### Workflows
Certain limitations are applied to the normal workflow of producing and consuming entities.

#### Producers
Producers MUST order _EntityEvent_ records in the logical order that they occurred. _EntityEventSequence_ must have the property that it is unique, 
always increases in value and cannot change. 

Requesting the same _EntityEventSequence_ MUST result in the same _ResourceName_ and _ResourceRecordKey_. 

When possible, providers should order events to simplify referential integrity for data consumers so they can replay events without 
underlying underlying business knowledge of internal system relationships. 

For example, the addition of a Property Resource event should occur before any Media Resource events associated with the 
Property appearing in the Events Resource.

#### Consumers
Consumers use standard workflows for replication.

**Example**

A consumer wishes to get all the events after the EntityEventSequence of 100.

```
GET /EntityEvent?$filter=EntityEventSequence gt 100
```
```
{
  "@odata.context": "/EntityEvent?$filter=EntityEventSequence gt 100",
  "value": [
    {
      "EntityEventSequence": 101,
      "ResourceName": "Member",
      "ResourceRecordKey": "21"
    },
    {
      "EntityEventSequence": 103,
      "ResourceName": "Property",
      "ResourceRecordKey": "539"
    },
    {
      "EntityEventSequence": 110,
      "ResourceName": "Media",
      "ResourceRecordKey": "1239"
    }
  ]
}
```

## Replication

One of the business cases for the EntityEvent Resource is replication. This is an optional, but important, part of the proposal. This section outlines related considerations.

### Business Cases

There are two types of consumers and two cases comprise the workflow of a consumer. 

* Most consumers will be synchronizing with the most current state of the system. 
* A much smaller number of consumers will be collecting state changes throughout history to create analytics.

#### Initial Synchronization

The consumer is gathering each _EntityEvent_ record for the first time. Based on rules created by the producer or by the consumer, the consumer starts at the lowest
value _EntityEventSequence_ number and processes each record.


##### Example: Get All Records a Consumer Has Access To

**Request**
```
GET /EntityEvent?$filter=EntityEventSequence ge 0
```

This will return the first EntityEventSequence the client has access to. This could be either 0 or 10,000, etc.

If the provider supports Web API Core 2.1.0+ or Data Dictionary 2.0+, `@odata.nextLink` will present in the response until all data is returned.

#### Ongoing Synchronization

The consumer has previously been in synchronization and needs to 'catch up' with the current state of the system. 

#### Handling Record Visibility
Based on business rules, EntityEvent records may change the availability and visibility for consumers based on the role of a consumer. 

For example, a consumer who had a role to receive IDX listings may not be permitted to only view listings that have a status of 'ACTIVE'. 

A consumer with this role would see an event that changes status and would then attempt to retrieve the record from the producer 
only to receive no record. 

This can be used by the consumer to know that the _ResourceRecordKey_ they are attempting to retrieve is no longer part of the permitted 
records for their system and could take appropriate steps to remove that _ResourceRecordKey_ from their visible record set.

#### Consumer Visibility Workflow

A consumer asks for the current set of _EntityEvent_ records.

In this set is an event that changes a record that the consumer has from visible to not visible - we can think of this as a consumer delete event. 

The consumer does not know what has happened to the record, only that the record has changed state. 

When the consumer makes a normal query against the appropriate Resource, they will get a "No Record Found" (`HTTP 404`) response 
and can infer that the record and the child resources should be deleted. 

Based on earlier sections of this proposal, the child records should have appeared at a lower _EntityEventSequence_ value than the parent record. 

This allows the consumer to remove references rather than deal with cascading deletes. 

# Impact
Producers should expose a new resource that implements the requirements of the proposal.

# Compatibility
As new functionality, this does not affect Compatibility.

# Certification
Additional test rules will be needed.
* Check metadata for the existence of an _EntityEvent_ resource of the form _EntityEventSequence_, _ResourceName_, _ResourceRecordKey_. 
* Additional fields in an _EntityEvent Resource_ will be ignored by the compliance test.
* Confirm that data is returned for a GET on the _EntityEvent_ Resource.
* Confirm that for a sample set that any two records have sequentially increasing _EntityEventSequence_ values.
* Confirm that for a sample set of _EntityEvent_ records, that all records only contain _ResourceName_ items that are defined in the metadata.
* Confirm that the _EntityEvent Resource_ response is well formed.
* Confirm that for each record in a sample set of _EntityEvent_ records, that either a record with _ResourceName_ and _ResourceRecordKey_ is returned OR an `HTTP 400` response value indicating the records are no longer available.
* Test sequence order.
* Confirm that requesting the same _EntityEventSequence_ results in the same combination of _ResourceName_ and _ResourceRecordKey_.

# Original Proposal
[**Download PDF**](https://github.com/RESOStandards/transport/files/9862609/RESOWebAPIRCP-RCP.-.WEBAPI-027.Event.Resource.and.Replication.Model-251022-173840.pdf)
