# RESO EntityEvent Resource and Replication Model

| **RCP** | 27 |
| :--- |:--- |
| **Version** | **2.0.2** |
| **Authors** | Joshua Darnell ([RESO](mailto:josh@reso.org))<br />Paul Stusiak ([Falcon Technologies](pstusiak@falcontechnologies.com)) | 
| **Status** | **RATIFIED** |
| **Date Submitted** | January 2019 |
| **Date Approved** | December 2023 |
| **Dependencies** | [Data Dictionary 2.0 EntityEvent Resource](https://ddwiki.reso.org/display/DDW20/EntityEvent+Resource) |
| **Related Links** | [Push Events using Webhooks](./webhooks-push.md) |

# Synopsis
The RESO EntityEvent Resource provides an efficient way to represent event streams and replicate data by using the interface of an append-only log.

# Rationale
One of the most common business cases for real estate data is the replication of listing and other data between producers and consumers.

The current state of the art is to use modification timestamps, long polling, and state inference to synchronize data. This places an additional burden on the data consumer to know the state of the producer and coordinate timestamps across systems and vendors. It also results in the need to frequently resynchronize data, as it can become difficult or impossible to resolve timestamps or reconcile the current state of a given system.

# Proposal

A new Data Dictionary resource named [_EntityEvent_](https://ddwiki.reso.org/display/DDW20/EntityEvent+Resource) has been created.

This resource provides a field called [_EntityEventSequence_](https://ddwiki.reso.org/display/DDW20/EntityEventSequence+Field), which is the logical timestamp of a given event, as well as the [_ResourceName_](https://ddwiki.reso.org/pages/viewpage.action?pageId=1135897) and [_ResourceRecordKey_](https://ddwiki.reso.org/display/DDW20/ResourceRecordKey+Field) of the item that was created, updated, or deleted. An optional field called [_ResourceRecordUrl_](https://ddwiki.reso.org/display/DDW20/ResourceRecordUrl+Field) allows systems to provide opaque URLs that can be used to fetch records for each EntityEvent.

The logical timestamp is an event identifier that denotes that a business event occurred. Business events are rules or actions that represent the business logic of the resources of the system.

Examples of business events could be a listing price change, a listing status change, an phone number change for an agent, or the addition or deletion of a photo to any other resource that has a media object. These business events and the associated logic exists in current systems implementing the Web API today. By representing these events in a consistent way, eventual record state consistency can be achieved.

EntityEventSequence is a durable, immutable, monotonic identifier that preserves the order that events occur in a system. It can only increase in value.

Given any EntityEventSequence values `e1` and `e2`, if `e1` < `e2` we can say `e1` occurred _before_ `e2`. For example, let `e1 = 200` and `e2 = 1001`.

## Entity Events
The _EntityEvent_ resource provides a service that returns event records as defined in this proposal. 
* A producer MUST provide a Resource named _EntityEvent_.
* An EntityEvent record has three required fields, _EntityEventSequence_, _ResourceName_, and _ResourceRecordKey_, which uniquely identify the given record.
* The data type of _EntityEventSequence_ is the positive portion of int64.
* The data type of _ResourceName_ is string. It is the well-known name for the resource type.
* The data type of _ResourceRecordKey_ is the unique identifier for a specific record within the given resource and MUST be a string.
* The _EntityEventSequence_ MUST conform to the property that is immutable and monotonic. 
* _EntityEventSequence_ numbers MAY be hidden from feeds as long as at least one later record contains a final record for that resource name and key.


The _EntityEvent_ resource MAY contain all events that occur in a system or may contain events limited to those events or resources 
that a consumer has permission to view. 

The existence of an identified resource in _EntityEvent_ does not change the visibility of the resource that generated the underlying record. 
That is controlled by the producer and the permission model they have implemented.

### Workflows
Certain limitations are applied to the normal workflow of producing and consuming entities.

#### Producers
Producers MUST order _EntityEvent_ records in the logical order that they occurred. _EntityEventSequence_ must have the property that it is unique, 
always increases in value, and cannot change. 

Requesting the same _EntityEventSequence_ MUST result in the same _ResourceName_ and _ResourceRecordKey_. 

When possible, providers should order events to simplify referential integrity for data consumers so they can replay events without 
underlying business knowledge of internal system relationships. 

For example, the addition of a Property Resource event should occur before any Media Resource events associated with the 
Property appearing in the Events Resource.

#### Consumers
Consumers use standard workflows for replication.

**Example**

A consumer wishes to get all the events after the EntityEventSequence of 100.

```
GET /EntityEvent?$filter=EntityEventSequence gt 100
```
```json
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

This will return the _first EntityEventSequence the client has access to_. This could be either 0 or 10,000, etc.

If the provider supports Web API Core 2.1.0+ or Data Dictionary 2.0+, `@odata.nextLink` will be present in the response until all EntityEvent data is returned.

#### Ongoing Synchronization

The consumer has previously been in synchronization and needs to 'catch up' with the current state of the system. 

#### Handling Record Visibility
Based on business rules, EntityEvent records may change the availability and visibility for consumers based on the role of a consumer. 

For example, a consumer who had a role to receive IDX listings may not be permitted to only view listings that have a status of 'Active'. 

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
As new functionality, this does not affect existing standards.

# Certification
Additional testing rules beyond those in the Data Dictionary:
* Check metadata for the existence of an _EntityEvent_ with required fields: _EntityEventSequence_, _ResourceName_, and _ResourceRecordKey_.
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
