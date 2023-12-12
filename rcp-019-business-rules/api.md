## Introduction

This document describes how to retrieve a set of rules from an originating system using the Web API.

## Specification

A service MAY provide business rules to permit complex validation of data before submission to the service. Clients should use these validation rules to provide the user with a more responsive experience. A service MAY have additional rules that are not provided to the client and SHOULD validate any changes to an Entity on the service when the client submits a changed Entity.


Validation Rules are for a specific Resource and MAY be constrained by the client application, role or other system specific limitation.

Validation Rules for a Resource will change over time. To allow the evolution of the rules, an opaque token is provided to uniquely identify the state of the set of rules. Clients MAY cache Validation Rules.


|  |  |  |
|:-|:-|:-|
| vrHash | base64 | A base64 opaque token, calculated in the service to uniquely identify the state of the Validation Rule set |

Validation Rules are returned as a JSON response body from the service. The response body includes the for that rule set.vrHash

Accessing a rule set must adhere to the OData standard taking the following form:

```
https://odata.reso.org/RESO/OData/[Resource]/ValidationRules('<vrHash|null>')
```

A service should treat the endpoint `./ValidationRules` identically to the endpoint `./ValidationRules('')`

Changing rules, roles or other constraints MUST change the `vrHash` value returned by the service. If a client submits a stale `vrHash`, the client SHOULD use the Validation Rule set returned in the response body and discard any cached validation rules. A client SHOULD submit the `vrHash` on a ValidationRules request to allow the service to limit the response body to just the `vrHash`. That is, when the request matches that `vrHash` of the service, the service MAY return only the `vrHash` in the response body.

The rule set may be further filtered by adding the action that the client will use the rule set for. The action is defined in section 2.7.8.4.1.

```
https://odata.reso.org/RESO/OData/[Resource]/ValidationRules('<vrHash|null>')?$filter=action
```

The response body is JSON formatted consisting of the `ruleSet`, which is an ordered list of validation expressions, and the `ruleHash`. The `ruleSet` is the collection of rules the service provides for the selected Resource during creation or modification of an Entity of that Resource type. The `ruleSet` is optional, since there may be no rules available for the client for that Resource. In this case, the service MUST return an HTTP status of 204.

The request below returns the collection of validation expression rules for the Property resource:all

Request:

```
GET RESO/OData/Property/ValidationRules(‘32248c144')
```

Response body:

```http
HTTP/1.1 200 OK
Content-Type: application/json; odata.metadata=minimal; odata.
streaming=true
OData-Version: 4.0
Date: Sat, 28 Jun 2018 01:05:32 GMT
Content-Length: xxxx

{
    "@odata.context": "https://odata.reso.org/RESO/OData/Property/ValidationRules('32248c144')",
    "value": {
        "vrHash": "667qa3321158",
        "ruleSet": [
            {
                "sequence": 1,
                "field": "ListPrice",
                "action": "SET_REQUIRED",
                "expression": ".TRUE.",
                "message": "ListPrice is Required."
            },
            {
                "sequence": 2,
                "field": "ListingId",
                "action": "REJECT",
                "expression": "UserLevel != 'Admin' .AND. ListPrice <= 0",
                "message": "ListPrice must be greater than zero."
            }
        ]
    }
}
```

The ValidationRules URL allows filtering by UpdateAction, where update_action is one of those specified in 2.7.10.

```
GET Property/ValidationRules('32248c144')?$filter=update_action eq 'Add'
```

The response is similar to that shown above, except that the rules will be only those for the requested update_action.
