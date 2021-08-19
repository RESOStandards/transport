Tests for Expand:
* Make sure that if a server has the field and related resource, that the navigation properties exist and `$expand` is supported.
* Test the related resource to ensure records are present and the same in both cases.
* For HistoryTransactional, the server may limit or even not return the records, even if they have the HistoryTransactional resource. If we find data there during testing, we will record it, otherwise we won't.
* For all advertised navigation properties, then expand will be tested.
