Tests for Expand:
* Make sure that for any field present in a given resource, if it has a standard relationship that it can be expanded.
* Test the related resource to ensure records are present and the same in both cases.
* For HistoryTransactional, the server may limit or even not return the records, even if they have the HistoryTransactional resource. If we find data there during testing, we will record it, otherwise we won't.
