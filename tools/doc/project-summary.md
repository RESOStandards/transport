# RESO Transport Tools — Project Summary

## What Is This Project?

The Real Estate Standards Organization (RESO) sets the rules for how real estate data is shared between systems — think of it like a universal language that MLSs (Multiple Listing Services), brokerages, and technology companies use so their software can talk to each other. When you search for homes on Zillow, Realtor.com, or your local MLS website, the data flowing behind the scenes follows RESO standards.

This project builds **reference tools** that help the real estate technology industry adopt and test against those standards. It is a comprehensive toolkit that includes a working server, a web interface, test data generation, compliance testing, and reusable code libraries.

---

## What We Built

### 1. A Working Reference Server

We built a fully functional server that speaks the OData protocol — the industry-standard way real estate systems communicate. It handles all 13 major real estate data types defined by RESO:

- **Property** — Listings with 400+ fields (price, bedrooms, location, features, etc.)
- **Member** — Real estate agents and brokers
- **Office** — Brokerage offices
- **Media** — Photos and virtual tours
- **Open House** — Scheduled showings
- **Contacts, Teams, Showings** — CRM and operational data
- **Property Rooms, Unit Types** — Detailed property breakdowns
- **Internet Tracking, Saved Searches, Prospecting** — User activity data
- **Lookups** — The controlled vocabularies (like "Active", "Pending", "Sold" for listing status)

The server supports three different database backends (PostgreSQL, MongoDB, and SQLite), runs in Docker containers for easy deployment, and handles advanced querying — filtering, sorting, pagination, field selection, and expanding related records.

### 2. A Web-Based User Interface

A modern web application that lets users:

- Browse any of the 13 resources with paginated tables
- Create new records with smart form validation
- Edit existing records
- Navigate between related records (e.g., jump from a listing to its agent)
- See real-time validation feedback (required fields, data format checks, business rules like "list price must be greater than or equal to low list price")

### 3. A Test Data Generator

A tool that creates realistic-looking real estate data for testing purposes. It understands the relationships between different data types — for example, it knows that a Property listing needs to reference a valid Member (agent) and Office (brokerage), so it creates those first and links them correctly. It generates data for all 1,727 fields across all resources, including 3,611 controlled vocabulary values (lookup fields like property types, status values, etc.).

### 4. Automated Compliance Testing

We integrated with RESO's official certification tools (the RESO Commander) to run automated compliance checks. The system:

- Generates test configurations automatically from the server's metadata
- Runs 42 Web API Core 2.0.0 specification tests (all passing)
- Runs 928 Data Dictionary 2.0 field-level compliance checks
- Supports both Docker-based and local testing workflows
- Tests both enumeration modes (string-based and OData EnumType — see below)

### 5. Reusable Code Libraries

We built several standalone libraries that can be used independently:

- **Validation Library** — Business rules and field validation that works in both browsers and servers
- **OData Filter Parser** — Parses query filter expressions (like "ListPrice gt 500000 and City eq 'Denver'") with zero external dependencies
- **OData Client SDK** — A developer-friendly toolkit for building applications that consume RESO data

### 6. Two Enumeration Modes

Real estate data uses thousands of standardized values (like property types: "Residential", "Commercial", "Land", etc.). We support two different ways of representing these:

- **String Mode** — Values appear as human-readable text ("Active Under Contract") with a Lookup Resource that catalogs all valid values
- **EnumType Mode** — Values appear as formal OData enumeration types with PascalCase names ("ActiveUnderContract") embedded directly in the metadata schema

Both modes are fully functional and independently testable, giving implementers flexibility in how they build their systems.

---

## By the Numbers

| Metric | Value |
|--------|-------|
| Packages built | 7 |
| Automated tests | ~650 |
| Database backends | 3 (PostgreSQL, MongoDB, SQLite) |
| RESO resources supported | 13 |
| Data Dictionary fields | 1,727 |
| Lookup values | 3,611 |
| Web API Core tests passing | 42/42 |
| DD compliance checks | 928 |
| Releases | 23 (v0.0.1 – v0.0.23) |

---

## Why It Matters

This toolkit gives the real estate technology industry a **working, testable reference implementation** of the RESO standard. Developers building MLS systems, real estate platforms, or data integrations can:

- See exactly how a compliant server should behave
- Test their own implementations against the same compliance checks
- Use the reusable libraries in their own projects
- Generate realistic test data without needing access to real MLS data

It turns the RESO specification from a document into a living, running system that anyone can spin up, explore, and test against.
