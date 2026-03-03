import { describe, it, expect } from "vitest";
import { buildUri } from "../src/uri/builder.js";

describe("buildUri", () => {
  const base = "http://localhost:8080";

  it("builds a simple resource URL", () => {
    expect(buildUri(base, "Property").build()).toBe(
      "http://localhost:8080/Property",
    );
  });

  it("strips trailing slash from base URL", () => {
    expect(buildUri("http://localhost:8080/", "Property").build()).toBe(
      "http://localhost:8080/Property",
    );
  });

  it("adds key syntax", () => {
    expect(buildUri(base, "Property").key("ABC123").build()).toBe(
      "http://localhost:8080/Property('ABC123')",
    );
  });

  it("URI-encodes key values", () => {
    expect(buildUri(base, "Property").key("key with spaces").build()).toBe(
      "http://localhost:8080/Property('key%20with%20spaces')",
    );
  });

  it("adds $select", () => {
    const url = buildUri(base, "Property")
      .select("ListPrice", "City")
      .build();
    expect(url).toBe(
      "http://localhost:8080/Property?$select=ListPrice,City",
    );
  });

  it("adds $filter", () => {
    const url = buildUri(base, "Property")
      .filter("ListPrice gt 200000")
      .build();
    expect(url).toContain("$filter=");
    expect(url).toContain("ListPrice");
  });

  it("adds $orderby", () => {
    const url = buildUri(base, "Property")
      .orderby("ListPrice desc")
      .build();
    expect(url).toContain("$orderby=");
  });

  it("adds $top", () => {
    const url = buildUri(base, "Property").top(10).build();
    expect(url).toBe("http://localhost:8080/Property?$top=10");
  });

  it("adds $skip", () => {
    const url = buildUri(base, "Property").skip(20).build();
    expect(url).toBe("http://localhost:8080/Property?$skip=20");
  });

  it("adds $count", () => {
    const url = buildUri(base, "Property").count().build();
    expect(url).toBe("http://localhost:8080/Property?$count=true");
  });

  it("combines multiple query options", () => {
    const url = buildUri(base, "Property")
      .select("ListPrice", "City")
      .filter("ListPrice gt 200000")
      .orderby("ListPrice desc")
      .top(10)
      .skip(0)
      .count()
      .build();
    expect(url).toContain("$select=ListPrice,City");
    expect(url).toContain("$top=10");
    expect(url).toContain("$skip=0");
    expect(url).toContain("$count=true");
  });

  it("combines key with query options", () => {
    const url = buildUri(base, "Property")
      .key("ABC")
      .select("ListPrice")
      .build();
    expect(url).toBe(
      "http://localhost:8080/Property('ABC')?$select=ListPrice",
    );
  });

  it("adds $expand", () => {
    const url = buildUri(base, "Property").expand("Media").build();
    expect(url).toBe("http://localhost:8080/Property?$expand=Media");
  });

  it("adds $expand with nested options", () => {
    const url = buildUri(base, "Property")
      .expand("Media($select=MediaURL,MimeType)")
      .build();
    expect(url).toContain("$expand=");
    expect(url).toContain("Media");
  });

  it("adds compound key", () => {
    const url = buildUri(base, "OrderLine")
      .compoundKey({ OrderId: "123", LineNumber: "1" })
      .build();
    expect(url).toBe(
      "http://localhost:8080/OrderLine(OrderId='123',LineNumber='1')",
    );
  });

  it("adds $search", () => {
    const url = buildUri(base, "Property").search("luxury pool").build();
    expect(url).toContain("$search=");
    expect(url).toContain("luxury");
  });

  it("adds $compute", () => {
    const url = buildUri(base, "Property")
      .compute("ListPrice mul 1.1 as AdjustedPrice")
      .build();
    expect(url).toContain("$compute=");
  });

  it("adds $format", () => {
    const url = buildUri(base, "Property").format("json").build();
    expect(url).toContain("$format=json");
  });

  it("is immutable — chaining returns new builder", () => {
    const builder = buildUri(base, "Property");
    const withKey = builder.key("ABC");
    const withTop = builder.top(10);

    expect(builder.build()).toBe("http://localhost:8080/Property");
    expect(withKey.build()).toBe("http://localhost:8080/Property('ABC')");
    expect(withTop.build()).toBe("http://localhost:8080/Property?$top=10");
  });
});
