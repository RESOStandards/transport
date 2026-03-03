import { readFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { describe, expect, it } from 'vitest';
import { getEntityType, getEnumType, parseCsdlXml } from '../src/csdl/parser.js';

// Minimal EDMX for unit tests (includes NavigationProperty)
const minimalEdmx = `<?xml version="1.0" encoding="utf-8"?>
<edmx:Edmx Version="4.0" xmlns:edmx="http://docs.oasis-open.org/odata/ns/edmx">
  <edmx:DataServices>
    <Schema Namespace="org.reso.metadata" xmlns="http://docs.oasis-open.org/odata/ns/edm">
      <EnumType Name="StandardStatus">
        <Member Name="Active" Value="0" />
        <Member Name="Pending" Value="1" />
        <Member Name="Closed" Value="2" />
      </EnumType>
      <EntityType Name="Property">
        <Key>
          <PropertyRef Name="ListingKey" />
        </Key>
        <Property Name="ListingKey" Type="Edm.String" MaxLength="255" Nullable="false" />
        <Property Name="ListPrice" Type="Edm.Decimal" Precision="14" Scale="2" />
        <Property Name="City" Type="Edm.String" Nullable="true" />
        <Property Name="BedroomsTotal" Type="Edm.Int64" />
        <Property Name="StandardStatus" Type="org.reso.metadata.StandardStatus" />
        <NavigationProperty Name="Media" Type="Collection(org.reso.metadata.Media)" />
        <NavigationProperty Name="ListAgent" Type="org.reso.metadata.Member" Nullable="true" Partner="Listings" />
      </EntityType>
      <EntityType Name="Member">
        <Key>
          <PropertyRef Name="MemberKey" />
        </Key>
        <Property Name="MemberKey" Type="Edm.String" MaxLength="255" Nullable="false" />
        <Property Name="MemberFirstName" Type="Edm.String" />
        <NavigationProperty Name="Listings" Type="Collection(org.reso.metadata.Property)" Partner="ListAgent" />
      </EntityType>
      <EntityType Name="Media">
        <Key>
          <PropertyRef Name="MediaKey" />
        </Key>
        <Property Name="MediaKey" Type="Edm.String" MaxLength="255" Nullable="false" />
        <Property Name="MediaURL" Type="Edm.String" />
        <Property Name="ResourceRecordKey" Type="Edm.String" />
      </EntityType>
      <EntityContainer Name="Default">
        <EntitySet Name="Property" EntityType="org.reso.metadata.Property" />
        <EntitySet Name="Member" EntityType="org.reso.metadata.Member" />
        <EntitySet Name="Media" EntityType="org.reso.metadata.Media" />
      </EntityContainer>
    </Schema>
  </edmx:DataServices>
</edmx:Edmx>`;

// Extended EDMX with all new OData 4.01 features
const extendedEdmx = `<?xml version="1.0" encoding="utf-8"?>
<edmx:Edmx Version="4.0" xmlns:edmx="http://docs.oasis-open.org/odata/ns/edmx">
  <edmx:DataServices>
    <Schema Namespace="org.example" xmlns="http://docs.oasis-open.org/odata/ns/edm">
      <EnumType Name="Color">
        <Member Name="Red" Value="0" />
        <Member Name="Green" Value="1" />
        <Member Name="Blue" Value="2" />
      </EnumType>
      <EnumType Name="Permission" UnderlyingType="Edm.Int64" IsFlags="true">
        <Member Name="Read" Value="1" />
        <Member Name="Write" Value="2" />
        <Member Name="Execute" Value="4" />
      </EnumType>
      <ComplexType Name="Address">
        <Property Name="Street" Type="Edm.String" />
        <Property Name="City" Type="Edm.String" />
        <Property Name="State" Type="Edm.String" MaxLength="2" />
        <Property Name="PostalCode" Type="Edm.String" />
      </ComplexType>
      <ComplexType Name="ExtendedAddress" BaseType="org.example.Address" OpenType="true">
        <Property Name="Country" Type="Edm.String" />
      </ComplexType>
      <EntityType Name="BaseEntity" Abstract="true">
        <Key>
          <PropertyRef Name="Id" />
        </Key>
        <Property Name="Id" Type="Edm.Guid" Nullable="false" />
        <Property Name="CreatedAt" Type="Edm.DateTimeOffset" />
      </EntityType>
      <EntityType Name="Customer" BaseType="org.example.BaseEntity" OpenType="true">
        <Property Name="Name" Type="Edm.String" MaxLength="100" />
        <Property Name="HomeAddress" Type="org.example.Address" />
        <NavigationProperty Name="Orders" Type="Collection(org.example.Order)" Partner="Customer" ContainsTarget="true" />
      </EntityType>
      <EntityType Name="Order">
        <Key>
          <PropertyRef Name="OrderId" />
        </Key>
        <Property Name="OrderId" Type="Edm.Int64" Nullable="false" />
        <Property Name="Amount" Type="Edm.Decimal" Precision="10" Scale="2" />
        <Property Name="CustomerId" Type="Edm.Guid" />
        <NavigationProperty Name="Customer" Type="org.example.Customer" Partner="Orders">
          <ReferentialConstraint Property="CustomerId" ReferencedProperty="Id" />
        </NavigationProperty>
        <NavigationProperty Name="Items" Type="Collection(org.example.OrderItem)" />
      </EntityType>
      <EntityType Name="OrderItem">
        <Key>
          <PropertyRef Name="ItemId" />
        </Key>
        <Property Name="ItemId" Type="Edm.Int64" Nullable="false" />
        <Property Name="ProductName" Type="Edm.String" />
        <Property Name="Quantity" Type="Edm.Int32" />
      </EntityType>
      <EntityType Name="Document" HasStream="true">
        <Key>
          <PropertyRef Name="DocumentId" />
        </Key>
        <Property Name="DocumentId" Type="Edm.Guid" Nullable="false" />
        <Property Name="FileName" Type="Edm.String" />
        <Property Name="Content" Type="Edm.Stream" />
      </EntityType>
      <Action Name="ResetAllData" />
      <Action Name="ApproveOrder" IsBound="true" EntitySetPath="order">
        <Parameter Name="order" Type="org.example.Order" />
        <Parameter Name="Reason" Type="Edm.String" Nullable="true" />
        <ReturnType Type="org.example.Order" />
      </Action>
      <Function Name="GetTopCustomers" IsComposable="true">
        <Parameter Name="Count" Type="Edm.Int32" />
        <ReturnType Type="Collection(org.example.Customer)" />
      </Function>
      <Function Name="CalculateTotal" IsBound="true">
        <Parameter Name="order" Type="org.example.Order" />
        <ReturnType Type="Edm.Decimal" Nullable="false" />
      </Function>
      <EntityContainer Name="Default">
        <EntitySet Name="Customers" EntityType="org.example.Customer">
          <NavigationPropertyBinding Path="Orders" Target="Orders" />
        </EntitySet>
        <EntitySet Name="Orders" EntityType="org.example.Order">
          <NavigationPropertyBinding Path="Customer" Target="Customers" />
          <NavigationPropertyBinding Path="Items" Target="OrderItems" />
        </EntitySet>
        <EntitySet Name="OrderItems" EntityType="org.example.OrderItem" />
        <EntitySet Name="Documents" EntityType="org.example.Document" />
        <Singleton Name="AppSettings" Type="org.example.BaseEntity">
          <NavigationPropertyBinding Path="Owner" Target="Customers" />
        </Singleton>
        <ActionImport Name="ResetAllData" Action="org.example.ResetAllData" />
        <FunctionImport Name="GetTopCustomers" Function="org.example.GetTopCustomers" EntitySet="Customers" />
      </EntityContainer>
    </Schema>
  </edmx:DataServices>
</edmx:Edmx>`;

describe('parseCsdlXml', () => {
  it('parses namespace', () => {
    const schema = parseCsdlXml(minimalEdmx);
    expect(schema.namespace).toBe('org.reso.metadata');
  });

  it('parses entity types', () => {
    const schema = parseCsdlXml(minimalEdmx);
    expect(schema.entityTypes).toHaveLength(3);
    expect(schema.entityTypes[0].name).toBe('Property');
    expect(schema.entityTypes[1].name).toBe('Member');
    expect(schema.entityTypes[2].name).toBe('Media');
  });

  it('parses key properties', () => {
    const schema = parseCsdlXml(minimalEdmx);
    const property = schema.entityTypes[0];
    expect(property.key).toEqual(['ListingKey']);
  });

  it('parses property attributes', () => {
    const schema = parseCsdlXml(minimalEdmx);
    const property = schema.entityTypes[0];
    const listingKey = property.properties.find(p => p.name === 'ListingKey');
    expect(listingKey).toBeDefined();
    expect(listingKey?.type).toBe('Edm.String');
    expect(listingKey?.maxLength).toBe(255);
    expect(listingKey?.nullable).toBe(false);

    const listPrice = property.properties.find(p => p.name === 'ListPrice');
    expect(listPrice?.type).toBe('Edm.Decimal');
    expect(listPrice?.precision).toBe(14);
    expect(listPrice?.scale).toBe(2);
  });

  it('parses enum types', () => {
    const schema = parseCsdlXml(minimalEdmx);
    expect(schema.enumTypes).toHaveLength(1);
    expect(schema.enumTypes[0].name).toBe('StandardStatus');
    expect(schema.enumTypes[0].members).toHaveLength(3);
    expect(schema.enumTypes[0].members[0]).toEqual({
      name: 'Active',
      value: '0'
    });
  });

  it('parses navigation properties — collection', () => {
    const schema = parseCsdlXml(minimalEdmx);
    const property = schema.entityTypes[0];
    expect(property.navigationProperties).toHaveLength(2);

    const media = property.navigationProperties[0];
    expect(media.name).toBe('Media');
    expect(media.type).toBe('Collection(org.reso.metadata.Media)');
    expect(media.isCollection).toBe(true);
    expect(media.entityTypeName).toBe('Media');
  });

  it('parses navigation properties — single with partner', () => {
    const schema = parseCsdlXml(minimalEdmx);
    const property = schema.entityTypes[0];
    const listAgent = property.navigationProperties[1];
    expect(listAgent.name).toBe('ListAgent');
    expect(listAgent.isCollection).toBe(false);
    expect(listAgent.entityTypeName).toBe('Member');
    expect(listAgent.nullable).toBe(true);
    expect(listAgent.partner).toBe('Listings');
  });

  it('parses reverse navigation property', () => {
    const schema = parseCsdlXml(minimalEdmx);
    const member = schema.entityTypes[1];
    expect(member.navigationProperties).toHaveLength(1);
    const listings = member.navigationProperties[0];
    expect(listings.name).toBe('Listings');
    expect(listings.isCollection).toBe(true);
    expect(listings.partner).toBe('ListAgent');
  });

  it('returns empty navigationProperties for entity types without them', () => {
    const schema = parseCsdlXml(minimalEdmx);
    const media = schema.entityTypes[2];
    expect(media.navigationProperties).toHaveLength(0);
  });

  it('parses entity container', () => {
    const schema = parseCsdlXml(minimalEdmx);
    expect(schema.entityContainer).toBeDefined();
    expect(schema.entityContainer?.name).toBe('Default');
    expect(schema.entityContainer?.entitySets).toHaveLength(3);
    expect(schema.entityContainer?.entitySets[0]).toEqual({
      name: 'Property',
      entityType: 'org.reso.metadata.Property'
    });
  });

  it('returns empty arrays for complexTypes, actions, and functions on minimal schema', () => {
    const schema = parseCsdlXml(minimalEdmx);
    expect(schema.complexTypes).toHaveLength(0);
    expect(schema.actions).toHaveLength(0);
    expect(schema.functions).toHaveLength(0);
  });

  it('throws on missing Schema element', () => {
    expect(() => parseCsdlXml('<foo/>')).toThrow('Could not find Schema element');
  });
});

describe('parseCsdlXml — complex types', () => {
  it('parses complex types with properties', () => {
    const schema = parseCsdlXml(extendedEdmx);
    expect(schema.complexTypes).toHaveLength(2);

    const address = schema.complexTypes[0];
    expect(address.name).toBe('Address');
    expect(address.properties).toHaveLength(4);
    expect(address.properties[0].name).toBe('Street');
    expect(address.properties[2].name).toBe('State');
    expect(address.properties[2].maxLength).toBe(2);
    expect(address.navigationProperties).toHaveLength(0);
  });

  it('parses complex type inheritance and OpenType', () => {
    const schema = parseCsdlXml(extendedEdmx);
    const extended = schema.complexTypes[1];
    expect(extended.name).toBe('ExtendedAddress');
    expect(extended.baseType).toBe('org.example.Address');
    expect(extended.openType).toBe(true);
    expect(extended.properties).toHaveLength(1);
    expect(extended.properties[0].name).toBe('Country');
  });
});

describe('parseCsdlXml — referential constraints', () => {
  it('parses referential constraints on navigation properties', () => {
    const schema = parseCsdlXml(extendedEdmx);
    const order = schema.entityTypes.find(et => et.name === 'Order');
    expect(order).toBeDefined();

    const customerNav = order?.navigationProperties.find(np => np.name === 'Customer');
    expect(customerNav).toBeDefined();
    expect(customerNav?.referentialConstraints).toHaveLength(1);
    expect(customerNav?.referentialConstraints?.[0]).toEqual({
      property: 'CustomerId',
      referencedProperty: 'Id'
    });
  });

  it('omits referentialConstraints when none exist', () => {
    const schema = parseCsdlXml(extendedEdmx);
    const order = schema.entityTypes.find(et => et.name === 'Order');
    const itemsNav = order?.navigationProperties.find(np => np.name === 'Items');
    expect(itemsNav?.referentialConstraints).toBeUndefined();
  });
});

describe('parseCsdlXml — entity type inheritance attributes', () => {
  it('parses abstract entity type', () => {
    const schema = parseCsdlXml(extendedEdmx);
    const base = schema.entityTypes.find(et => et.name === 'BaseEntity');
    expect(base).toBeDefined();
    expect(base?.abstract).toBe(true);
  });

  it('parses baseType on derived entity type', () => {
    const schema = parseCsdlXml(extendedEdmx);
    const customer = schema.entityTypes.find(et => et.name === 'Customer');
    expect(customer).toBeDefined();
    expect(customer?.baseType).toBe('org.example.BaseEntity');
    expect(customer?.openType).toBe(true);
  });

  it('parses HasStream on entity type', () => {
    const schema = parseCsdlXml(extendedEdmx);
    const doc = schema.entityTypes.find(et => et.name === 'Document');
    expect(doc).toBeDefined();
    expect(doc?.hasStream).toBe(true);
  });

  it('does not set inheritance flags when absent', () => {
    const schema = parseCsdlXml(extendedEdmx);
    const order = schema.entityTypes.find(et => et.name === 'Order');
    expect(order?.baseType).toBeUndefined();
    expect(order?.abstract).toBeUndefined();
    expect(order?.openType).toBeUndefined();
    expect(order?.hasStream).toBeUndefined();
  });
});

describe('parseCsdlXml — enum IsFlags', () => {
  it('parses IsFlags and UnderlyingType on enum type', () => {
    const schema = parseCsdlXml(extendedEdmx);
    const permission = schema.enumTypes.find(et => et.name === 'Permission');
    expect(permission).toBeDefined();
    expect(permission?.isFlags).toBe(true);
    expect(permission?.underlyingType).toBe('Edm.Int64');
  });

  it('does not set enum flags when absent', () => {
    const schema = parseCsdlXml(extendedEdmx);
    const color = schema.enumTypes.find(et => et.name === 'Color');
    expect(color?.isFlags).toBeUndefined();
    expect(color?.underlyingType).toBeUndefined();
  });
});

describe('parseCsdlXml — navigation property bindings', () => {
  it('parses navigation property bindings on entity sets', () => {
    const schema = parseCsdlXml(extendedEdmx);
    const customers = schema.entityContainer?.entitySets.find(es => es.name === 'Customers');
    expect(customers?.navigationPropertyBindings).toHaveLength(1);
    expect(customers?.navigationPropertyBindings?.[0]).toEqual({
      path: 'Orders',
      target: 'Orders'
    });
  });

  it('parses multiple navigation property bindings', () => {
    const schema = parseCsdlXml(extendedEdmx);
    const orders = schema.entityContainer?.entitySets.find(es => es.name === 'Orders');
    expect(orders?.navigationPropertyBindings).toHaveLength(2);
    expect(orders?.navigationPropertyBindings?.[0]).toEqual({
      path: 'Customer',
      target: 'Customers'
    });
    expect(orders?.navigationPropertyBindings?.[1]).toEqual({
      path: 'Items',
      target: 'OrderItems'
    });
  });

  it('omits navigationPropertyBindings when none exist', () => {
    const schema = parseCsdlXml(extendedEdmx);
    const orderItems = schema.entityContainer?.entitySets.find(es => es.name === 'OrderItems');
    expect(orderItems?.navigationPropertyBindings).toBeUndefined();
  });
});

describe('parseCsdlXml — ContainsTarget', () => {
  it('parses ContainsTarget on navigation property', () => {
    const schema = parseCsdlXml(extendedEdmx);
    const customer = schema.entityTypes.find(et => et.name === 'Customer');
    const ordersNav = customer?.navigationProperties.find(np => np.name === 'Orders');
    expect(ordersNav?.containsTarget).toBe(true);
  });
});

describe('parseCsdlXml — actions and functions', () => {
  it('parses unbound action without parameters', () => {
    const schema = parseCsdlXml(extendedEdmx);
    const resetAction = schema.actions.find(a => a.name === 'ResetAllData');
    expect(resetAction).toBeDefined();
    expect(resetAction?.isBound).toBeUndefined();
    expect(resetAction?.parameters).toHaveLength(0);
    expect(resetAction?.returnType).toBeUndefined();
  });

  it('parses bound action with parameters and return type', () => {
    const schema = parseCsdlXml(extendedEdmx);
    const approveAction = schema.actions.find(a => a.name === 'ApproveOrder');
    expect(approveAction).toBeDefined();
    expect(approveAction?.isBound).toBe(true);
    expect(approveAction?.entitySetPath).toBe('order');
    expect(approveAction?.parameters).toHaveLength(2);
    expect(approveAction?.parameters[0]).toEqual({
      name: 'order',
      type: 'org.example.Order'
    });
    expect(approveAction?.parameters[1]).toEqual({
      name: 'Reason',
      type: 'Edm.String',
      nullable: true
    });
    expect(approveAction?.returnType).toEqual({
      type: 'org.example.Order'
    });
  });

  it('parses composable function with return type', () => {
    const schema = parseCsdlXml(extendedEdmx);
    const getTopFn = schema.functions.find(f => f.name === 'GetTopCustomers');
    expect(getTopFn).toBeDefined();
    expect(getTopFn?.isComposable).toBe(true);
    expect(getTopFn?.isBound).toBeUndefined();
    expect(getTopFn?.parameters).toHaveLength(1);
    expect(getTopFn?.parameters[0]).toEqual({
      name: 'Count',
      type: 'Edm.Int32'
    });
    expect(getTopFn?.returnType).toEqual({
      type: 'Collection(org.example.Customer)'
    });
  });

  it('parses bound function', () => {
    const schema = parseCsdlXml(extendedEdmx);
    const calcFn = schema.functions.find(f => f.name === 'CalculateTotal');
    expect(calcFn).toBeDefined();
    expect(calcFn?.isBound).toBe(true);
    expect(calcFn?.returnType).toEqual({
      type: 'Edm.Decimal',
      nullable: false
    });
  });
});

describe('parseCsdlXml — singletons', () => {
  it('parses singletons in entity container', () => {
    const schema = parseCsdlXml(extendedEdmx);
    expect(schema.entityContainer?.singletons).toHaveLength(1);

    const singleton = schema.entityContainer?.singletons[0];
    expect(singleton?.name).toBe('AppSettings');
    expect(singleton?.type).toBe('org.example.BaseEntity');
    expect(singleton?.navigationPropertyBindings).toHaveLength(1);
    expect(singleton?.navigationPropertyBindings?.[0]).toEqual({
      path: 'Owner',
      target: 'Customers'
    });
  });

  it('returns empty singletons array for minimal schema', () => {
    const schema = parseCsdlXml(minimalEdmx);
    expect(schema.entityContainer?.singletons).toHaveLength(0);
  });
});

describe('parseCsdlXml — action and function imports', () => {
  it('parses action imports', () => {
    const schema = parseCsdlXml(extendedEdmx);
    expect(schema.entityContainer?.actionImports).toHaveLength(1);

    const ai = schema.entityContainer?.actionImports[0];
    expect(ai?.name).toBe('ResetAllData');
    expect(ai?.action).toBe('org.example.ResetAllData');
    expect(ai?.entitySet).toBeUndefined();
  });

  it('parses function imports with entity set', () => {
    const schema = parseCsdlXml(extendedEdmx);
    expect(schema.entityContainer?.functionImports).toHaveLength(1);

    const fi = schema.entityContainer?.functionImports[0];
    expect(fi?.name).toBe('GetTopCustomers');
    expect(fi?.function).toBe('org.example.GetTopCustomers');
    expect(fi?.entitySet).toBe('Customers');
  });

  it('returns empty import arrays for minimal schema', () => {
    const schema = parseCsdlXml(minimalEdmx);
    expect(schema.entityContainer?.actionImports).toHaveLength(0);
    expect(schema.entityContainer?.functionImports).toHaveLength(0);
  });
});

describe('getEntityType', () => {
  it('finds entity type by name', () => {
    const schema = parseCsdlXml(minimalEdmx);
    const et = getEntityType(schema, 'Property');
    expect(et).toBeDefined();
    expect(et?.name).toBe('Property');
  });

  it('returns undefined for unknown name', () => {
    const schema = parseCsdlXml(minimalEdmx);
    expect(getEntityType(schema, 'Unknown')).toBeUndefined();
  });
});

describe('getEnumType', () => {
  it('finds enum type by name', () => {
    const schema = parseCsdlXml(minimalEdmx);
    const et = getEnumType(schema, 'StandardStatus');
    expect(et).toBeDefined();
    expect(et?.members).toHaveLength(3);
  });

  it('returns undefined for unknown name', () => {
    const schema = parseCsdlXml(minimalEdmx);
    expect(getEnumType(schema, 'Unknown')).toBeUndefined();
  });
});

describe('parseCsdlXml — sample-metadata.xml compatibility', () => {
  it("parses the test tool's sample metadata", async () => {
    const samplePath = resolve(import.meta.dirname, '../../web-api-add-edit-test/sample-metadata.xml');
    let xml: string;
    try {
      xml = await readFile(samplePath, 'utf-8');
    } catch {
      // Skip if file not available
      return;
    }
    const schema = parseCsdlXml(xml);
    expect(schema.namespace).toBeTruthy();
    expect(schema.entityTypes.length).toBeGreaterThan(0);
    const property = getEntityType(schema, 'Property');
    expect(property).toBeDefined();
    expect(property?.key.length).toBeGreaterThan(0);
    expect(property?.properties.length).toBeGreaterThan(0);
  });
});
