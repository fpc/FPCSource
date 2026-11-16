# Architecture — fcl-opf

## Overview

fcl-opf is a Free Pascal object-relational persistence library built on FPC's `sqldb` layer. It maps Object Pascal classes to database tables, generates SQL across multiple dialects, manages relationships, and provides automated schema migration.

## Architecture Pattern

**Component-based singleton framework** with three main singletons acting as service locators. All singletons are replaceable via `SetInstanceClass` before first use.

```
┌──────────────────────────────────────────────────────────────────┐
│                         TOPManager                               │
│                    (Central Facade / Singleton)                   │
│                                                                  │
│  ┌──────────────┐  ┌────────────┐  ┌───────────┐  ┌───────────┐│
│  │ TOPRegistry   │  │TOPSQLBuilder│  │Connections│  │Transaction││
│  │  (Singleton)  │  │ (Singleton) │  │  (List)   │  │  Handler  ││
│  └──────┬───────┘  └─────┬──────┘  └───────────┘  └───────────┘│
│         │                │                                       │
│  ┌──────┴───────┐  ┌─────┴──────┐                               │
│  │TPersisterDef │  │TOPDDLBuilder│                               │
│  │ (per class)  │  │ (DDL gen)  │                               │
│  │              │  │            │                               │
│  │  FieldMap    │  │ TSQLDriver │                               │
│  │  (prop↔col)  │  │ (dialect)  │                               │
│  └──────────────┘  └────────────┘                               │
│                                                                  │
│  ┌────────────────┐  ┌──────────────────┐  ┌──────────────────┐ │
│  │TOPRelation     │  │TOPManyToMany     │  │TOPMigration      │ │
│  │  Registry      │  │  Registry        │  │  Steps           │ │
│  │ (1:N relations)│  │ (N:M relations)  │  │ (custom steps)   │ │
│  └────────────────┘  └──────────────────┘  └──────────────────┘ │
└──────────────────────────────────────────────────────────────────┘

┌──────────────────────────────────────────────────────────────────┐
│                      Execution Layer                             │
│  TOPExecutor (query execution & row-to-object mapping)           │
│  TTransactionHandler (reference-counted multi-connection txns)   │
│  TOPLogger (SQL tracing, diagnostics)                            │
│  TOPClassFactory (customizable object creation)                  │
└──────────────────────────────────────────────────────────────────┘

┌──────────────────────────────────────────────────────────────────┐
│                    Schema Evolution Layer                         │
│  TOPSchemaInfo (introspected DB schema model)                    │
│  TOPSchemaDiff (desired vs. actual comparison)                   │
│  TOPMigrationTracker (version history in _opf_migrations)        │
│  TOPDDLBuilder (ALTER TABLE / CREATE TABLE generation)           │
└──────────────────────────────────────────────────────────────────┘
```

## Source Units

| Unit | Component | Role |
|------|-----------|------|
| `opf.manager.pp` | TOPManager, TOPAuditInfo | Central facade: Save, Load, Delete, CreateSchema, MigrateSchema, RegisterMigration, audit fields, lifecycle hooks |
| `opf.registry.pp` | TOPRegistry, TPersisterDef | Class-to-persister registration, field map storage, cached SQL |
| `opf.fields.pp` | TOPFieldMap, TOPFieldMapList | Property-to-column mapping with type info, constraints, and options |
| `opf.sql.pp` | TOPSQLBuilder | DML generation: SELECT, INSERT, UPDATE, DELETE, WHERE clauses |
| `opf.ddl.pp` | TOPDDLBuilder | DDL generation: CREATE TABLE, ALTER TABLE, indexes, foreign keys |
| `opf.driver.pp` | TSQLDriver | Dialect-specific keyword/type resolution with driver registry |
| `opf.executor.pp` | TOPExecutor | Query execution, parameter binding, result-set-to-object mapping |
| `opf.transaction.pp` | TTransactionHandler | Reference-counted transaction management across multiple connections |
| `opf.relations.pp` | TOPRelationRegistry | 1:N parent-child relationships, cascade delete orchestration |
| `opf.m2m.pp` | TOPManyToManyRegistry | N:M relationships via junction tables, bidirectional loading |
| `opf.schema.pp` | TOPSchemaInfo, TOPSchemaDiff | Schema introspection, desired-vs-actual comparison |
| `opf.migration.pp` | TOPSchemaMigrator, TOPMigrationTracker | Schema migration component with SQL script generation and version tracking |
| `opf.persister.rtti.pp` | TRTTIPersister | RTTI-based auto-discovery of published properties |
| `opf.persister.extrtti.pp` | TExtRTTIPersister, TAttributePersister | Extended RTTI for TNullable fields; attribute-driven mapping (trunk only) |
| `opf.persister.intf.pp` | IOPFieldProvider | Interface-driven persistence for maximum performance |
| `opf.nullable.pp` | IOPFNullable, TNullable specializations | Nullable field support (Levels 0, 1, 2) |
| `opf.json.pp` | IOPSerializer, TJsonRttiSerializer | JSON serialization for class-typed properties |
| `opf.logging.pp` | TOPLogger | Configurable logging: SQL, Trace, Info, Warning, Error |
| `opf.connections.pp` | TSQLDBConnectionDef, TSQLDBConnectionList | Connection definitions with JSON/INI serialization |
| `opf.proxy.pp` | TOPLazy, IOPLazyInit | Lazy-loading proxy record and auto-setup interface |
| `opf.listreg.pp` | TOPListRegistry, TOPListManipulator | List type handlers for polymorphic collection support |
| `opf.factory.pp` | TOPClassFactory, TOPFactoryContext | Customizable object creation with per-class factory callbacks |
| `opf.attributes.pp` | OPTableName, OPFieldName, OPSkip, etc. | Declarative attribute-driven mapping (trunk only) |
| `opf.snapshothooks.pp` | IOPDirtyTracking, TOPVariantArray | Per-object dirty tracking interface, variant capture and compare utilities used by the variant trackers |
| `opf.snapshot.pp` | TOPSnapshotManager, TOPSelfTracker | Change tracker keeping a variant array of the original property values, and the tracker for objects that keep their own snapshot |
| `opf.changetracking.pp` | IOPChangeTracker, IOPChangeTrackerHost, TOPChangeNames, TOPObjectStateTracker | Container-neutral change tracking contracts, changed-name collection, partial field map building, shared reference-state table with mode-driven class tracking, FIFO eviction and observer-driven removal of an entry when its object is freed; objects that cannot be observed are only tracked with TrackUnobserved |
| `opf.memento.pp` | TOPMemento, IOPMementoSource, IOPMementoRestored, TOPMementoFactory, OPDefaultMementoFactory | Memento contract: capture, compare and restore the state of one object, with an exact subject-class match, an optional restore notification and a factory hook for a generic memento |
| `opf.mementotracker.pp` | TOPMementoTracker | Change tracker keeping one memento per tracked object, taken from IOPMementoSource or from OPDefaultMementoFactory, so a changed object is saved with only the fields the memento reports as different |
| `opf.memento.extrtti.pp` | TOPRttiMemento | Memento built from the field map for a class that has no hand-written one: each value captured as a TValue, class properties as serialized text (trunk only) |
| `opf.criteria.pp` | TOPCriteria, TOPCompareOp | Fluent parameterized criteria builder with expression tree, pagination |
| `opf.linq.pp` | TOPLinqExpr, TOPLinqProp, Prop() | LINQ-style operator syntax via record operator overloading, class helper for Load |
| `opf.consts.pp` | Error string constants | Centralized error messages |
| `opf.types.pp` | TOPBaseComponent, EOPF | Base types and exception classes |
| `opf.task.pp` | TOPTask, TOPListTask | Execute/undo task framework with status tracking; undoing a list undoes its executed subtasks in reverse order, and a failing automatic undo keeps the original error |
| `opf.variants.pp` | Variant helpers | Variant-to-field conversion utilities |

## Core Components

### TOPManager (`opf.manager.pp`)

Central facade coordinating all persistence operations.

- **CRUD:** `Save`, `Load`, `Delete` for single objects; `Load` with criteria for lists; `Save` on a registered list saves all items in one transaction (inserts new, updates modified, deletes marked, skips clean) via protected virtual `DoSaveList`
- **Custom SQL:** `LoadSQL` executes user-provided SQL with ORM result mapping
- **Relationships:** `LoadChildren`, `LoadAllChildren`, `LoadAssociations`, `AddAssociation`, `RemoveAssociation`
- **Schema:** Delegates to `TOPSchemaMigrator` — `CreateSchema`, `MigrateSchema`, `GenerateMigrationSQL`, `RegisterMigration`. Auto-fills migrator registries from its own if not set
- **Transactions:** `BeginTransaction`, `Commit`, `Rollback` for explicit multi-operation boundaries
- **Auditing:** Automatic `foCreatedAt`/`foUpdatedAt`/`foCreatedBy`/`foUpdatedBy` field population via `AuditInfo` (`TOPAuditInfo`) sub-property
- **Criteria queries:** `Load(aList, aClass, aCriteria: TOPCriteria)` with parameterized WHERE, ORDER BY, LIMIT/OFFSET
- **LINQ queries:** `Load(aList, aClass, (Prop('Name') = 'Alice') and (Prop('Age') > 18))` via class helper in `Opf.Linq`
- **Soft delete:** `foSoftDelete` field option — Delete issues UPDATE instead of DELETE; loads auto-filter soft-deleted records; cascade propagation
- **Change tracking:** `ChangeTracker: IOPChangeTracker` — one attached tracker at a time, set by any component passed to `AttachChangeTracker` or by setting `Manager` on the tracker itself, and cleared by `DetachChangeTracker`; `GetDirtyFields(aObject, aFields)` returns `False` when the object is not tracked. The `TUTF8StringArray` form replaces the array with the changed property names; the `TStrings` form appends them to the caller's list without clearing it, and leaves the list untouched when it returns `False`
- **Optimistic concurrency:** `foVersion` field option auto-manages version column; `WHERE Version = :OldVersion` on UPDATE; `EOPF` with the `SErrConcurrencyConflict` message on conflict; supports integer and UUID versions
- **Lifecycle hooks:** Protected virtual `DoBeforeSave`, `DoAfterSave`, `DoAfterLoad`, `DoBeforeDelete`, `DoAfterDelete` + corresponding published events
- Singleton, replaceable via `SetInstanceClass`
- Owns connection list, audit info; references registry, SQL builder, relation registries, change tracker

### TOPRegistry (`opf.registry.pp`)

Maps `TClass` to `TPersisterDef` (persister class, field map, cached SQL, table name).

- `RegisterPersister(aClass, aPersisterClass)` — register a class for persistence
- `FindPersister(aClass)` — lookup by class type
- Each `TPersisterDef` holds: class reference, persister class, `TOPFieldMapList`, cached SQL strings, table name
- Singleton, replaceable via `SetInstanceClass`

### TOPSQLBuilder (`opf.sql.pp`)

Generates DML statements (SELECT, INSERT, UPDATE, DELETE) from field maps.

- 8 SQL dialects via `TSQLDialect` enum
- Configurable options: quote identifiers, lowercase keywords, space after separator, prepend table name
- Singleton, replaceable via `SetInstanceClass`

### TOPDDLBuilder (`opf.ddl.pp`)

Generates DDL statements and manages schema operations.

- `CreateSchema` — CREATE TABLE with constraints, indexes, foreign keys
- `CreateTable`, `CreateForeignKey`, `CreateJunctionTable`, `CreateIndexes`
- `GetAddColumnSQL`, `GetDropColumnSQL`, `GetAlterColumnTypeSQL`, `GetAlterColumnNullableSQL`
- `GetDropTableSQL`, `GetDropIndexSQL`, `GetDropForeignKeySQL`
- `IntrospectSchema` — reads actual database schema into `TOPSchemaInfo`
- `GenerateMigrationSQL` — converts schema diffs into ALTER statements
- `ExecDDL` — executes a single DDL statement with its own transaction
- Delegates dialect-specific behavior to `TSQLDriver`

### TSQLDriver (`opf.driver.pp`)

Encapsulates dialect-specific SQL generation.

- Factory: `TSQLDriver.CreateDriver(aDialect)` with class-level driver registry
- Provides: `SupportsReturningClause`, `GetPostInsertValueSQL`, `GetSequenceNextValSQL`, `GetBooleanKeyword`, `GetBlobKeyword`, `GetMemoKeyword`, `SupportsIfNotExists`, `GetLimitOffsetSQL`
- Dialect-specific subclasses registered for each `TSQLDialect` value
- Extensible: register new drivers for new databases

### TOPExecutor (`opf.executor.pp`)

Executes SQL queries and maps results to objects.

- `DoQuerySQL` — single-object query with parameter binding
- `DoQuerySQLList` — multi-row query populating a list via `TOPListManipulator`
- `MapRowToObject` — maps a `TField` row to an object via `ApplyProperty` on the persister
- Column validation against field map before query execution
- `UpperCaseUUID` property controls UUID case (default: lowercase)
- Singleton with replaceable class

### TTransactionHandler (`opf.transaction.pp`)

Reference-counted transaction manager for explicit multi-operation boundaries.

- Created per `BeginTransaction` scope (not a singleton)
- Supports nested begin/commit via reference counting
- Manages multiple connections with coordinated commit/rollback
- Saves and restores existing transactions on enlisted connections

## Persister Paths

Three strategies for mapping objects to database operations, from simplest to most performant:

### 1. RTTI Persister (`TRTTIPersister`)

Zero-config. Automatically discovers published properties via RTTI and maps them to columns by convention.

- Property name = column name
- Class name (minus `T` prefix) = table name
- Supports: String, Integer, Int64, Float, Boolean, Enum, Set, Variant, class-as-JSON
- Nullable support: Level 0 (stored-based), Level 1 (IOPFNullable interface)

### 2. Extended RTTI Persister (`TExtRTTIPersister`)

Inherits from `TRTTIPersister`. Adds Level 2 nullable support via `TNullable<T>` record fields using FPC trunk's extended RTTI. Requires FPC 3.3.1+.

### 3. Attribute Persister (`TAttributePersister`)

Inherits from `TExtRTTIPersister`. Uses declarative attributes for mapping control:

- `[OPTableName('...')]` — custom table name
- `[OPFieldName('...')]` — custom column name
- `[OPSkip]` — exclude property from persistence
- `[OPFieldOptions([foInKey, ...])]` — field options
- `[OPPrimaryKey('ID')]` — primary key designation
- `[OPRelation('ParentClass')]` — foreign key relationship; `[OPRelation('ParentClass', 'fk_name')]` also names the foreign key constraint, and a third `True` argument adds ON DELETE CASCADE
- `[OPIndex('col1,col2')]` — index definition
- `[OPUnique]`, `[OPDefault('value')]`, `[OPSize(100)]`, `[OPSequence('seq_name')]`

Requires FPC 3.3.1+ with `{$modeswitch prefixedattributes}`.

### 4. Interface Persister (`IOPFieldProvider`)

Maximum performance. The object itself implements `IOPFieldProvider` with `GetFieldDefs`, `ApplyField`, and `PersistField` — no RTTI overhead, full control over mapping.

## Relationships

### One-to-Many (1:N)

Registered via `TOPRelationRegistry.RegisterRelationship(ParentClass, ChildClass, FKField)`.

- `LoadChildren` / `LoadAllChildren` on TOPManager
- Cascade delete: application-level traversal, with optional DB-level `ON DELETE CASCADE`
- FK constraint name auto-generated or explicitly set

### Many-to-Many (N:M)

Registered via `TOPManyToManyRegistry.RegisterManyToMany(ClassA, ClassB, JunctionTable, FKA, FKB)`.

- `LoadAssociations`, `AddAssociation`, `RemoveAssociation` on TOPManager
- Junction table auto-created by `CreateSchema`
- Bidirectional loading via list property names
- Cascade delete cleans up junction rows

### Lazy Loading (`opf.proxy.pp`)

`TOPLazy<T>` is a generic record that wraps an object or list reference and defers loading until first access via its `Value` property.

**Record structure:**
- `FValue: T` — the wrapped object/list (owned by parent, not by the record)
- `FLoaded: Boolean` — set to True before calling loader (prevents re-entry)
- `FOnLoad: TLazyLoadEvent` — callback that performs the actual load

**Two integration paths:**

1. **FPC 3.2.2 — `IOPLazyInit` interface**: The parent class implements `IOPLazyInit`. After `Load`, the Manager calls `InitLazyLoaders(aManager)`. The user writes a callback per lazy field that calls `LoadChildren`/`LoadAssociations`/`Load` as appropriate.

2. **FPC 3.3.1+ — Automatic via extended RTTI**: The Manager discovers `TOPLazy` fields via `TRttiContext.GetFields`, matches them against the relation/M2M registries by comparing the record's `FValue` to backing fields, and auto-sets loaders using `TOPLazyBinding` helper objects. Requires `{$RTTI EXPLICIT FIELDS}` on both the specialization type and the class. The pre-declared `TOPLazyObjectList` and `TOPLazyObject` in `opf.proxy.pp` have this directive applied.

**Supports all relationship types:**
- 1:N children via `LoadChildren`
- M:N associations via `LoadAssociations`
- N:1 single object via FK transfer + `Load`

**UUID generation:**
The `TOPExecutor.UpperCaseUUID` property (default `False`) controls whether generated UUIDs use uppercase or lowercase hex digits. Set to `True` for compatibility with databases that store uppercase GUIDs (e.g., tiOPF-created data).

## Schema Migration

### TOPSchemaMigrator (`opf.migration.pp`)

`TOPSchemaMigrator` is a `TComponent` that manages schema creation, migration, and SQL script generation. It can be dropped on a datamodule and wired to the same registries as `TOPManager`.

**Published properties:**
- `Registry`, `RelationRegistry`, `M2MRegistry` — the ORM registries (same as on TOPManager)
- `MigrationTableName` — configurable tracking table name (default: `_opf_migrations`)

**Public methods:**
- `CreateSchema` — generate DDL from registered model, record version 1
- `MigrateSchema` — introspect + diff + execute ALTER/CREATE to reach target version
- `GenerateMigrationSQL` — produce a complete SQL script without executing (for external tools/installers)
- `RegisterMigration` — register custom Pascal callbacks for data transformations

`TOPManager` holds a `SchemaMigrator` property and delegates to it. When nil properties are found on the migrator, the Manager auto-fills them from its own registries.

### Migration Pipeline

`MigrateSchema` is a coordinator that calls protected virtual methods:

```
1. CreateTracker             — create TOPMigrationTracker with connection + table name
2. EnsureMigrationTable      — create tracking table if needed
3. GetCurrentVersion         — read max version from migration history
4. Version check             — skip if already at target
5. FindMatchingSteps         — find registered custom steps for this transition
6. ComputeSchemaDiffs        — introspect DB, compare with model, filter, generate SQL
7. ExecuteDDLStatements      — apply each ALTER/CREATE statement
8. ExecuteCustomSteps        — run RegisterMigration callbacks in individual transactions
9. RecordMigrationVersion    — write version to tracking table
```

Each step is a protected virtual method, allowing subclasses to override specific phases.

**Custom migration steps** (`RegisterMigration`) run after DDL, before version recording. Each step gets its own transaction. On exception, the transaction rolls back and the migration is not recorded, allowing safe retry.

**Options:** `moDryRun` (compute SQL and steps without changing anything), `moLogSQL` (log the SQL, then execute it), `moAllowDropColumn`, `moAllowDropTable`, `moAllowNarrowing` (destructive operations require opt-in), `moKeepIndexes` (do not drop indexes the model does not declare).

**Schema diff kinds:** AddTable, DropTable, AddColumn, DropColumn, AlterColumnType, AlterColumnNullable, AddIndex, DropIndex, AddForeignKey, DropForeignKey, AddJunctionTable.

### SQL Script Generation

`GenerateMigrationSQL` returns the full SQL script as an array of statements without executing anything. This enables external migration workflows:

```pascal
SQL := OPManager.GenerateMigrationSQL(Conn, 2, [], [gsoIncludeVersionTracking]);
// SQL contains: ALTER TABLE ..., CREATE TABLE ..., INSERT INTO _opf_migrations ...
```

The `gsoIncludeVersionTracking` option appends the `INSERT INTO _opf_migrations` statement to the script, so external tools maintain the version history. See the `demos/generate_migration/` example for a complete v1→v2 workflow.

### TOPMigrationTracker

Low-level helper that manages the migration tracking table. Has a `Connection` property (set once) and a configurable `TableName` (default `_opf_migrations`). Methods: `HasMigrationTable`, `EnsureMigrationTable`, `GetCurrentVersion`, `RecordMigration`, `GetMigrationHistory`.

## Object Factory

`TOPClassFactory` (`opf.factory.pp`) controls how objects are created when the ORM loads lists, children, or associations. By default, the executor calls `aClass.Create` — the factory allows customizing this.

### Registration

Two callback types, registered per class:

```pascal
// Plain function
OPFactory.RegisterFactory(TPerson, @MyPersonFactory);

// Method of object (stateful factory)
OPFactory.RegisterFactory(TPerson, @MyBuilder.CreatePerson);
```

Both receive `(aClass: TClass; aContext: TOPFactoryContext): TObject`.

### Factory Context

`TOPFactoryContext` provides:

| Property | Type | Description |
|----------|------|-------------|
| `Reason` | `TOPFactoryReason` | Why: `frListItem`, `frChild`, `frAssociation` |
| `ListObject` | `TObject` | The list being populated |
| `OwnerObject` | `TObject` | Parent/owner when loading children |
| `PropertyName` | `UTF8String` | Property name being loaded |

### Hierarchy Lookup

Factory registration is class-hierarchy-aware. A factory registered for `TAnimal` is also used when creating `TDog` (a descendant), unless `TDog` has its own factory. The lookup walks up `ClassParent` until a match is found.

### TComponent Handling

TComponent descendants receive automatic owner management without explicit factory registration:
- If `aContext.OwnerObject` is a `TComponent`, it is passed as the owner to `TComponentClass(aClass).Create(Owner)`
- Otherwise, created with `nil` owner

### RTTI Constructor Discovery

When a class is registered with `TExtRTTIPersister` (trunk only), the `AfterRegister` hook automatically scans for parameterless constructors via extended RTTI:

1. `TRttiContext.GetType(aClass).GetMethods` enumerates methods
2. Finds the first parameterless constructor that is not `TObject.Create`
3. Stores its `CodeAddress` on the `TPersisterDef`
4. Auto-registers a factory with `OPFactory`

This solves the FPC-specific issue that `TObject.Create` is **not virtual** — calling `aClass.Create` via a `TClass` variable always invokes `TObject.Create`, skipping any custom initialization. The RTTI factory calls the correct constructor directly.

**Requirements:** The class must use `{$RTTI EXPLICIT METHODS([vcPublic])}` for constructor discovery to work.

### Integration Point

`TOPExecutor.MapRowsToList` calls `OPFactory.CreateInstance(aClass, Context)` for each row. If no factory is registered and the class is not a TComponent descendant, falls back to `aClass.Create`.

## Interfaces

CORBA interfaces (non-reference-counted, declared via `{$INTERFACES CORBA}`):

- **IOPPersistenceState** — `GetPersistenceState` / `SetPersistenceState` — tracks object lifecycle: Clean, Key, New, Update, Delete, Deleted
- **IOPPrimaryKey** — `GetPrimaryKey` / `SetPrimaryKey` / `HasPrimaryKeyValue` — primary key access for manual control
- **IOPFieldProvider** — `GetFieldDefs` / `ApplyField` / `PersistField` — interface-driven persistence
- **IOPFNullable** — `IsNull(PropertyName)` — per-property null tracking (Level 1)
- **IOPSerializer** — `Serialize` / `Deserialize` — JSON serialization abstraction
- **IOPDirtyTracking** — `StoreSnapshot` / `RetrieveSnapshot` / `MarkDirty` / `GetMarkedDirty` / `ClearSnapshot` — per-class dirty tracking opt-in (object stores its own snapshot)
- **IOPChangeTracker** — `Capture` / `Discard` / `GetChanges` / `MarkDirty` / `PurgeAll` — change tracker contract, consumed by TOPManager.ChangeTracker
- **IOPChangeTrackerHost** — `AttachChangeTracker` / `DetachChangeTracker` — the side a change tracker attaches itself to, implemented by TOPManager
- **IOPMementoSource** — `CreateMemento` — hands out a memento holding the current state of the object; the caller owns it
- **IOPMementoRestored** — `MementoRestored` — optional notification, called after a memento was written back into the object
- **IOPConnectionResolver** — `ResolveConnection(aClass)` — custom per-class connection routing; it receives the class of the objects saved, loaded or deleted. Cascades and junction rows use the connection of the parent

## Database Dialect Support

| Dialect | DDL | DML | Schema Introspect | IF NOT EXISTS | Returning Clause |
|---------|-----|-----|-------------------|---------------|-----------------|
| SQLite | Yes | Yes | Yes | Yes | No |
| PostgreSQL | Yes | Yes | Yes | Yes | Yes |
| Firebird | Yes | Yes | Yes | No | Yes |
| Firebird 3 | Yes | Yes | Yes | Yes | Yes |
| MySQL | Yes | Yes | Yes | Yes | No |
| SQL Server | Yes | Yes | Partial | No | No |
| Oracle | Yes | Yes | Partial | No | No |
| Vanilla | Yes | Yes | No | Yes | No |

Auto-detection: `DetectDialectFromConnection` matches the connection class name to the appropriate dialect.

## Transaction Model

- **Implicit:** Single Save/Delete operations are automatically wrapped in a transaction
- **Explicit:** `BeginTransaction` / `Commit` / `Rollback` for multi-operation boundaries
- **Nesting:** Reference-counted — nested begin/commit pairs work correctly
- **Multi-connection:** TTransactionHandler coordinates across multiple connections
- **DDL:** Each DDL statement executes in its own transaction (required for SQLite)

## Entity Lifecycle Hooks

Two-layer hook design on `TOPManager` following the standard Delphi/Lazarus component pattern:

**Layer 1 — Protected virtual methods** (for subclass overrides):

| Method | Parameters | When |
|--------|-----------|------|
| `DoBeforeSave` | `aObject`, `aState` | Before SQL execution in Save |
| `DoAfterSave` | `aObject`, `aState` | After SQL execution, before state set to opsClean |
| `DoAfterLoad` | `aObject` | After single-object Load (not list loads) |
| `DoBeforeDelete` | `aObject` | Before cascade and DELETE SQL |
| `DoAfterDelete` | `aObject` | After DELETE SQL, before state set to opsDeleted |

**Layer 2 — Published event properties** (for IDE / datamodule usage):

| Event | Type | Fires when |
|-------|------|-----------|
| `BeforeSave` | `TOPSaveEvent` | Before SQL execution |
| `AfterSave` | `TOPSaveEvent` | After SQL execution |
| `AfterLoad` | `TOPObjectEvent` | After single-object load |
| `BeforeDelete` | `TOPObjectEvent` | Before cascade/delete |
| `AfterDelete` | `TOPObjectEvent` | After delete SQL |

Default `Do*` bodies call the corresponding event. Override `Do*` in a descendant and call `inherited` to keep the event, or omit `inherited` to suppress it. All hooks fire inside the transaction boundary — exceptions trigger rollback.

## Automatic Auditing Columns

Audit field options auto-populate timestamps and user identity during `Save`, without manual code per call.

### Configuration

All audit settings live in `TOPAuditInfo` (`TPersistent`), a published sub-property of `TOPManager`:

| Property | Type | Description |
|----------|------|-------------|
| `TimestampKind` | `TOPTimestampKind` | `tkLocal` (default) or `tkUTC` |
| `NumericalID` | `Int64` | User identity as a number |
| `StringID` | `String` | User identity as a string |
| `UserIDKind` | `TOPUserIDKind` | Auto-set discriminator: `ukNone`, `ukNumerical`, `ukString`, `ukObject` |
| `ObjectID` | `TObject` (public, write-only) | User identity as a registered ORM object: its primary key is read at assignment and kept in `ObjectKey` (and its class name in `ObjectClassName`), so the object may be freed afterwards. Raises when no key can be found |

Setting `NumericalID` to non-zero auto-sets `UserIDKind` to `ukNumerical`; setting `StringID` to non-empty auto-sets to `ukString`; setting `ObjectID` to non-nil auto-sets to `ukObject`. Setting to the empty value clears the discriminator back to `ukNone`.

### Semantic Rules

| Field option | On INSERT (`opsNew`) | On UPDATE (`opsUpdate`) |
|--------------|---------------------|------------------------|
| `foCreatedAt` | Set to timestamp | Not touched |
| `foUpdatedAt` | Set only if `foRequired` also present | Always set |
| `foCreatedBy` | Set from current user | Not touched |
| `foUpdatedBy` | Set only if `foRequired` also present | Always set |

### User Value Resolution

For `foCreatedBy`/`foUpdatedBy`, the value is resolved from `AuditInfo` based on `UserIDKind`:

- **`ukNumerical`** — uses `NumericalID` directly
- **`ukString`** — uses `StringID` directly
- **`ukObject`** — uses `ObjectKey`, the primary key read when `ObjectID` was assigned (via `IOPPrimaryKey` if supported, otherwise the key field of the class's persister in the manager's registry)

If the discriminator type differs from the field's property type, conversion is attempted (e.g., `Int64` → `String` via `IntToStr`). If `UserIDKind` is `ukNone` and an audit user field is encountered, an `EOPF` exception is raised.

### Injection Point

The protected virtual method `ApplyAuditFields(aObject, aState, aFieldMap)` runs inside `Save(aObject, aState, aPersister)`, between `DoBeforeSave` and the executor call. It sets object properties via RTTI before the executor reads them for parameter binding. Override in a descendant to customize audit behavior.

See `demos/auditing/` for a complete working example covering timestamps, numerical/string user tracking, UTC mode, and lifecycle hooks.

## Soft Delete

The `foSoftDelete` field option turns `Delete` into an UPDATE that marks a record as deleted without physically removing it. Auto-detection based on field type:

| Field type | Delete sets | "Not deleted" condition | Filter in SELECT |
|------------|------------|------------------------|-----------------|
| Boolean (required) | `TRUE` | `FALSE` | `col = FALSE` (or `col = 0` for non-BOOLEAN dialects) |
| Boolean (nullable) | `TRUE` | `NULL` or `FALSE` | `(col IS NULL OR col = FALSE)` |
| DateTime (float) | Current timestamp | `NULL` | `col IS NULL` |

### Automatic Load Filtering

The soft-delete filter is automatically appended to SELECT queries in:

- `Load(single)` — always filtered (soft-deleted record raises `SErrLoadNotFound`)
- `Load(list)` without criteria — filtered
- `LoadChildren` / `LoadAssociations` — filtered

The filter is **not** applied when:

- `Load(list)` is called with non-empty criteria — user criteria means full control over what is returned
- `LoadSQL` — custom SQL is never modified

### Cascade Soft Delete

When deleting a parent that has registered child relationships:

- Children **with** `foSoftDelete` are soft-deleted (UPDATE) — their soft-delete column is set
- Children **without** `foSoftDelete` are hard-deleted (DELETE) as before
- M2M junction rows are **preserved** on soft delete (the parent still exists physically; junctions are needed for restorability)

### Configuration

At most one field per class may have `foSoftDelete` (validated at registration time). The field must be `tkBool` or `tkFloat` (DateTime). For DateTime soft delete, the timestamp respects `AuditInfo.TimestampKind` (local or UTC).

For DateTime soft-delete fields, remove `foInInsert` and `foInUpdate` from the field options so the column stays `NULL` in the database until a soft delete sets it.

The protected virtual method `GetSoftDeleteFilter` on `TOPManager` generates the dialect-aware WHERE clause fragment. Override in a descendant to customize filter logic.

See `demos/soft_delete/` for a complete working example covering Boolean and DateTime soft delete, criteria bypass, and cascade propagation.

## Change Tracking (Selective UPDATE)

Opt-in change tracking that generates UPDATE statements containing only modified columns. When no property changed since the reference state was taken, the UPDATE is skipped entirely.

### One Contract, Three Trackers

Every mechanism implements the same contract, `IOPChangeTracker` — `Capture`, `Discard`, `GetChanges`, `MarkDirty`, `PurgeAll`. `TOPManager` holds one tracker in `ChangeTracker` and never learns which mechanism it is. A tracker is a component: setting its `Manager` property, or passing it to `AttachChangeTracker`, attaches it through `IOPChangeTrackerHost`, and attaching another replaces it. All three constructors end with `Manager := OPManager`, so creating a tracker already attaches it to the default manager, and creating a second one takes the place of the first. The manager asks one tracker one question, so no mechanism takes priority over another.

| Tracker | Reference state | Tracks |
|---------|-----------------|--------|
| `TOPSnapshotManager` | a variant array of the mapped property values | every class the mode allows |
| `TOPSelfTracker` | the snapshot the object stores itself through `IOPDirtyTracking` | only classes implementing that interface |
| `TOPMementoTracker` | a `TOPMemento` taken from the object | classes with `IOPMementoSource`, or any class when `OPDefaultMementoFactory` is set |

All three descend from `TOPObjectStateTracker`, which holds the table of reference states keyed by object and everything around it: the tracking modes, `RegisterClass` / `ExcludeClass`, `MaxEntries` eviction, the names given to `MarkDirty`, the stored-class check per entry and the observer handling. A descendant fills in two hooks — `CreateState` builds the reference state and `CompareState` names what differs from it. A third, `ReleaseState`, is called when an entry is dropped and does nothing unless a descendant overrides it; only `TOPSelfTracker` does, because its state lives in the object rather than in the entry. It is skipped on a class mismatch and when the object reports its own destruction, since the object it would reach into is gone or is no longer the one the state belongs to.

`GetChanges` answers in three ways, and the difference between the first two matters:

| Result | Names | What Save does |
|--------|-------|----------------|
| `False` | empty | full UPDATE of every `foInUpdate` field |
| `True` | empty | no UPDATE at all |
| `True` | filled | UPDATE of the key, version and named fields |

A skipped UPDATE skips the rest of the save with it: `DoAfterSave` does not fire, the object keeps the persistence state it had instead of moving to `opsClean`, and no fresh reference state is taken.

An object the tracker holds no reference state for is therefore written in full: no tracker attached, a class the mode excludes, a class the tracker can build no state for, and an entry dropped by eviction all end up there. Within what the tracker can compare, a changed column is never silently left out of an UPDATE; the one blind spot is a property type the variant trackers cannot capture, described under Property Type Support below.

### Tracking Modes

| Mode | Behavior |
|------|----------|
| `tmImplicit` (default) | All classes tracked automatically; use `ExcludeClass` to opt out |
| `tmExplicit` | Only classes added via `RegisterClass` are tracked |
| `tmNone` | Tracking disabled |

`TOPTrackMode` and the `tm` names are declared in `Opf.ChangeTracking`. `Opf.Snapshot` declares `TOPSnapshotMode` as another name for `TOPTrackMode`, and `smNone`, `smImplicit` and `smExplicit` as constants for the three values, so code written against the older names keeps compiling with `Opf.Snapshot` alone; the `tm` names need `Opf.ChangeTracking` in the uses clause.

```pascal
SM := TOPSnapshotManager.Create(nil);       // auto-attaches to default OPManager
SM.Mode := smExplicit;                       // only track registered classes
SM.RegisterClass(TCustomer);                 // TCustomer gets partial UPDATEs
// TInvoice (not registered) gets full UPDATEs
```

A class that keeps its own snapshot needs a `TOPSelfTracker` attached; the interface alone is not consulted.

```pascal
TEmployee = class(TObject, IOPPersistenceState, IOPDirtyTracking)
  // ... implement StoreSnapshot, RetrieveSnapshot, MarkDirty, GetMarkedDirty, ClearSnapshot
end;
```

### The Memento Mechanism

A memento is a copy of the state of one object at one moment. `TOPMemento` declares four methods a descendant fills in — `GetSubjectClass`, `Capture`, `DoRestore` and `DoCompare` — and builds `IsChanged`, `GetChanges` and `RestoreTo` on top of them. The class a memento names in `GetSubjectClass` must match the subject exactly: handing a descendant object to a memento written for its base class raises `EOPFMemento` instead of quietly ignoring the fields the descendant added.

An object hands out a memento through `IOPMementoSource.CreateMemento`, and the caller owns the result. A class that wants to hear about a restore also implements `IOPMementoRestored`; `RestoreTo` calls `MementoRestored` when it is there. Because a memento written next to its subject writes fields rather than properties, restoring does not itself mark the object as changed.

The memento a user takes and the memento `TOPMementoTracker` keeps are never the same instance, so canceling an edit and building a partial UPDATE cannot disturb each other. That is what makes undo an application concern: the framework supplies the state copy and the restore, the stack and its lifetime belong to the application.

On trunk, `TOPRttiMemento` builds a memento out of the field map for a class that has no hand-written one. It captures every `foInUpdate` and `foInKey` property as a `TValue` — which, unlike a `Variant`, holds records, sets and `TNullable<T>` — and captures a class-valued property as serialized text. Its `SameValue` walks a record field by field, compares a nullable on `HasValue` first and its payload only when both sides have one, and reports a value it cannot compare as changed. The unit assigns `OPDefaultMementoFactory` in its initialization section, which is the only link between the tracker and this trunk-only unit; `TOPMementoTracker.UseDefaultFactory` turns the fallback off. Because the assignment happens at initialization, the fallback exists only when the unit is linked into the program: the `fcl_opf` package pulls it in on trunk through `ide/opf.trunk.pas`, while a program compiled straight against `src/` has to name `Opf.Memento.ExtRTTI` in a uses clause of its own. On FPC 3.2.2 there is no factory at all, so a class without a hand-written memento is not tracked and is written in full.

### Architecture

```
opf.snapshot.pp                opf.changetracking.pp          opf.manager.pp
───────────────                ─────────────────────          ──────────────
TOPSnapshotManager      ──┐    IOPChangeTracker         ────> TOPManager
TOPSelfTracker          ──┤    IOPChangeTrackerHost           .ChangeTracker: IOPChangeTracker
                          ├──> TOPObjectStateTracker          AttachChangeTracker
opf.mementotracker.pp     │    TOPChangeNames                 DetachChangeTracker
─────────────────────     │    BuildChangedFieldMapList       CaptureObjectSnapshot
TOPMementoTracker       ──┘                                   GetDirtyFields
                                                              Partial UPDATE in Save

opf.snapshothooks.pp           opf.memento.pp                 opf.memento.extrtti.pp
────────────────────           ──────────────                 ──────────────────────
IOPDirtyTracking               TOPMemento                     TOPRttiMemento
OPFCapturePropertyValues       IOPMementoSource               sets OPDefaultMementoFactory
OPFComputeDirtyFields          IOPMementoRestored             (trunk only)
                               OPDefaultMementoFactory
```

`opf.manager.pp` uses `opf.changetracking.pp` and nothing else of this subsystem. Inside the subsystem `opf.snapshothooks.pp` is reached through `opf.snapshot.pp` only — a class implementing `IOPDirtyTracking` still names `Opf.SnapshotHooks` in its own uses clause, as `demos/dirty_interface/` does. `opf.mementotracker.pp` reaches the trunk-only unit through `OPDefaultMementoFactory` rather than a uses clause.

A tracker is a `TOPBaseComponent` descendant with a `Manager` property. Setting `Manager` attaches the tracker through `IOPChangeTrackerHost.AttachChangeTracker`. Bidirectional `FreeNotification` ensures safe lifecycle management.

### Timing

- **Capture:** after Load completes (after `DoAfterLoad`, after state = `opsClean`)
- **Compare:** during Save, after `ApplyAuditFields`, before SQL generation
- **Refresh:** after a successful Save (the new reference state holds the post-save values)
- **Discard:** after Delete
- **Purge:** a reference state survives only a successful commit. An outermost `Rollback` and a `Commit` that fails while committing forget every reference state, since the values in them were never kept by the database; every object is untracked afterwards and writes all its columns on its next Save. A successful commit keeps them, a nested rollback touches nothing, and a `Commit` with no transaction open raises before anything is purged

### Partial UPDATE Generation

When a tracker reports changed names, `BuildChangedFieldMapList` builds a filtered `TOPFieldMapList` holding the key and version fields plus the named `foInUpdate` fields. This filtered map is passed to the existing `GetUpdateSQL` and `ExecuteSQL` — no changes to the SQL builder or executor were needed. Column order follows the field map. A reported name that no field map entry carries is ignored, so a memento may capture state that is not persistent.

### MarkDirty

Every tracker supports explicit `MarkDirty(aObject, aPropertyName)` to force a column into the UPDATE regardless of what the comparison finds. Useful for computed properties or forced writes. Marked names are held on the tracker entry, merged into the answer `GetChanges` gives, and cleared by the next `Capture`. `MarkDirty` on an object with no entry does nothing and does not start tracking it. `TOPSelfTracker` is the exception: it hands the name straight to the object's own `IOPDirtyTracking.MarkDirty` without looking at the entry table, so the name is held by the object and is cleared by that class's `StoreSnapshot`, and marking an untracked object is harmless because `GetChanges` still answers `False` for it.

### Memory Management

- `MaxEntries` property (default 0 = unlimited): FIFO eviction of the oldest entries before a new one is added, so the table never holds more than the limit. The evicted object is untracked again, so its next Save produces a full UPDATE
- Only objects supporting `IFPObserved` (e.g. any `TPersistent`) get an entry: the tracker observes the object and drops the entry when it is freed. `TrackUnobserved` (default False) also tracks plain `TObject` instances; these cannot be observed and must be purged by hand before they are freed
- `Purge(aObject)` — remove one entry; `PurgeAll` — remove every entry
- Class-validation guard: an entry whose stored `TClass` no longer matches the class of the object at that address is dropped and reported as untracked, which catches a freed object whose address was reused

### Property Type Support

`TOPSnapshotManager` and `TOPSelfTracker` capture values as Variants, mirroring the RTTI persister's type dispatch:

| Property kind | Capture method |
|--------------|---------------|
| String, WideString, UnicodeString | `GetStrProp` |
| Integer, Boolean, Enum, Set | `GetOrdProp` |
| Int64 | `GetInt64Prop` |
| Float (Double, DateTime) | `GetFloatProp` |
| Variant | `GetVariantProp` |
| Class (TStrings) | `.Text` property |
| Class (other) | `OPSerializer.Serialize` (JSON) |

A property of any other kind — a record, and so a `TNullable<T>` — is captured as `Unassigned` and never reported as changed, so its column stays out of a partial UPDATE. A memento has no such limit: a hand-written one compares the fields it captured, and `TOPRttiMemento` holds each value in a `TValue`.

See `demos/dirty_implicit/`, `demos/dirty_explicit/`, `demos/dirty_interface/` and `demos/memento/` for complete working examples.

## Optimistic Concurrency

The `foVersion` field option enables optimistic concurrency control via a version column. The ORM auto-manages the version value and detects concurrent modifications.

### Version Strategies

| Property type | Initial value | On UPDATE | Comparison |
|--------------|--------------|-----------|------------|
| Integer (`tkInteger`, `tkInt64`) | 1 | Increment by 1 | Numeric equality |
| String (`tkAString`) | New UUID | Regenerate UUID | String equality |

### How It Works

On **INSERT**, if the version property is zero (integer) or empty (string), the ORM sets the initial value before SQL generation. The version appears in the INSERT like any other column.

On **UPDATE**, the ORM:

1. Reads the current (old) version from the object
2. Increments (integer) or regenerates (UUID) the version on the object
3. Generates `UPDATE T SET ..., Version=:Version WHERE (ID=:ID) AND (Version=:_opf_oldversion)`
4. Binds `:_opf_oldversion` with the old value, `:Version` with the new value
5. Executes and checks `RowsAffected`
6. If `RowsAffected = 0` → raises `EOPF` with the `SErrConcurrencyConflict` message (another user modified or deleted the record)

### Change Tracking Integration

When change tracking produces a partial UPDATE, the version field is **always** included — `BuildChangedFieldMapList` treats `foVersion` like `foInKey` (unconditional inclusion). This ensures the version WHERE check applies even when only a subset of columns changed.

### Validation Rules

- One `foVersion` field per class (enforced at registration, like `foSoftDelete`)
- Must be `tkInteger`, `tkInt64`, or `tkAString`
- Must have `foInUpdate` (so it appears in the SET clause)
- Must NOT have `foInKey` (the version WHERE clause is managed separately from the primary key WHERE)

### Execution Path

The versioned UPDATE executes inline in `TOPManager.Save` (not through `TOPExecutor.ExecuteSQL`) to access `TSQLQuery.RowsAffected` after execution. This follows the same pattern as soft-delete, which also executes inline.

See `demos/version_integer/` and `demos/version_uuid/` for complete working examples with conflict simulation.

## Parameterized Criteria API

`TOPCriteria` (`opf.criteria.pp`) is a fluent, recursive expression builder that generates parameterized WHERE clauses. Users specify property names; the ORM resolves them to database column names at query time via the field map.

### Fluent Builder

```pascal
Criteria := TOPCriteria.Create;
try
  Criteria
    .Where('Active', opEqual, True)
    .AddAnd(TOPCriteria.Create
      .Where('Age', opGreater, 18)
      .AddOr('VIP', opEqual, True))
    .OrderBy('Name')
    .Limit(10).Offset(20);
  OPManager.Load(List, TPerson, Criteria);
finally
  Criteria.Free;
end;
```

### Comparison Operators

`opEqual`, `opNotEqual`, `opLess`, `opLessOrEqual`, `opGreater`, `opGreaterOrEqual`, `opLike`, `opNotLike`, `opContains`. Typed overloads for Integer, Int64, String, Double, Boolean, TDateTime — no Variants. `opContains` wraps the value in `%` wildcards automatically.

### Special Conditions

`WhereIsNull`, `WhereIsNotNull`, `WhereIn` (array of const / array of TDateTime), `WhereNotIn`, `WhereBetween`. All have `AddAnd*` / `AddOr*` variants.

### Composition

`AddAnd` / `AddOr` accept both field+op+value and sub-criteria (parenthesized). Sub-criteria ownership is transferred to the parent; when the merge raises, the sub-criteria is freed. `Not_` wraps the current expression in `NOT (...)`.

### Pagination

`OrderBy` (chainable, ASC/DESC), `Limit`, `Offset`. LIMIT/OFFSET syntax is dialect-specific via `TSQLDriver.GetLimitOffsetSQL`:

| Dialect | Syntax |
|---------|--------|
| SQLite, PostgreSQL, MySQL | `LIMIT N OFFSET M` |
| Firebird (pre-3) | `ROWS M+1 TO M+N` |
| Firebird 3+, SQL Server, Oracle | `OFFSET M ROWS FETCH NEXT N ROWS ONLY` |

### Soft-Delete Integration

`IncludeDeleted` on a criteria suppresses the automatic soft-delete filter. Without it, soft-deleted records are filtered out as usual.

### Expression Tree

Nodes are stored as records in a dynamic array (arena-style memory). Sub-criteria passed to `AddAnd`/`AddOr` have their nodes and params merged into the parent with automatic parameter renumbering.

See `demos/criteria/` for a complete working example.

## LINQ-Style Query Syntax

`Opf.Linq` (`opf.linq.pp`) provides a natural Pascal operator syntax on top of `TOPCriteria`. Uses `{$modeswitch advancedrecords}` for record operator overloading — works on both FPC 3.2.2 and trunk.

### Usage

```pascal
uses Opf.Linq;

// No objects to create or free — automatic memory management
OPManager.Load(List, TPerson,
  (Prop('Active') = True)
  and ((Prop('Dept') = 'Engineering') or (Prop('Dept') = 'Sales'))
  and (Prop('Age') >= 21)
  and Prop('Name').Contains('sm'));
```

### Architecture

- `TOPLinqProp` — record returned by `Prop('FieldName')`. Typed comparison operators (`=`, `<>`, `>`, `<`, `>=`, `<=`) for Integer, Int64, String, Double, Boolean, TDateTime. Methods: `IsNull`, `IsNotNull`, `Like`, `NotLike`, `Contains`.
- `TOPLinqExpr` — record wrapping a refcounted `TOPCriteria`. Logical operators: `and`, `or`, `not`. Each operator builds the `TOPCriteria` tree using the existing criteria API.
- `TOPManagerLinqHelper` — class helper adding `Load(aList, aClass, aExpr: TOPLinqExpr)` to `TOPManager` without creating a dependency from the manager on the LINQ unit.

### Memory Management

Each `TOPLinqExpr` holds an `IOPLinqCtx` COM interface wrapping a `TOPCriteria`. FPC's COM interface refcounting automatically frees intermediates created during operator evaluation. Zero memory leaks verified via HeapTrc. Management operators (`Initialize`/`Finalize`) were evaluated but rejected — FPC does not reliably finalize all temporaries in complex operator chains.

### Integration

The `and`/`or` operators detach criteria from their contexts and combine them via `TOPCriteria.AddAnd`/`AddOr`. The class helper extracts the final `TOPCriteria` and passes it to the existing `Load(aList, aClass, aCriteria: TOPCriteria)`, which handles field name resolution, soft-delete filtering, and pagination.

See `demos/linq/` for a complete working example.

## Field Options

`TOPFieldOptions` set on `TOPFieldMap`:

| Option | Meaning |
|--------|---------|
| `foInKey` | Part of primary key |
| `foInSelect` | Included in SELECT |
| `foInInsert` | Included in INSERT |
| `foInUpdate` | Included in UPDATE |
| `foRequired` | NOT NULL constraint |
| `foAutoInc` | Auto-increment (DB-managed) |
| `foUUID` | Application-side UUID generation |
| `foAsString` | Store as string representation |
| `foNullable` | Explicit nullable marker |
| `foUnique` | UNIQUE constraint |
| `foCreatedAt` | Auto-populate with timestamp on INSERT |
| `foUpdatedAt` | Auto-populate with timestamp on UPDATE (and INSERT if `foRequired`) |
| `foCreatedBy` | Auto-populate with current user identity on INSERT |
| `foUpdatedBy` | Auto-populate with current user identity on UPDATE (and INSERT if `foRequired`) |
| `foSoftDelete` | Soft delete marker — Delete issues UPDATE instead of DELETE; loads auto-filter |

Additional field properties: `DefaultValue` (Variant), `SequenceName`, `MaxSize`, `MinSize`, `OnGenerateValue` (custom generator event).

## Logging

`TOPLogger` singleton with configurable log kinds:

| Kind | Purpose |
|------|---------|
| `lkSQL` | Generated SQL statements |
| `lkTrace` | Operation entry/exit |
| `lkInfo` | Migration progress, status messages |
| `lkWarning` | Non-fatal issues |
| `lkError` | Error conditions |

Output to console (`LogToConsole`) or file (`LogFileName`). Guarded in production code with `{$IFNDEF OPF_SKIP_LOGGING}`.

## Testing

- **Framework:** FPCUnit with console test runner
- **Test count:** 1130+ tests (FPC trunk), 1100+ (FPC 3.2.2)
- **Coverage:** All public API methods, all persister paths, all CRUD operations, schema migration, M2M, relationships, nullable types, transactions, custom SQL, logging
- **Integration tests:** SQLite (in-memory, always available), PostgreSQL, Firebird, MySQL (when configured via `tests/databases.ini`)
- **Memory:** HeapTrc enabled — zero memory leaks
- **Compiler compatibility:** Full suite on FPC trunk and FPC 3.2.2 (extended RTTI tests excluded on 3.2.2 via `{$IFNDEF VER3_2}`)

## Compiler Requirements

- **FPC 3.2.2 (release):** Full support. `TExtRTTIPersister`, `TAttributePersister` and `TOPRttiMemento` excluded automatically; a memento is written by hand there.
- **FPC trunk (3.3.1+):** Full support including extended RTTI, `TNullable<T>`, attribute-driven mapping, and the generic memento of `Opf.Memento.ExtRTTI`.
- **Dependencies:** Only FPC's FCL and sqldb — no external libraries.
