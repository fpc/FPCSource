# fcl-opf — Object Persistence Framework for Free Pascal

fcl-opf is a zero-dependency ORM library built on Free Pascal's sqldb layer. 
Register any class with published properties and get immediate CRUD operations against 
PostgreSQL, SQLite, Interbase/Firebird, MySQL, SQL Server, or Oracle — no base class inheritance required, 
no framework patterns to adopt.

## Features

### Core ORM

- **Zero-config CRUD** — Register a class, get Save, Load, Update, Delete with automatic SQL generation
- **Convention-based mapping** — Published properties map to columns by name; class name (minus `T` prefix) maps to table name
- **Explicit field mapping** — Override any convention with per-class field map customization
- **JSON mapping files** — Opt-in unit `Opf.Mapping.Json`: `SaveMapping` writes the mapping of all registered classes (table, columns, options, sizes, sequences, defaults, primary key constraint, indexes, relations, many-to-many links) to a JSON file; edit it and `LoadMapping` applies it on top of the default mapping. Works on FPC 3.2.2, where there are no attributes. See [JSON mapping files](#json-mapping-files)
- **Interface-driven persisters** — Maximum performance path with no RTTI overhead
- **Custom SQL** — `LoadSQL` lets you write SQL while the ORM maps result sets to objects
- **List loading** — Load collections of objects matching query criteria
- **List saving** — `Save(aList)` detects registered lists and saves all items in a single transaction — inserts new, updates modified, deletes marked, skips clean

### Query Building

- **Parameterized criteria API** — `TOPCriteria` fluent builder with type-safe comparisons (`opEqual`, `opGreater`, `opContains`, etc.), `WhereIn`, `WhereBetween`, `IsNull`/`IsNotNull`, recursive `AddAnd`/`AddOr` sub-expressions, `Not_`, and `IncludeDeleted` for soft-delete bypass. Conditions combine in the order they are added: `Where(a).AddOr(b).AddAnd(c)` means `(a OR b) AND c`, and the soft-delete filter is always added as a separate parenthesized condition. A second `Where` starts a new condition (its parameters replace the earlier ones). Adding a sub-criteria also takes over its `OrderBy`, `Limit`/`Offset` and `IncludeDeleted`; conflicting `Limit` or `Offset` values raise (the sub-criteria is then freed)
- **SQL safety** — Values are always passed as parameters. Field names in criteria and `OrderBy` must be mapped properties (or mapped column names); anything else raises an exception, and `OrderBy` only accepts a field name with an optional `ASC`/`DESC`. Identifiers are quoted or transformed by the driver's reserved word strategy everywhere SQL is generated. SQL text you write yourself — `WhereRaw('UPPER(Name) = ?', ['BOB'])`, `Load(List, Class, 'criteria')`, `LoadSQL` — is used as given: never build it from user input
- **LINQ-style operator syntax** — Natural Pascal expressions via record operator overloading: `(Prop('Active') = True) and (Prop('Age') > 18)`. Fully recursive with `and`, `or`, `not`. Automatic memory management via COM interface refcounting — no objects to create or free. Works on FPC 3.2.2 and trunk
- **Pagination** — `Limit`/`Offset` on `TOPCriteria` with dialect-specific SQL generation (SQLite/PostgreSQL `LIMIT N OFFSET M`, Firebird `ROWS`, SQL Server/Oracle `FETCH NEXT`). `-1` means not set, `Limit(0)` loads nothing (no query is run), and an `Offset` without `Limit` works on SQLite and MySQL as well
- **Property name resolution** — Criteria and LINQ expressions use property names; the ORM resolves them to database column names at query time

### Relationships

- **Parent-child (1:N)** — Register relationships with automatic cascade delete
- **Many-to-many (N:M)** — Implicit junction table management, bidirectional association loading
- **Cascade operations** — Configurable DB-level or application-level cascade delete. A relation of a class to itself (a tree) is followed row by row, deepest rows first. Deleting a soft-deletable parent soft-deletes the children that have a soft-delete field and leaves the others (and what is below them) alone, so the parent can be restored; a hard delete removes all children. Rows that are already soft-deleted keep their deletion value. Cascades need a single-column primary key: a parent with a composite key and cascading relations raises before anything is deleted. Cascades and junction rows use the connection of the parent
- **Lazy loading** — `TOPLazy<T>` generic record wraps any object or list reference and triggers loading on first property access. Two integration paths: FPC 3.2.2 uses `IOPLazyInit` interface (user writes a small callback); FPC 3.3.1+ uses extended RTTI for fully automatic setup — the Manager discovers lazy fields and wires loaders from the relation/M2M registries. Supports 1:N children, M:N associations, and N:1 single-object references. A loader that fails leaves the value unloaded, so the next access tries again. Lazy fields are also set up for objects loaded in lists, as children and as associations. The binding behind an automatic lazy field is held by the `TOPLazy` record and freed with its entity, for any entity class. If the manager is freed first, accessing an unloaded lazy field raises an error

### Type Support

- **Standard types** — String, UnicodeString/WideString, Char/WideChar, Integer, Cardinal, Int64, QWord, Float, Boolean, Enum, Set, Variant. `Cardinal` columns are `BIGINT`. `QWord` is stored in a signed `BIGINT`: values above `High(Int64)` raise `EOPF` instead of being stored as negative numbers. UnicodeString values are passed as Unicode parameters; for non-ASCII text on PostgreSQL and Firebird the program needs a UTF-8 system code page (on Unix: the `cwstring` unit, which Lazarus programs include)
- **Dates and money** — `TDateTime`, `TDate` and `TTime` are stored in native `TIMESTAMP`/`DATE`/`TIME` columns and passed as date/time parameters; `Currency` is stored as `DECIMAL(18,4)` and passed exactly. Databases created by earlier versions store dates as numbers: set `OPFDateTimeStorage := dtsFloat` (or `foDateTimeAsFloat` on a field) to keep using them
- **JSON blob serialization** — Class-typed properties stored as JSON columns, with automatic round-trip serialization. Variant values that JSON has no type for (Currency, TDateTime, Unassigned, arrays of arrays) are written as tagged objects so they read back with the same type; set `OPFVariantJSONTypeTags := False` for plain JSON
- **Nullable fields** — Three levels: stored-based (Level 0), interface-based (Level 1), and `TNullable<T>` via extended RTTI (Level 2, trunk only). Level 2 supports strings, integers of any size, `Int64`, `QWord`, floats, dates, `Boolean`, enumerations and characters; a `TNullable<T>` of another type raises when the class is registered

### Schema Management

- **DDL generation** — `CreateSchema` generates CREATE TABLE from registered metadata, including primary keys, foreign keys, indexes, and junction tables. Foreign keys are added after all tables, so classes may reference each other. Generated constraint names are shortened to the identifier length of the database (Oracle 30, Firebird 31, PostgreSQL 63, MySQL 64) with a hash suffix; a name you give that is too long raises. `DEFAULT` values for dates are written as date literals
- **Schema migration** — `MigrateSchema` introspects the database, diffs against the registered model, and generates ALTER TABLE statements. A new table gets its indexes and foreign keys too. Type changes that can lose data (shorter `VARCHAR`, fewer `DECIMAL` digits, another type family) are skipped with a warning unless `moAllowNarrowing` is given; changes the database cannot make (SQLite: column type and nullability; Firebird 2.5: nullability) are skipped with a warning
- **Atomic schema changes** — On PostgreSQL, SQL Server and SQLite (transactional DDL) the schema changes of `MigrateSchema` run in one transaction, together with recording the version when there are no custom steps, and `CreateSchema` creates all tables and the tracking table in one transaction: a failure leaves nothing applied. Firebird, MySQL and Oracle commit each DDL statement on its own (Firebird cannot use new objects in the transaction that created them, the others commit DDL implicitly)
- **Migration tracking** — Version history recorded in a configurable table (default `_opf_migrations`), created with the column types of the dialect; one row per version
- **Custom migration steps** — `RegisterMigration` for data transformations that cannot be expressed as DDL (e.g., splitting columns, seeding data). Steps are chained: from version 1 to 4 the steps 1→2, 2→3 and 3→4 run in that order, each transition in one transaction together with recording its version, so a failing step leaves the database at the previous version and a later run continues there. A gap, two steps leaving the same version for different targets, or a step beyond the target version raises before anything is changed. Schema changes run before the custom steps
- **Migrator settings** — `SQLDialect` (detected from the connection when not set; Firebird 3+ is recognized), `ReservedWordStrategy` (must match the strategy of the SQL and DDL builders), and `TransactionPolicy` (`mtpRaise`, the default, refuses to start when the connection's own transaction is active; `mtpCommitCaller` commits it)
- **SQL script generation** — `GenerateMigrationSQL` produces a complete SQL script without executing, for use by external installers or DBAs. Optional `gsoIncludeVersionTracking` flag appends the migration tracking INSERT
- **Dry-run mode** — Preview migration SQL without executing
- **Component-based migrator** — `TOPSchemaMigrator` is a `TComponent` that can be dropped on a datamodule and wired to the same registries as `TOPManager`. Protected virtual methods (`ComputeSchemaDiffs`, `ExecuteDDLStatements`, `ExecuteCustomSteps`, `RecordMigrationVersion`) allow overriding specific phases

### Database Support

| Database | Dialect | Status |
|----------|---------|--------|
| SQLite | `dSQLite` | Full support |
| PostgreSQL | `dPostGres` | Full support |
| Interbase/Firebird | `dFirebird` / `dFirebird3` | Full support |
| MySQL | `dMySQL` | Full support |
| SQL Server | `dSQLServer` | DDL + DML |
| Oracle | `dOracle` | DDL + DML |

Any `TSQLConnection` descendant works — the framework auto-detects the dialect from the connection class.

### Object Factory

- **Customizable object creation** — Register per-class factory callbacks to control how objects are created when loading lists
- **Two callback styles** — Plain function (`TOPFactoryFunc`) or method of object (`TOPFactoryMethod`)
- **Context-aware** — Factory receives a `TOPFactoryContext` with reason (list item, child, association), list object, owner, and property name
- **Class hierarchy lookup** — A factory registered for `TAnimal` also creates `TDog` descendants
- **TComponent auto-handling** — TComponent descendants automatically receive the owner from context, no explicit factory needed
- **RTTI constructor discovery** — When using `TExtRTTIPersister`, custom parameterless constructors are automatically discovered and called (trunk only, requires `{$RTTI EXPLICIT}`)

### Auditing & Lifecycle

- **Automatic auditing columns** — `foCreatedAt`, `foUpdatedAt`, `foCreatedBy`, `foUpdatedBy` field options auto-populate timestamps and user identity during Save, with no manual code per call
- **Configurable audit identity** — `TOPAuditInfo` (`TPersistent`) on the manager supports numerical ID, string username, or object-based user (its primary key is read when it is assigned, so the object need not outlive the manager); auto-set discriminator selects the active identity type
- **Configurable timestamps** — Local or UTC via `AuditInfo.TimestampKind`
- **Soft delete** — `foSoftDelete` field option turns `Delete` into an UPDATE (Boolean→`TRUE`, DateTime→timestamp). `Load`, `LoadChildren`, `LoadAssociations` and `Load(list)` with a `TOPCriteria` automatically filter soft-deleted records (`IncludeDeleted` turns this off); `Load(list)` with a raw criteria string adds it too, unless `omoRawCriteriaSoftDeleteFilter` is removed from the manager's `Options`. Cascade soft delete propagates to children
- **Change tracking (selective UPDATE)** — Opt-in comparison against a reference state generates UPDATE statements containing only changed columns. Three trackers implement one contract, `IOPChangeTracker`, and the manager holds one of them at a time: `TOPSnapshotManager` keeps a variant array of the original property values, with `tmImplicit` (all classes tracked) / `tmExplicit` (only registered classes) / `tmNone` modes; `TOPSelfTracker` lets a class implementing `IOPDirtyTracking` keep its own snapshot; `TOPMementoTracker` keeps a `TOPMemento` the object hands out through `IOPMementoSource`, and on trunk, where the package links `Opf.Memento.ExtRTTI`, falls back to `TOPRttiMemento` built from the field map for classes that have none. An object the tracker holds no reference state for is written with all its columns, so it never loses one. Only objects that report being freed (`IFPObserved`, e.g. any `TPersistent`, including `TOPFNullableObject`) get a reference state, so a freed object can never be mistaken for a new one at the same address; other objects are always written in full. Setting the tracker's `TrackUnobserved` also tracks them: they must then be purged before they are freed. Supports explicit `MarkDirty` to force columns into the UPDATE. Skips the UPDATE entirely when nothing changed. `MaxEntries` property with FIFO eviction bounds memory. Class-validation guard detects stale pointer reuse. `Purge`/`PurgeAll` for explicit cleanup, and an outermost `Rollback` or a failing `Commit` forgets the reference states taken or dropped in that transaction (all of them when `TrackUnobserved` is set and one of those objects could not be observed); a change of only the null state of a nullable field is detected as a change
- **Memento** — `CreateMemento` hands the caller a copy of an object's state that `RestoreTo` writes back, so canceling an edit needs no second copy of its own. A hand-written memento works on every compiler and compares whatever it captured, records and `TNullable<T>` included; the framework supplies the state copy and the restore, while undo stacks stay with the application
- **Optimistic concurrency** — `foVersion` field option auto-manages a version column. `TOPManager.DeleteChecks` (default `[dckVersion, dckRowsAffected]`) makes `Delete` check the version too and raise when no row was deleted: `SErrConcurrencyConflict` for a stale versioned object, `SErrDeleteNotFound` otherwise. Set it to `[]` for the old behaviour (a delete of a missing row succeeds silently). On INSERT, version is initialized (1 for integer, new UUID for string). On UPDATE, version is auto-incremented/regenerated and `WHERE Version = :OldVersion` is appended. If zero rows are affected (another user modified the record), `EOPF` is raised with the `SErrConcurrencyConflict` message. Integrates with change tracking — version field is always included in partial UPDATEs
- **Entity lifecycle hooks** — Protected virtual `DoBeforeSave`, `DoAfterSave`, `DoAfterLoad`, `DoBeforeDelete`, `DoAfterDelete` methods on `TOPManager`, plus corresponding published events (`BeforeSave`, `AfterSave`, etc.) assignable in the IDE Object Inspector when the manager is on a datamodule

### Infrastructure

- **Transactions** — Implicit per-operation wrapping with explicit `BeginTransaction` / `Commit` / `Rollback` for multi-operation scenarios. When the database rolls back, objects get back what the manager wrote into them (generated key, version, audit fields, soft-delete flag, persistence state): at once for the objects of a failed `Save`/`Delete`/list save, and at the outermost `Rollback` for objects that report being freed (`IFPObserved`, e.g. any `TPersistent`). A connection whose own transaction is already active is refused (`ActiveTransactionPolicy = atpRaise`, the default) or used as it is (`atpJoin`). There is no two-phase commit: when the commit fails on a second connection after the first one committed, `EOPFPartialCommit` is raised. It lists the committed connections (`CommittedConnections`), the one that failed (`FailedConnection`), those rolled back without a commit attempt (`NotCommittedConnections`) and the original error (`OriginalErrorClass`, `OriginalMessage`). `TOPManager.OnPartialCommit` is called before it is raised, e.g. to log or start compensating actions
- **Multiple connections** — Named connection definitions with default designation and backend switching without code changes. An `IOPConnectionResolver` receives the class of the objects (`ResolveConnection(aClass)`) and can route classes to different connections. Connection definitions save passwords through the cipher also to streams, replace a file only when the new one is complete, keep params in a `[<name>.Params]` INI section (old files are still read), and apply `Port` and `Role` to the connection
- **Error details** — `EOPF.ErrorCode`, `SQLState` and `OriginalClassName` hold the details of the database error that caused an exception (e.g. tell a unique violation from a deadlock). For PostgreSQL the SQLSTATE comes through `Opf.PQErrors`, part of the package; other handlers can be added with `OPFRegisterErrorDetailsHandler`
- **Threads** — The singletons (registries, builders, executor, logger, factory, serializer) are created safely when several threads ask for them at once. Registering classes and relations is not synchronised: finish it before other threads use the registries
- **Extensibility** — Subclass `TOPManager`, `TOPRegistry`, or `TOPSQLBuilder`; replaceable singletons via `SetInstanceClass`
- **Centralized logging** — `TOPLogger` with configurable log kinds (SQL, Trace, Info, Error)
- **Lazarus integration** — Design-time package for visual component usage

## Quick Start

```pascal
program demo;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

uses
  SysUtils, sqlite3conn, sqldb, db,
  Opf.Manager, Opf.Registry, Opf.Persister.Rtti, Opf.Driver;

type
  TCustomer = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FName: String;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
  end;

// ... implement Get/SetPersistenceState ...

var
  Conn: TSQLite3Connection;
  Customer: TCustomer;
begin
  // Setup
  Conn := TSQLite3Connection.Create(nil);
  Conn.DatabaseName := ':memory:';
  Conn.Transaction := TSQLTransaction.Create(Conn);
  Conn.Open;

  OPRegistry.RegisterPersister(TCustomer, TRTTIPersister);
  OPManager.CreateSchema(Conn);

  // Save
  Customer := TCustomer.Create;
  Customer.ID := 1;
  Customer.Name := 'Alice';
  Customer.FState := opsNew;
  OPManager.Save(Customer);
  Customer.Free;

  // Load
  Customer := TCustomer.Create;
  Customer.ID := 1;
  OPManager.Load(Customer);
  WriteLn(Customer.Name); // Alice
  Customer.Free;

  Conn.Free;
end.
```

## JSON mapping files

`Opf.Mapping.Json` is not used by any other unit: a program that does not name it in a uses clause is not affected. The registry only offers a hook, `TOPRegistry.RegistrationListener`.

Write the default mapping once, edit the file, and load it at startup:

```pascal
uses Opf.Mapping.Json;

begin
  // Classes are registered in initialization sections
  if WriteMapping then
    SaveMapping('mapping.json')
  else
    LoadMapping('mapping.json');
end.
```

A class that cannot be registered yet, because it has no `ID` or `<Class>ID` property, can still be written. `SaveDefaultMapping` registers the classes in a private registry and puts `foInKey` on the first field of a class without a key:

```pascal
SaveDefaultMapping('mapping.json', [TCustomer, TOrder], TRTTIPersister);
```

A class entry lists only what differs from the default mapping; every key is optional:

```json
{
  "version": 1,
  "classes": {
    "TPerson": {
      "table": "PERSONS",
      "pkConstraint": "PK_PERSONS",
      "indexes": [ { "name": "IX_PERSON_NAME", "fields": ["LastName", "FirstName"] } ],
      "fields": {
        "PersonNr": { "column": "PERSON_NR", "options": ["foInKey", "foInSelect", "foInInsert"] },
        "LastName": { "column": "LAST_NAME", "maxSize": 80, "default": "" },
        "Notes":    { "skip": true }
      },
      "relations": [ { "parent": "TDepartment", "foreignKey": "DepartmentID", "constraint": "FK_PERSON_DEPT", "dbCascade": false } ]
    }
  },
  "manyToMany": [ { "classA": "TPerson", "classB": "TTag", "junctionTable": "PERSON_TAG", "foreignKeyA": "PERSON_ID", "foreignKeyB": "TAG_ID" } ]
}
```

- `LoadMapping` applies the file to the classes that are already registered, and to classes registered later, through the registry hook. Call it before the manager loads or saves objects.
- `LoadMapping(FileName, TRTTIPersister)` also registers the classes of the file that are not registered yet. They are found with `Classes.GetClass`, so they must be registered with `RegisterClass`. Such a definition has `AutoCreated` set: a later `RegisterPersister` for the same class replaces it instead of raising.
- A key that is present replaces the value of the default mapping; `options` is always the complete set of options of the field. When a class entry puts `foInKey` on a field, the fields without an entry lose `foInKey`, so the file defines the whole primary key.
- The `fields` of an index are property or column names; they are stored as column names. `foreignKey` is a column or property name. A relation or many-to-many link in the file replaces one registered in code; it is registered as soon as both classes are registered.
- The last change wins: settings made in code after `RegisterPersister` returns override the file for classes registered after `LoadMapping`.
- A field entry for a property that is not mapped, an unknown key or option, or a value of the wrong JSON type raises `EOPFMapping`. Class names may be written as `UnitName.ClassName`.
- `TOPJSONMapping` does the same for a registry other than the global one (`Registry` and `M2MRegistry` properties).

## Building

### With FPC (command line)

Compile any demo or program with the `-Fu` flag pointing to the source directory:

```bash
fpc -Fu<path-to-fcl-opf>/src your_program.pas
```

For example, from a demo subdirectory:

```bash
cd demos/basic_crud
fpc -Fu../../src demo_basic_crud.lpr
./demo_basic_crud
```

### With Lazarus

1. Open and install the package `ide/fcl_opf.lpk`
2. Open any demo `.lpi` file — the package dependency is already configured
3. Build and run

A project group file `fcl_opf.lpg` is provided that references both packages and all demo projects.

### Compiler Compatibility

- **FPC 3.2.2 (release)** — Full support. Extended RTTI features (`TExtRTTIPersister`, `TNullable<T>` mapping, `TOPRttiMemento`) are left out automatically: the package does not link the trunk-only units, and the code that would reach them is guarded by `{$IFNDEF VER3_2}`; a memento is written by hand there.
- **FPC trunk (3.3.1+)** — Full support including extended RTTI, attribute-driven mapping, and the generic memento of `Opf.Memento.ExtRTTI`.

## Examples

The `demos/` directory contains 39 self-contained example applications:

| Demo | Description |
|------|-------------|
| `basic_crud/` | Register a class, Save, Load, Update, Delete |
| `conventions/` | Convention-based naming and auto-mapping |
| `create_table/` | DDL generation from field maps |
| `auditing/` | Automatic auditing columns, lifecycle hooks, and user tracking |
| `autogenerate/` | Auto-increment, UUID, and custom PK generation |
| `relationships/` | Parent-child relationships and cascade delete |
| `m2m/` | Many-to-many relationships end-to-end |
| `criteria/` | Parameterized criteria API with pagination and soft-delete bypass |
| `linq/` | LINQ-style operator syntax for queries |
| `custom_sql/` | User-provided SQL with ORM result mapping |
| `custom_persister/` | Custom persister class implementation |
| `interface/` | Interface-driven persister (no RTTI) |
| `json_blob/` | Class-typed properties stored as JSON columns |
| `json_mapping/` | Table and column mapping from a JSON file, for FPC 3.2.2 |
| `nullable/` | Three levels of nullable field support |
| `dirty_implicit/` | Dirty tracking with global snapshot manager in implicit mode (all classes tracked) |
| `dirty_explicit/` | Dirty tracking with global snapshot manager in explicit mode (only registered classes) |
| `dirty_interface/` | Change tracking with a TOPSelfTracker over the per-class IOPDirtyTracking interface, with MarkDirty |
| `memento/` | Change tracking with a hand-written memento, and an edit canceled by restoring one |
| `version_integer/` | Optimistic concurrency with integer version column and conflict detection |
| `version_uuid/` | Optimistic concurrency with UUID version column and conflict detection |
| `soft_delete/` | Soft delete with automatic load filtering and cascade |
| `transactions/` | Implicit and explicit transaction support |
| `multidb/` | Switching database backends |
| `migration/` | Incremental ORM adoption alongside raw SQL |
| `factory_explicit/` | Custom factory callback for object creation during list loading |
| `factory_component/` | Automatic TComponent owner passing via factory context |
| `factory_rtti/` | RTTI-based constructor discovery with TExtRTTIPersister (trunk only) |
| `lazarus/` | Lazarus design-time component usage |
| `save_list/` | Save an entire list in one call (insert/update/delete by state) |
| `generate_migration/` | Generate migration SQL script for external execution (v1→v2 workflow) |
| `film_library/` | Full Lazarus GUI application with films, directors, actors, genres |
| `rest_server/` | REST API server exposing the film library via HTTP (fphttpserver + httproute) |
| `lazy_children_interface/` | Lazy loading of 1:N children via IOPLazyInit (FPC 3.2.2+) |
| `lazy_children_auto/` | Lazy loading of 1:N children via automatic RTTI setup (trunk only) |
| `lazy_m2m_interface/` | Lazy loading of M:N associations via IOPLazyInit (FPC 3.2.2+) |
| `lazy_m2m_auto/` | Lazy loading of M:N associations via automatic RTTI setup (trunk only) |
| `lazy_single_interface/` | Lazy loading of N:1 single object via IOPLazyInit (FPC 3.2.2+) |
| `lazy_single_auto/` | Lazy loading of N:1 single object via automatic RTTI setup (trunk only) |

### Schema Migration Examples

The `demos/migration/` directory includes 10 v1/v2 pairs demonstrating specific migration scenarios:

| Example | Scenario |
|---------|----------|
| `01_add_table/` | Add a new table |
| `02_add_column/` | Add a column to an existing table |
| `03_drop_column/` | Drop a column (with `moAllowDropColumn`) |
| `04_add_relation/` | Add a foreign key relationship |
| `05_drop_relation/` | Remove a foreign key relationship |
| `06_add_m2m/` | Add a many-to-many junction table |
| `07_drop_m2m/` | Remove a many-to-many junction table |
| `08_custom_step/` | Custom data migration callback (split FullName into FirstName + LastName) |
| `09_add_unique/` | Add a unique constraint (shows failure on duplicate data) |
| `10_add_default/` | Add a column with a default value |

Run v1 first to create the database, then v2 to migrate it:

```bash
cd demos/migration/01_add_table
fpc -Fu../../../src add_table_v1.lpr && ./add_table_v1
fpc -Fu../../../src add_table_v2.lpr && ./add_table_v2
```

## Test Suite

The test suite covers all public API methods with 1000+ tests across unit tests and multi-database integration tests:

```bash
cd tests
fpc -FUlib -Fu../src testfpopf.lpr
./testfpopf --all --format=plain
```

Integration tests run against SQLite (in-memory), PostgreSQL, Firebird, and MySQL when configured via `tests/databases.ini`. 
Tests for unavailable backends are automatically skipped. For a Firebird database file owned by the
server, set `Host=localhost` in the `[Firebird]` section: without it, the client opens the file directly.

## Upgrading

Behaviour that changed with the review fixes:

- **Criteria grouping** — `Where(a).AddOr(b).AddAnd(c)` now means `(a OR b) AND c` (it used to be generated as `a OR b AND c`), and an `OR` no longer bypasses the soft-delete filter.
- **Unknown field names** in criteria and `OrderBy` raise an exception instead of being copied into the SQL. Use `WhereRaw` for expressions; `OrderBy` accepts only a field name with optional `ASC`/`DESC`.
- **Empty `WhereIn`** matches nothing (and empty `WhereNotIn` everything) instead of producing invalid SQL.
- **Date/time and currency storage** — new tables use native date/time columns and `DECIMAL(18,4)`, and parameters are passed as date/time and BCD. For an existing database with numeric date columns, set `OPFDateTimeStorage := dtsFloat` before use.
- **Auto-increment keys** — Firebird 3+ uses identity columns, SQL Server `IDENTITY`, Oracle 12c+ identity columns, `Int64` keys `BIGINT`/`BIGSERIAL`. `dFirebird` (2.5) cannot generate auto-increment columns: use a sequence.
- **SQL Server string columns** are `NVARCHAR`/`NCHAR`, so they hold any Unicode text; set `TSQLDriverSQLServer.UnicodeStrings := False` for `VARCHAR`/`CHAR` in the code page of the column's collation.
- **Active connection transaction** — when the connection's own transaction is active, the manager raises `EOPFTransaction` instead of failing in sqldb; set `ActiveTransactionPolicy := atpJoin` to use that transaction.
- **Rollback and change tracking** — only the reference states touched in the rolled back transaction are discarded.
- **Migrations** — custom steps are chained from version to version, and each transition records its version.
- **Quoting** — the SQL and DDL builders quote with the driver's quote character (backtick for MySQL) unless `QuoteChar` is set; identifiers that are not valid unquoted (e.g. `_name` on Firebird) are always quoted.
- **Criteria** — a second `Where` replaces the conditions and parameters of the first; a sub-criteria added with `AddAnd`/`AddOr` brings its `OrderBy`, `Limit`/`Offset` and `IncludeDeleted` along; `Limit(0)` returns no rows (it used to mean "no limit"), and `Limit`/`Offset` below -1 raise.
- **Cardinal columns** are created as `BIGINT` (they were `INTEGER`, which overflows above `High(Integer)`). `MigrateSchema` widens existing `INTEGER` columns to `BIGINT`.
- **Strings** — `UnicodeString`/`WideString` properties and Unicode values in criteria and `LoadSQL` are passed as Unicode parameters instead of through the system code page.
- **Migrations** — narrowing type changes need `moAllowNarrowing`; type and nullability changes the database cannot make are skipped instead of failing the whole migration; on PostgreSQL, SQL Server and SQLite the DDL of one migration is applied in one transaction.
- **Generated names** — internal parameters are named `opf_sd` and `opf_pk` (were `sdparam` and `pkvalue`); a column whose name is not a plain identifier, or starts with `opf_`, gets a generated parameter name in INSERT/UPDATE/DELETE statements (use `TOPSQLBuilder.FieldParamName`). Generated constraint names that are too long for the database are shortened.
- **Delete** — by default a delete of a row that is not there (any more) raises, and a versioned object is only deleted at its loaded version (`DeleteChecks`; `[]` restores the old behaviour). Soft-deleting a parent no longer hard-deletes its children without a soft-delete field, and hard-deleting a parent also hard-deletes its soft-deletable children. Cascades from a parent with a composite key raise.
- **`stored` methods** — only a `stored` specifier that names a Boolean field makes a property nullable. A method (the streaming "is not the default" test) no longer does, so a value equal to the default is written instead of NULL; set `foNullable` explicitly to keep the old behaviour.
- **`opContains`** escapes `%`, `_` and `!` in the text (`ESCAPE '!'`); `opLike`/`opNotLike` keep the pattern as given. `opContains`, `opLike` and `opNotLike` with a number, Boolean or date raise.
- **Auto-increment keys** that a dialect cannot return (`dVanilla`) raise instead of leaving the key 0. SQL Server retrieves the key in the same batch as the INSERT, Oracle with `RETURNING ... INTO`.
- **`Load(Object, KeyOnly=True)`** reads only the key columns (an existence check) and leaves the object in state `opsKey`.
- **The connection resolver** receives the class of the objects instead of `nil`.
- **Connection INI files** keep params in a `[<name>.Params]` section; files in the old format are still read.
- **`RegisterPersister(Class, Nil)`** gives an empty field map instead of `nil`.
- **`OPRelation` attributes** of a class registered in a registry other than the global one go into that registry's `RelationRegistry`.
- **`Logger`** must be a `TOPLogger`.
- **Updates of audited objects** — an object without changes is no longer updated just to set its `foUpdatedAt`/`foUpdatedBy` fields.
- **`Save(Object, State, Persister)`** commits on its own, like the other `Save` overloads.
- **Class properties** — a `nil` `TStrings` property gets a `TStringList`, a `nil` `TCollection` property needs a factory (`OPFactory`, reason `frProperty`); NULL clears a `TStrings` or `TCollection`.
- **The RTTI factory** of an extended RTTI class no longer creates descendants with the base class constructor.

## Documentation

See [docs/architecture.md](docs/architecture.md) for the full architectural overview, 
including component design, persister paths, transaction model, and extension points.

## License

Modified LGPL — see the package file for details.

## Author

Michael Van Canneyt
