# Schema Migration Examples

Each subdirectory contains a **v1/v2 pair** demonstrating a specific schema migration scenario.

**Usage:** Run the `_v1` program first to create the database, then `_v2` to migrate it.

```
cd 01_add_table
fpc -Fu../../../src add_table_v1.lpr && ./add_table_v1
fpc -Fu../../../src add_table_v2.lpr && ./add_table_v2
```

## Example Index

| # | Directory | v1 State | v2 Change | Key Concept |
|---|-----------|----------|-----------|-------------|
| 01 | `01_add_table/` | Person | + Address table | CREATE TABLE via migration |
| 02 | `02_add_column/` | Person(ID, Name) | + Email column | ALTER TABLE ADD COLUMN |
| 03 | `03_drop_column/` | Person(ID, Name, Nickname) | - Nickname | ALTER TABLE DROP COLUMN (moAllowDropColumn) |
| 04 | `04_add_relation/` | Person + Department | + FK Person.DeptID -> Department | ALTER TABLE + foreign key constraint |
| 05 | `05_drop_relation/` | Person -> Department FK | - relation removed | DROP foreign key constraint |
| 06 | `06_add_m2m/` | Person + Skill | + M2M junction table | CREATE junction table |
| 07 | `07_drop_m2m/` | Person <-> Skill M2M | - M2M removed | DROP junction table (moAllowDropTable) |
| 08 | `08_custom_step/` | Person(FullName) | FirstName + LastName | RegisterMigration callback for data transform |
| 09 | `09_add_unique/` | Person(Email) with duplicates | + foUnique on Email | Shows migration failure on duplicate data |
| 10 | `10_add_default/` | Person(ID, Name) | + Status with DefaultValue | New column with default value |

## Notes

- All examples use SQLite file-based databases (`.db` files)
- v1 always creates the schema at version 1 via `CreateSchema`
- v2 always migrates to version 2 via `MigrateSchema`
- Examples 03, 07 demonstrate destructive operations that require explicit opt-in options
- Example 08 demonstrates the `RegisterMigration` API for custom data transformations
- Example 09 intentionally shows what happens when migration constraints conflict with existing data
