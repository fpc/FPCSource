{
    This file is part of the Free Pascal component  library.
    Copyright (c) 2026 by Michael Van Canneyt
    member of the Free Pascal development team

    String constants

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit Opf.Consts;

{$mode objfpc}{$H+}

interface

Resourcestring
  SErrRegistryCreated = 'a registry instance was already created';
  SErrManagerCreated = 'a manager instance was already created';
  SErrSQLBuilderCreated = 'a SQL builder instance was already created';
  SDuplicateFieldName = 'Duplicate field name : "%s"';
  SDuplicatePropertyName = 'Duplicate property name : "%s"';
  SErrNoStateSupport = 'Class "%s" does not have persistent state support';
  SErrNeedClass = 'InstanceClass may not be Nil when registering a class';
  SErrDuplicatePersister = 'Duplicate persister for class "%s"';
  SErrUnsupportedPropertyType = 'Unsupported object property type: "%s"';
  SErrUnsupportedNullableType = 'Class %s: nullable field %s has an unsupported value type %s';
  SErrCollectionNeedsFactory = 'Class %s: property %s is nil and a %s cannot be created without its item class: create it in the constructor or register a factory (OPFactory)';
  SErrUnableToGetStringLength = 'Unable to determine string length for field "%s.%s"';
  SErrUnsupportedIntegerBound = 'Unsupported integer field length bound for field "%s.%s": %d';
  SErrUnsupportedFloatBound = 'Unsupported float field length bound for field "%s.%s": %d';
  SErrNoFieldsForClassRefPersister = 'Cannot determine primary key fields from property %s of class %s, table %s: no persister';
  SErrNoFieldsForClassRefField = 'Cannot determine primary key fields from property %s of class %s, table %s: no referenced field %s';
  SErrDeleteNoKeyFields = 'DELETE requires at least one key field (foInKey) in the field map';
  SErrUpdateNoKeyFields = 'UPDATE requires at least one key field (foInKey) in the field map';
  SErrSerializeNilObject = 'aObject must not be nil';
  SErrDeserializeNilObject = 'aObject must not be nil';
  SErrDeserializeInvalidJSON = 'Failed to parse JSON: %s';
  SErrDeserializeTypeMismatch = 'JSON type mismatch: expected %s but got %s';
  SErrListRegCreated = 'an instance was already created';
  SErrDuplicateListClass = 'Duplicate list class "%s"';
  SErrNeedListClass = 'ListClass may not be Nil when registering a list class';
  SErrNeedManipulator = 'Manipulator may not be Nil when registering a list class';
  SErrNoPrimaryKey = 'No primary key identified for class "%s". Add foInKey to a field, name a property ID or %sID, or implement IOPPrimaryKey';
  SErrPKPropertyNotFound = 'Convention-detected primary key property "%s" not found in field map for class "%s"';
  SErrExecutorCreated = 'An instance was already created';
  SErrExecutorNilConnection = 'Connection is nil';
  SErrExecutorNilPersister = 'Persister is nil';
  SErrExecutorNilFieldMap = 'FieldMap is nil or empty';
  SErrExecutorNilObject = 'Object instance is nil';
  SErrExecutorEmptySQL = 'SQL statement is empty';
  SErrExecutorExecFailed = 'SQL execution failed: %s';
  SErrExecutorMissingColumn = 'Result set missing column "%s" for mapped field "%s"';
  SErrExecutorQueryFailed = 'Query execution failed: %s';
  SErrExecutorNilList = 'List object is nil';
  SErrExecutorNilManipulator = 'List manipulator is nil';
  SErrExecutorNilClass = 'Object class is nil';
  SErrTransactionCreated = 'An instance was already created';
  SErrTransactionNilConnection = 'Connection is nil';
  SErrTransactionBeginFailed = 'Failed to begin transaction: %s';
  SErrTransactionCommitFailed = 'Failed to commit transaction: %s';
  SErrTransactionRollbackFailed = 'Failed to rollback transaction: %s';
  SErrIntegrationBackendUnavailable = '%s backend is not available - skipping tests';
  SErrNoDefaultConnection = 'No default connection configured';
  SErrNoPersisterForClass = 'No persister registered for class "%s"';
  SErrDefaultConnectionNil = 'Default connection has no SingleConnection assigned';
  SErrUnsupportedPersistenceState = 'Unsupported persistence state for Save: %d';
  SErrLoadNotFound = 'No record found for class "%s" with the given primary key';
  SErrLoadNoKeyFields = 'Load requires at least one key field (foInKey) for class "%s"';
  SErrLoadNilList = 'List object is nil';
  SErrLoadNilClass = 'Object class is nil';
  SErrLoadNoManipulator = 'No list manipulator registered for class "%s"';
  SErrExecutorAutoIncRetrieval = 'Failed to retrieve auto-increment value for class "%s"';
  SErrExecutorUUIDGenFailed = 'Failed to generate UUID for field "%s" of class "%s"';
  SErrExecutorSequenceRetrieval = 'Failed to retrieve sequence value for "%s" on class "%s": %s';
  SErrExecutorSequenceNotSupported = 'Sequence-based generation not supported for dialect "%s"';
  SErrExecutorCustomGenFailed = 'Custom value generator failed for field "%s" of class "%s": %s';
  SErrRelationRegistryCreated = 'An instance was already created';
  SErrRelationParentNotRegistered = 'Parent class "%s" is not registered in OPRegistry';
  SErrRelationChildNotRegistered = 'Child class "%s" is not registered in OPRegistry';
  SErrDuplicateRelationship = 'Duplicate relationship for parent "%s" and child "%s"';
  SErrRelationForeignKeyNotFound = 'Foreign key field "%s" not found in child class "%s" field map';
  SErrCascadeDeleteFailed = 'Cascade delete failed for child class "%s" of parent "%s": %s';
  SErrCascadeCycleDetected = 'Circular relationship detected during cascade delete at class "%s"';
  SErrCascadeChildNoPK = 'Cannot cascade delete through class "%s": no primary key field defined but child relations exist';
  SErrLoadChildrenNoRelations = 'No child relations registered for class "%s"';
  SErrLoadChildrenAmbiguous = 'Multiple child relations for class "%s" - specify child class explicitly';
  SErrLoadChildrenPropertyNotFound = 'List property "%s" not found on class "%s"';
  SErrLoadChildrenNoMatchingProperty = 'No matching list property found on class "%s" for child class "%s"';
  SErrLoadChildrenRelationNotFound = 'No child relation for class "%s" with child class "%s"';
  SErrClassDoesNotSupportIOPFieldProvider = 'Class "%s" does not support IOPFieldProvider interface';
  SErrNoActiveTransaction = 'No active transaction - call BeginTransaction first';
  SErrTransactionDoomedRollback = 'Transaction was previously rolled back';
  SErrTransactionDoomedRollbackFailed = 'Transaction was previously rolled back; rollback failed: %s';
  SErrRollbackFailed = 'Rollback failed: %s';
  SErrConnectionTransactionActive = 'Connection "%s" already has an active transaction "%s": commit or roll it back before using the manager, or set ActiveTransactionPolicy to atpJoin';
  SErrPartialCommit = 'Commit failed after %d of %d connections were committed; data may be inconsistent: %s';
  SErrPartialCommitHandler = 'OnPartialCommit handler failed: %s';
  SErrLoadSQLOddParamCount = 'LoadSQL array of const requires an even number of elements (alternating name/value pairs)';
  SErrLoadSQLParamNameNotString = 'LoadSQL parameter name must be a string type';
  SErrLoadSQLEmptySQL = 'SQL statement is empty';
  SErrDDLEmptyFieldMap = 'CREATE TABLE requires at least one field in the field map';
  SErrEmptyTableName = 'Table name must not be empty';
  SErrUnsupportedConstraintType = 'Unsupported constraint type: %d';
  SErrIfNotExistsNotSupported = 'IF NOT EXISTS is not supported by %s dialect';
  SErrFKParentNotRegistered = 'Cannot create foreign key: parent class "%s" not registered';
  SErrFKChildNotRegistered = 'Cannot create foreign key: child class "%s" not registered';
  SErrFKParentNoPrimaryKey = 'Cannot create foreign key: parent class "%s" has no primary key field';
  SErrUnsafeIdentifier = 'SQL identifier "%s" contains unsafe characters';
  SErrUnsafeCriteria = 'SQL criteria contains unsafe pattern "%s" - use parameterized queries';
  SErrCriteriaUnknownField = 'Criteria field "%s" is not a mapped property or column - use WhereRaw for SQL expressions';
  SErrCriteriaInvalidField = 'Criteria field name "%s" is not a valid identifier';
  SErrCriteriaInvalidOrderBy = 'Invalid ORDER BY item "%s": expected a field name optionally followed by ASC or DESC';
  SErrCriteriaSelf = 'A criteria cannot be combined with itself';
  SErrCriteriaAlreadyOwned = 'Sub-criteria was already added to this criteria';
  SErrCriteriaConflictingPaging = 'Sub-criteria %s (%d) conflicts with the %s of the criteria (%d)';
  SErrCriteriaInvalidPaging = 'Invalid %s value %d: use -1 for no %s';
  SErrCriteriaBetweenCount = 'WhereBetween requires exactly 2 values, got %d';
  SErrCriteriaLikeNeedsText = 'LIKE on field "%s" needs a text value';
  SErrQWordOutOfRange = 'Value %u is too large: unsigned 64-bit values are stored as signed 64-bit integers (maximum 9223372036854775807)';
  SErrUnsupportedVarRecParam = 'Unsupported array of const element type %d for a parameter value';
  SErrUnsupportedVariantParam = 'Unsupported variant type %d for a parameter value';
  SErrNoUpdatableFields = 'UPDATE of table "%s" has no updatable fields';
  SErrMigrationCallerTransaction = 'Connection "%s" has an active transaction: commit or roll it back before migrating, or set TransactionPolicy to mtpCommitCaller';
  SErrMigrationAmbiguousStep = 'Migration steps from version %d lead to different versions (%d and %d)';
  SErrMigrationStepGap = 'No migration step from version %d, but a step from version %d is registered';
  SErrMigrationStepBackwards = 'Migration step from version %d leads to version %d, which is not higher';
  SErrMigrationStepOvershoot = 'Migration step from version %d leads to version %d, beyond the target version %d';
  SErrDDLStatementFailed = 'DDL statement %d of %d failed, nothing was applied: %s'+LineEnding+'%s';
  SErrIdentifierTooLong = 'Name "%s" is longer than %d characters, the maximum for %s';
  SErrDropIndexNeedsTable ='Dropping index "%s" requires the table name for dialect %s';
  SErrAutoIncNeedsFirebird3 ='Auto-increment column %s requires Firebird 3 or later (dFirebird3): use a sequence for older versions';
  SErrAutoIncCompositeKey = 'Table "%s": an auto-increment column cannot be part of a composite primary key';
  SErrLoadNilCriteria ='Load: criteria is nil';
  SErrLinqEmptyExpression ='LINQ expression is not assigned';
  SErrCriteriaRawParamCount = 'Raw criteria "%s" has %d placeholders but %d values were given';
  SErrConflictingFieldOptions = 'foRequired and foNullable cannot be set simultaneously';
  SErrM2MRegistryCreated = 'An instance was already created';
  SErrM2MClassANotRegistered = 'ClassA "%s" is not registered in OPRegistry';
  SErrM2MClassBNotRegistered = 'ClassB "%s" is not registered in OPRegistry';
  SErrDuplicateM2M = 'Duplicate M2M relationship for classes "%s" and "%s"';
  SErrM2MSelfRelation = 'ClassA and ClassB cannot be the same class "%s"';
  SErrM2MEmptyJunctionTable = 'Junction table name cannot be empty';
  SErrM2MEmptyForeignKeyA = 'ForeignKeyA cannot be empty';
  SErrM2MEmptyForeignKeyB = 'ForeignKeyB cannot be empty';
  SErrM2MNilClassA = 'ClassA may not be Nil';
  SErrM2MNilClassB = 'ClassB may not be Nil';
  SErrM2MRelationNotFound = 'No many-to-many relation found for %s and %s';
  SErrM2MAmbiguousProperty = 'Ambiguous M2M property name "%s" for class %s - multiple relations match';
  SErrM2MPropertyNotFound = 'No M2M relation with property name "%s" found for class %s';
  SErrM2MDuplicateAssociation = 'Association already exists between %s (PK=%s) and %s (PK=%s) in junction table "%s"';
  SErrJunctionClassANotRegistered = 'Cannot create junction table: ClassA "%s" not registered';
  SErrJunctionClassBNotRegistered = 'Cannot create junction table: ClassB "%s" not registered';
  SErrJunctionClassANoPrimaryKey = 'Cannot create junction table: ClassA "%s" has no primary key field';
  SErrJunctionClassBNoPrimaryKey = 'Cannot create junction table: ClassB "%s" has no primary key field';
  SErrCircularDependency = 'Circular dependency detected among registered classes';
  SErrAlterColumnNotSupported = 'ALTER COLUMN is not supported by %s dialect - use recreate-table pattern';
  SErrMigrationPersisterNotFound = 'No persister registered for table "%s"';
  SErrMigrationFieldNotFound = 'Field "%s" not found in persister for table "%s"';
  SErrMigrationRelationNotFound = 'No relation found for FK column "%s" on table "%s"';
  SErrMigrationM2MNotFound = 'No M2M definition found for junction table "%s"';
  SErrMigrationIndexNotFound = 'Index "%s" not found in persister for table "%s"';
  SErrMigrationTableCreate = 'Failed to create migration tracking table: %s';
  SErrMigrationRecordFailed = 'Failed to record migration version %d: %s';
  SErrAuditUserNotSet = 'Audit user not configured (UserIDKind=ukNone) but field "%s" has %s option';
  SErrAuditUserConversion = 'Cannot convert audit user value to field type for field "%s": %s';
  SErrAuditUserNoPrimaryKey = 'Cannot find the primary key of audit ObjectID (class "%s"): implement IOPPrimaryKey or register a persister with a key field';
  SErrDuplicateSoftDeleteField = 'Only one field per class may have the foSoftDelete option';
  SErrSoftDeleteFieldType = 'foSoftDelete field "%s" must be Boolean (tkBool) or DateTime (tkFloat)';
  SErrDuplicateVersionField = 'Only one field per class may have the foVersion option';
  SErrVersionFieldType = 'foVersion field "%s" must be Integer (tkInteger/tkInt64) or String (AnsiString/UnicodeString) for UUID';
  SErrVersionFieldInKey = 'foVersion field "%s" must not have foInKey - the version WHERE clause is managed automatically';
  SErrConcurrencyConflict = 'Optimistic concurrency conflict on class "%s": the record was modified or deleted by another user since it was loaded';
  SErrDeleteNotFound = 'Delete of class "%s": the record does not exist (any more)';
  SErrCannotReplaceFile = 'Cannot replace file "%s"';
  SErrParentNotRegistered = 'Parent class "%s" must be registered before child class "%s"';
  SErrAmbiguousParentClass = 'Parent class name "%s" matches more than one registered class: use UnitName.ClassName';
  SErrTaskUndoFailed = 'Undo after a failed execute failed: %s';
  SErrExecutorAutoIncUnsupported = 'Cannot retrieve the generated key of class "%s": dialect %s has no way to return it';
  SErrSequenceOutOfRange = 'sequence value %d does not fit in the property type';
  SErrCascadeCompositeKey = 'Cannot delete class "%s": cascading deletes need a single-column primary key';
  SErrMementoNilSubject = 'Memento subject may not be Nil';
  SErrMementoClassMismatch = 'Memento declares class "%s" but the subject is of class "%s"';
  SErrMementoNoClass = 'Memento "%s" has no declared class';

implementation

end.

