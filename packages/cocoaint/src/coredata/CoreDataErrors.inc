{ Parsed from Coredata.framework CoreDataErrors.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Fri Oct 2 11:46:33 EDT 2009 }


{$ifdef TYPES}
{$ifndef COREDATAERRORS_PAS_T}
{$define COREDATAERRORS_PAS_T}

{ Constants }

const
  NSManagedObjectValidationError = 1550;
  NSValidationMultipleErrorsError = 1560;
  NSValidationMissingMandatoryPropertyError = 1570;
  NSValidationRelationshipLacksMinimumCountError = 1580;
  NSValidationRelationshipExceedsMaximumCountError = 1590;
  NSValidationRelationshipDeniedDeleteError = 1600;
  NSValidationNumberTooLargeError = 1610;
  NSValidationNumberTooSmallError = 1620;
  NSValidationDateTooLateError = 1630;
  NSValidationDateTooSoonError = 1640;
  NSValidationInvalidDateError = 1650;
  NSValidationStringTooLongError = 1660;
  NSValidationStringTooShortError = 1670;
  NSValidationStringPatternMatchingError = 1680;
  NSManagedObjectContextLockingError = 132000;
  NSPersistentStoreCoordinatorLockingError = 132010;
  NSManagedObjectReferentialIntegrityError = 133000;
  NSManagedObjectExternalRelationshipError = 133010;
  NSManagedObjectMergeError = 133020;
  NSPersistentStoreInvalidTypeError = 134000;
  NSPersistentStoreTypeMismatchError = 134010;
  NSPersistentStoreIncompatibleSchemaError = 134020;
  NSPersistentStoreSaveError = 134030;
  NSPersistentStoreIncompleteSaveError = 134040;
  NSCoreDataError = 134060;
  NSPersistentStoreOperationError = 134070;
  NSPersistentStoreOpenError = 134080;
  NSPersistentStoreTimeoutError = 134090;
  NSPersistentStoreIncompatibleVersionHashError = 134100;
  NSMigrationError = 134110;
  NSMigrationCancelledError = 134120;
  NSMigrationMissingSourceModelError = 134130;
  NSMigrationMissingMappingModelError = 134140;
  NSMigrationManagerSourceStoreError = 134150;
  NSMigrationManagerDestinationStoreError = 134160;
  NSEntityMigrationPolicyError = 134170;
  NSSQLiteError = 134180;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef COREDATAERRORS_PAS_R}
{$define COREDATAERRORS_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef COREDATAERRORS_PAS_F}
{$define COREDATAERRORS_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef COREDATAERRORS_PAS_S}
{$define COREDATAERRORS_PAS_S}

{ External string constants }
var
  NSDetailedErrorsKey: NSString; external name '_NSDetailedErrorsKey';
  NSValidationObjectErrorKey: NSString; external name '_NSValidationObjectErrorKey';
  NSValidationKeyErrorKey: NSString; external name '_NSValidationKeyErrorKey';
  NSValidationPredicateErrorKey: NSString; external name '_NSValidationPredicateErrorKey';
  NSValidationValueErrorKey: NSString; external name '_NSValidationValueErrorKey';
  NSAffectedStoresErrorKey: NSString; external name '_NSAffectedStoresErrorKey';
  NSAffectedObjectsErrorKey: NSString; external name '_NSAffectedObjectsErrorKey';
  NSSQLiteErrorDomain: NSString; external name '_NSSQLiteErrorDomain';

{$endif}
{$endif}
