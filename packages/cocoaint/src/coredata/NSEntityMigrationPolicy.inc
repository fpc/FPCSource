{ Parsed from Coredata.framework NSEntityMigrationPolicy.h }

{$ifdef TYPES}
{$ifndef NSENTITYMIGRATIONPOLICY_PAS_T}
{$define NSENTITYMIGRATIONPOLICY_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSENTITYMIGRATIONPOLICY_PAS_R}
{$define NSENTITYMIGRATIONPOLICY_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSENTITYMIGRATIONPOLICY_PAS_F}
{$define NSENTITYMIGRATIONPOLICY_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSENTITYMIGRATIONPOLICY_PAS_S}
{$define NSENTITYMIGRATIONPOLICY_PAS_S}

{ External string constants }
var
  NSMigrationManagerKey: NSString; cvar; external;
  NSMigrationSourceObjectKey: NSString; cvar; external;
  NSMigrationDestinationObjectKey: NSString; cvar; external;
  NSMigrationEntityMappingKey: NSString; cvar; external;
  NSMigrationPropertyMappingKey: NSString; cvar; external;
  NSMigrationEntityPolicyKey: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  NSEntityMigrationPolicy = objcclass;
  NSEntityMigrationPolicyPointer = ^NSEntityMigrationPolicy;
  NSEntityMigrationPolicyPtr = NSEntityMigrationPolicyPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSENTITYMIGRATIONPOLICY_PAS_C}
{$define NSENTITYMIGRATIONPOLICY_PAS_C}

{ NSEntityMigrationPolicy }
  NSEntityMigrationPolicy = objcclass external (NSObject)
    
  public
    function beginEntityMapping_manager_error(mapping: NSEntityMapping; manager: NSMigrationManager; error: NSErrorPointer): Boolean; message 'beginEntityMapping:manager:error:';
    function createDestinationInstancesForSourceInstance_entityMapping_manager_error(sInstance: NSManagedObject; mapping: NSEntityMapping; manager: NSMigrationManager; error: NSErrorPointer): Boolean; message 'createDestinationInstancesForSourceInstance:entityMapping:manager:error:';
    function endInstanceCreationForEntityMapping_manager_error(mapping: NSEntityMapping; manager: NSMigrationManager; error: NSErrorPointer): Boolean; message 'endInstanceCreationForEntityMapping:manager:error:';
    function createRelationshipsForDestinationInstance_entityMapping_manager_error(dInstance: NSManagedObject; mapping: NSEntityMapping; manager: NSMigrationManager; error: NSErrorPointer): Boolean; message 'createRelationshipsForDestinationInstance:entityMapping:manager:error:';
    function endRelationshipCreationForEntityMapping_manager_error(mapping: NSEntityMapping; manager: NSMigrationManager; error: NSErrorPointer): Boolean; message 'endRelationshipCreationForEntityMapping:manager:error:';
    function performCustomValidationForEntityMapping_manager_error(mapping: NSEntityMapping; manager: NSMigrationManager; error: NSErrorPointer): Boolean; message 'performCustomValidationForEntityMapping:manager:error:';
    function endEntityMapping_manager_error(mapping: NSEntityMapping; manager: NSMigrationManager; error: NSErrorPointer): Boolean; message 'endEntityMapping:manager:error:';
  end;

{$endif}
{$endif}
