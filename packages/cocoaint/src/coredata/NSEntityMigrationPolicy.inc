{ Parsed from Coredata.framework NSEntityMigrationPolicy.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Fri Oct 2 11:46:33 EDT 2009 }


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
  NSMigrationManagerKey: NSString; external name '_NSMigrationManagerKey';
  NSMigrationSourceObjectKey: NSString; external name '_NSMigrationSourceObjectKey';
  NSMigrationDestinationObjectKey: NSString; external name '_NSMigrationDestinationObjectKey';
  NSMigrationEntityMappingKey: NSString; external name '_NSMigrationEntityMappingKey';
  NSMigrationPropertyMappingKey: NSString; external name '_NSMigrationPropertyMappingKey';

{$endif}
{$endif}

{$ifdef FORWARD}
  NSEntityMigrationPolicy = objcclass;
  NSEntityMigrationPolicyPointer = ^NSEntityMigrationPolicy;

{$endif}

{$ifdef CLASSES}
{$ifndef NSENTITYMIGRATIONPOLICY_PAS_C}
{$define NSENTITYMIGRATIONPOLICY_PAS_C}

{ NSEntityMigrationPolicy }
  NSEntityMigrationPolicy = objcclass(NSObject)
    
  public
    class function alloc: NSEntityMigrationPolicy; message 'alloc';

    function beginEntityMapping_manager_error(mapping: NSEntityMapping; manager: NSMigrationManager; error: NSErrorPointer): Boolean; message 'beginEntityMapping:manager:error:';
    function createDestinationInstancesForSourceInstance_entityMapping_manager_error(sInstance: NSManagedObject; mapping: NSEntityMapping; manager: NSMigrationManager; error: NSErrorPointer): Boolean; message 'createDestinationInstancesForSourceInstance:entityMapping:manager:error:';
    function endInstanceCreationForEntityMapping_manager_error(mapping: NSEntityMapping; manager: NSMigrationManager; error: NSErrorPointer): Boolean; message 'endInstanceCreationForEntityMapping:manager:error:';
    function createRelationshipsForDestinationInstance_entityMapping_manager_error(dInstance: NSManagedObject; mapping: NSEntityMapping; manager: NSMigrationManager; error: NSErrorPointer): Boolean; message 'createRelationshipsForDestinationInstance:entityMapping:manager:error:';
    function endRelationshipCreationForEntityMapping_manager_error(mapping: NSEntityMapping; manager: NSMigrationManager; error: NSErrorPointer): Boolean; message 'endRelationshipCreationForEntityMapping:manager:error:';
    function performCustomValidationForEntityMapping_manager_error(mapping: NSEntityMapping; manager: NSMigrationManager; error: NSErrorPointer): Boolean; message 'performCustomValidationForEntityMapping:manager:error:';
    function endEntityMapping_manager_error(mapping: NSEntityMapping; manager: NSMigrationManager; error: NSErrorPointer): Boolean; message 'endEntityMapping:manager:error:';
  end; external;

{$endif}
{$endif}
