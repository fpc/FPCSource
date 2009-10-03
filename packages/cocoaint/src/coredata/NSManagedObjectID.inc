{ Parsed from Coredata.framework NSManagedObjectID.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Fri Oct 2 11:46:33 EDT 2009 }


{$ifdef TYPES}
{$ifndef NSMANAGEDOBJECTID_PAS_T}
{$define NSMANAGEDOBJECTID_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSMANAGEDOBJECTID_PAS_R}
{$define NSMANAGEDOBJECTID_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSMANAGEDOBJECTID_PAS_F}
{$define NSMANAGEDOBJECTID_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSMANAGEDOBJECTID_PAS_S}
{$define NSMANAGEDOBJECTID_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSManagedObjectID = objcclass;
  NSManagedObjectIDPointer = ^NSManagedObjectID;

{$endif}

{$ifdef CLASSES}
{$ifndef NSMANAGEDOBJECTID_PAS_C}
{$define NSMANAGEDOBJECTID_PAS_C}

{ NSManagedObjectID }
  NSManagedObjectID = objcclass(NSObject, NSCopyingProtocol)
    
  public
    class function alloc: NSManagedObjectID; message 'alloc';

    function entity: NSEntityDescription; message 'entity';
    function persistentStore: NSPersistentStore; message 'persistentStore';
    function isTemporaryID: Boolean; message 'isTemporaryID';
    function URIRepresentation: NSURL; message 'URIRepresentation';
  end; external;

{$endif}
{$endif}
