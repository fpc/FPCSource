{ Parsed from Coredata.framework NSManagedObjectID.h }
{ Version: 2.1.4 - Sun Jan 2 15:08:57 CET 2011 }


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
  NSManagedObjectIDPtr = NSManagedObjectIDPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSMANAGEDOBJECTID_PAS_C}
{$define NSMANAGEDOBJECTID_PAS_C}

{ NSManagedObjectID }
  NSManagedObjectID = objcclass external (NSObject, NSCopyingProtocol)
    
  public
    function entity: NSEntityDescription; message 'entity';
    function persistentStore: NSPersistentStore; message 'persistentStore';
    function isTemporaryID: Boolean; message 'isTemporaryID';
    function URIRepresentation: NSURL; message 'URIRepresentation';

    { Adopted Protocols }
    function copyWithZone(zone_: NSZonePtr): id;
  end;

{$endif}
{$endif}
