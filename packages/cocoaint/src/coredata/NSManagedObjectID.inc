{ Parsed from CoreData.framework NSManagedObjectID.h }


{$ifdef TYPES}
type
  NSManagedObjectIDPtr = ^NSManagedObjectID;
{$endif}

{$ifdef CLASSES}

type
  NSManagedObjectID = objcclass external (NSObject, NSCopyingProtocol)
  private
  public
    function entity: NSEntityDescription; message 'entity';
    function persistentStore: NSPersistentStore; message 'persistentStore';
    function isTemporaryID: ObjCBOOL; message 'isTemporaryID';
    function URIRepresentation: NSURL; message 'URIRepresentation';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
  end;
{$endif}

