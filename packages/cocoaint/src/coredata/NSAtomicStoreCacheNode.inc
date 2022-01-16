{ Parsed from CoreData.framework NSAtomicStoreCacheNode.h }


{$ifdef TYPES}
type
  NSAtomicStoreCacheNodePtr = ^NSAtomicStoreCacheNode;
{$endif}

{$ifdef CLASSES}

type
  NSAtomicStoreCacheNode = objcclass external (NSObject)
  private
    _objectID: NSManagedObjectID;
    __versionNumber: culong;
    _propertyCache: NSMutableDictionary;
    _reserved1: pointer;
  public
    function initWithObjectID (moid: NSManagedObjectID): instancetype; message 'initWithObjectID:';
    function objectID: NSManagedObjectID; message 'objectID';
    procedure setPropertyCache(newValue: NSMutableDictionary); message 'setPropertyCache:';
    function propertyCache: NSMutableDictionary; message 'propertyCache';
    function valueForKey (key: NSString): id; message 'valueForKey:';
    procedure setValue_forKey (value: id; key: NSString); message 'setValue:forKey:';
  end;
{$endif}

