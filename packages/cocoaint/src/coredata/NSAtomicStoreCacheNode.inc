{ Parsed from Coredata.framework NSAtomicStoreCacheNode.h }

{$ifdef TYPES}
{$ifndef NSATOMICSTORECACHENODE_PAS_T}
{$define NSATOMICSTORECACHENODE_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSATOMICSTORECACHENODE_PAS_R}
{$define NSATOMICSTORECACHENODE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSATOMICSTORECACHENODE_PAS_F}
{$define NSATOMICSTORECACHENODE_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSATOMICSTORECACHENODE_PAS_S}
{$define NSATOMICSTORECACHENODE_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSAtomicStoreCacheNode = objcclass;
  NSAtomicStoreCacheNodePointer = ^NSAtomicStoreCacheNode;
  NSAtomicStoreCacheNodePtr = NSAtomicStoreCacheNodePointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSATOMICSTORECACHENODE_PAS_C}
{$define NSATOMICSTORECACHENODE_PAS_C}

{ NSAtomicStoreCacheNode }
  NSAtomicStoreCacheNode = objcclass external (NSObject)
  private
    _objectID: NSManagedObjectID;
    __versionNumber: culong;
    _propertyCache: NSMutableDictionary;
    _reserved1: Pointer;
    
  public
    function initWithObjectID(moid: NSManagedObjectID): id; message 'initWithObjectID:';
    function objectID: NSManagedObjectID; message 'objectID';
    function propertyCache: NSMutableDictionary; message 'propertyCache';
    procedure setPropertyCache(propertyCache_: NSMutableDictionary); message 'setPropertyCache:';
    function valueForKey(key: NSString): id; message 'valueForKey:';
    procedure setValue_forKey(value: id; key: NSString); message 'setValue:forKey:';
  end;

{$endif}
{$endif}
