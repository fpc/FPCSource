{ Parsed from Coredata.framework NSAtomicStoreCacheNode.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Fri Oct 2 11:46:33 EDT 2009 }


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

{$endif}

{$ifdef CLASSES}
{$ifndef NSATOMICSTORECACHENODE_PAS_C}
{$define NSATOMICSTORECACHENODE_PAS_C}

{ NSAtomicStoreCacheNode }
  NSAtomicStoreCacheNode = objcclass(NSObject)
  private
    __objectID: NSManagedObjectID;
    ___versionNumber: uintptr_t;
    __propertyCache: NSMutableDictionary;
    __reserved1: Pointer;
    
  public
    class function alloc: NSAtomicStoreCacheNode; message 'alloc';

    function initWithObjectID(moid: NSManagedObjectID): id; message 'initWithObjectID:';
    function objectID: NSManagedObjectID; message 'objectID';
    function propertyCache: NSMutableDictionary; message 'propertyCache';
    procedure setPropertyCache(propertyCache_: NSMutableDictionary); message 'setPropertyCache:';
    function valueForKey(key: NSString): id; message 'valueForKey:';
    procedure setValue_forKey(value: id; key: NSString); message 'setValue:forKey:';
  end; external;

{$endif}
{$endif}
