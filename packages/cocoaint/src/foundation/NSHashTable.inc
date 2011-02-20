{ Parsed from Foundation.framework NSHashTable.h }

{$ifdef TYPES}
{$ifndef NSHASHTABLE_PAS_T}
{$define NSHASHTABLE_PAS_T}

{ Constants }

const
  NSHashTableStrongMemory = 0;
  NSHashTableZeroingWeakMemory = NSPointerFunctionsZeroingWeakMemory;
  NSHashTableCopyIn = NSPointerFunctionsCopyIn;
  NSHashTableObjectPointerPersonality = NSPointerFunctionsObjectPointerPersonality;

{ Types }
type
  NSHashTableOptions = NSUInteger;
  NSHashTableOptionsPtr = ^NSHashTableOptions;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSHASHTABLE_PAS_R}
{$define NSHASHTABLE_PAS_R}

{ Records }
type
  
NSHashEnumerator = record
    _pi: NSUInteger;
    _si: NSUInteger;
    _bs: Pointer;
  end;
NSHashEnumeratorPtr = ^NSHashEnumerator;

type
  NSHashTableCallBacks = record
    hash: function (table: NSHashTable; param1: Pointer): NSUInteger; cdecl;
    isEqual: function (table: NSHashTable; param1: Pointer; param2: Pointer): Boolean; cdecl;
    retain: procedure (table: NSHashTable; param1: Pointer); cdecl;
    release: procedure (table: NSHashTable; param1: Pointer); cdecl;
    describe: function (table: NSHashTable; param1: Pointer): NSString; cdecl;
  end;
NSHashTableCallBacksPtr = ^NSHashTableCallBacks;


{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSHASHTABLE_PAS_F}
{$define NSHASHTABLE_PAS_F}

{ Functions }
procedure NSFreeHashTable(table: NSHashTable); cdecl; external;
procedure NSResetHashTable(table: NSHashTable); cdecl; external;
function NSCompareHashTables(table1: NSHashTable; table2: NSHashTable): Boolean; cdecl; external;
function NSCopyHashTableWithZone(table: NSHashTable; zone: NSZonePtr): NSHashTable; cdecl; external;
function NSHashGet(table: NSHashTable; pointer_: Pointer): Pointer; cdecl; external;
procedure NSHashInsert(table: NSHashTable; pointer_: Pointer); cdecl; external;
procedure NSHashInsertKnownAbsent(table: NSHashTable; pointer_: Pointer); cdecl; external;
function NSHashInsertIfAbsent(table: NSHashTable; pointer_: Pointer): Pointer; cdecl; external;
procedure NSHashRemove(table: NSHashTable; pointer_: Pointer); cdecl; external;
function NSEnumerateHashTable(table: NSHashTable): NSHashEnumerator; cdecl; external;
function NSNextHashEnumeratorItem(enumerator: NSHashEnumeratorPtr): Pointer; cdecl; external;
procedure NSEndHashTableEnumeration(enumerator: NSHashEnumeratorPtr); cdecl; external;
function NSCountHashTable(table: NSHashTable): NSUInteger; cdecl; external;
function NSStringFromHashTable(table: NSHashTable): NSString; cdecl; external;
function NSAllHashTableObjects(table: NSHashTable): NSArray; cdecl; external;
function NSCreateHashTableWithZone(callBacks: NSHashTableCallBacks; capacity: NSUInteger; zone: NSZonePtr): NSHashTable; cdecl; external;
function NSCreateHashTable(callBacks: NSHashTableCallBacks; capacity: NSUInteger): NSHashTable; cdecl; external;

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSHASHTABLE_PAS_S}
{$define NSHASHTABLE_PAS_S}

{ External symbols }
var
  NSIntegerHashCallBacks: NSHashTableCallBacks; cvar; external;
  NSNonOwnedPointerHashCallBacks: NSHashTableCallBacks; cvar; external;
  NSNonRetainedObjectHashCallBacks: NSHashTableCallBacks; cvar; external;
  NSObjectHashCallBacks: NSHashTableCallBacks; cvar; external;
  NSOwnedObjectIdentityHashCallBacks: NSHashTableCallBacks; cvar; external;
  NSOwnedPointerHashCallBacks: NSHashTableCallBacks; cvar; external;
  NSPointerToStructHashCallBacks: NSHashTableCallBacks; cvar; external;
  NSIntHashCallBacks: NSHashTableCallBacks deprecated 'in Mac OS X 10.5 and later'; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  NSHashTable = objcclass;
  NSHashTablePointer = ^NSHashTable;
  NSHashTablePtr = NSHashTablePointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSHASHTABLE_PAS_C}
{$define NSHASHTABLE_PAS_C}

{ NSHashTable }
  NSHashTable = objcclass external (NSObject, NSCopyingProtocol, NSCodingProtocol, NSFastEnumerationProtocol)
    
  public
    function initWithOptions_capacity(options: NSPointerFunctionsOptions; initialCapacity: NSUInteger): id; message 'initWithOptions:capacity:';
    function initWithPointerFunctions_capacity(functions: NSPointerFunctions; initialCapacity: NSUInteger): id; message 'initWithPointerFunctions:capacity:';
    class function hashTableWithOptions(options: NSPointerFunctionsOptions): id; message 'hashTableWithOptions:';
    class function hashTableWithWeakObjects: id; message 'hashTableWithWeakObjects';
    function pointerFunctions: NSPointerFunctions; message 'pointerFunctions';
    function count: NSUInteger; message 'count';
    function member(object_: id): id; message 'member:';
    function objectEnumerator: NSEnumerator; message 'objectEnumerator';
    procedure addObject(object_: id); message 'addObject:';
    procedure removeObject(object_: id); message 'removeObject:';
    procedure removeAllObjects; message 'removeAllObjects';
    function allObjects: NSArray; message 'allObjects';
    function anyObject: id; message 'anyObject';
    function containsObject(anObject: id): Boolean; message 'containsObject:';
    function intersectsHashTable(other: NSHashTable): Boolean; message 'intersectsHashTable:';
    function isEqualToHashTable(other: NSHashTable): Boolean; message 'isEqualToHashTable:';
    function isSubsetOfHashTable(other: NSHashTable): Boolean; message 'isSubsetOfHashTable:';
    procedure intersectHashTable(other: NSHashTable); message 'intersectHashTable:';
    procedure unionHashTable(other: NSHashTable); message 'unionHashTable:';
    procedure minusHashTable(other: NSHashTable); message 'minusHashTable:';
    function setRepresentation: NSSet; message 'setRepresentation';

    { Adopted Protocols }
    function copyWithZone(zone_: NSZonePtr): id;
    procedure encodeWithCoder(aCoder: NSCoder);
    function initWithCoder(aDecoder: NSCoder): id;
    function countByEnumeratingWithState_objects_count(state: NSFastEnumerationStatePtr; stackbuf: idPtr; len: NSUInteger): NSUInteger;
  end;

{$endif}
{$endif}
