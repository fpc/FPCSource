{ Parsed from Foundation.framework NSHashTable.h }


{ Types from NSHashTable }
{$ifdef TYPES}

{$endif}


{$ifdef TYPES}
type
  NSHashTablePtr = ^NSHashTable;
{$endif}

{$if  not (defined(__FOUNDATION_NSHASHTABLE__))}
{$ifdef TYPES}
const
  __FOUNDATION_NSHASHTABLE__ = 1;

const
  NSHashTableStrongMemory = 0 { available in 10_5, 6_0 };
  {$if (defined(TARGET_OS_MAC) and not (defined(TARGET_OS_EMBEDDED) or defined(TARGET_OS_IPHONE))) or defined(TARGET_OS_WIN32)}
  NSHashTableZeroingWeakMemory = NSPointerFunctionsZeroingWeakMemory deprecated 'in 10_5, 10_8';
  {$endif}
  NSHashTableCopyIn = NSPointerFunctionsCopyIn { available in 10_5, 6_0 };
  NSHashTableObjectPointerPersonality = NSPointerFunctionsObjectPointerPersonality { available in 10_5, 6_0 };
  NSHashTableWeakMemory = NSPointerFunctionsWeakMemory { available in 10_8, 6_0 };

type
  NSHashTableOptions = NSUInteger;
  NSHashTableOptionsPtr = ^NSHashTableOptions;
{$endif}

{$ifdef CLASSES}

type
  NSHashTable = objcclass external (NSObject, NSCopyingProtocol, NSCodingProtocol, NSFastEnumerationProtocol)
  public
    function initWithOptions_capacity (options: NSPointerFunctionsOptions; initialCapacity: NSUInteger): instancetype; message 'initWithOptions:capacity:'; { NS_DESIGNATED_INITIALIZER }
    function initWithPointerFunctions_capacity (functions: NSPointerFunctions; initialCapacity: NSUInteger): instancetype; message 'initWithPointerFunctions:capacity:'; { NS_DESIGNATED_INITIALIZER }
    class function hashTableWithOptions (options: NSPointerFunctionsOptions): NSHashTable; message 'hashTableWithOptions:';
    {$if (defined(TARGET_OS_MAC) and not (defined(TARGET_OS_EMBEDDED) or defined(TARGET_OS_IPHONE))) or defined(TARGET_OS_WIN32)}
    class function hashTableWithWeakObjects: id; message 'hashTableWithWeakObjects'; deprecated 'in 10_5, 10_8';
    {$endif}
    class function weakObjectsHashTable: NSHashTable; message 'weakObjectsHashTable'; { available in 10_8, 6_0 }
    function pointerFunctions: NSPointerFunctions; message 'pointerFunctions';
    function count: NSUInteger; message 'count';
    function member (object_: id): id; message 'member:';
    function objectEnumerator: NSEnumerator; message 'objectEnumerator';
    procedure addObject (object_: id); message 'addObject:';
    procedure removeObject (object_: id); message 'removeObject:';
    procedure removeAllObjects; message 'removeAllObjects';
    function allObjects: NSArray; message 'allObjects';
    function anyObject: id; message 'anyObject';
    function containsObject (anObject: id): ObjCBOOL; message 'containsObject:';
    function intersectsHashTable (other: NSHashTable): ObjCBOOL; message 'intersectsHashTable:';
    function isEqualToHashTable (other: NSHashTable): ObjCBOOL; message 'isEqualToHashTable:';
    function isSubsetOfHashTable (other: NSHashTable): ObjCBOOL; message 'isSubsetOfHashTable:';
    procedure intersectHashTable (other: NSHashTable); message 'intersectHashTable:';
    procedure unionHashTable (other: NSHashTable); message 'unionHashTable:';
    procedure minusHashTable (other: NSHashTable); message 'minusHashTable:';
    function setRepresentation: NSSet; message 'setRepresentation';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    function countByEnumeratingWithState_objects_count (state: NSFastEnumerationStatePtr; buffer: idPtr { variable size array of id }; len: NSUInteger): NSUInteger; message 'countByEnumeratingWithState:objects:count:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
  end;
{$endif}

{$if (defined(TARGET_OS_MAC) and not (defined(TARGET_OS_EMBEDDED) or defined(TARGET_OS_IPHONE))) or defined(TARGET_OS_WIN32)}
{$ifdef TYPES}
type
  NSHashEnumerator = record
    _pi: NSUInteger;
    _si: NSUInteger;
    _bs: pointer;
  end;
type
  NSHashEnumeratorPtr = ^NSHashEnumerator;
{$endif}

{$ifdef FUNCTIONS}
procedure NSFreeHashTable(table: NSHashTable); cdecl; external;
procedure NSResetHashTable(table: NSHashTable); cdecl; external;
function NSCompareHashTables(table1: NSHashTable; table2: NSHashTable): ObjCBOOL; cdecl; external;
function NSCopyHashTableWithZone(table: NSHashTable; zone: NSZonePtr): NSHashTable; cdecl; external;
function NSHashGet(table: NSHashTable; pointer_: pointer): pointer; cdecl; external;
procedure NSHashInsert(table: NSHashTable; pointer_: pointer); cdecl; external;
procedure NSHashInsertKnownAbsent(table: NSHashTable; pointer_: pointer); cdecl; external;
function NSHashInsertIfAbsent(table: NSHashTable; pointer_: pointer): pointer; cdecl; external;
procedure NSHashRemove(table: NSHashTable; pointer_: pointer); cdecl; external;
function NSEnumerateHashTable(table: NSHashTable): NSHashEnumerator; cdecl; external;
function NSNextHashEnumeratorItem(enumerator: NSHashEnumeratorPtr): pointer; cdecl; external;
procedure NSEndHashTableEnumeration(enumerator: NSHashEnumeratorPtr); cdecl; external;
function NSCountHashTable(table: NSHashTable): NSUInteger; cdecl; external;
function NSStringFromHashTable(table: NSHashTable): NSString; cdecl; external;
function NSAllHashTableObjects(table: NSHashTable): NSArray; cdecl; external;
{$endif}

{$ifdef TYPES}
type
  NSHashTableCallBacks = record
    hash: function (table: NSHashTable; param2: pointer): NSUInteger; cdecl;
    isEqual: function (table: NSHashTable; param2: pointer; param3: pointer): ObjCBOOL; cdecl;
    retain: procedure (table: NSHashTable; param2: pointer); cdecl;
    release: procedure (table: NSHashTable; param2: pointer); cdecl;
    describe: function (table: NSHashTable; param2: pointer): NSString; cdecl;
  end;
type
  NSHashTableCallBacksPtr = ^NSHashTableCallBacks;
{$endif}

{$ifdef FUNCTIONS}
function NSCreateHashTableWithZone(callBacks: NSHashTableCallBacks; capacity: NSUInteger; zone: NSZonePtr): NSHashTable; cdecl; external;
function NSCreateHashTable(callBacks: NSHashTableCallBacks; capacity: NSUInteger): NSHashTable; cdecl; external;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSIntegerHashCallBacks: NSHashTableCallBacks { available in 10_5 }; cvar; external;
  NSNonOwnedPointerHashCallBacks: NSHashTableCallBacks; cvar; external;
  NSNonRetainedObjectHashCallBacks: NSHashTableCallBacks; cvar; external;
  NSObjectHashCallBacks: NSHashTableCallBacks; cvar; external;
  NSOwnedObjectIdentityHashCallBacks: NSHashTableCallBacks; cvar; external;
  NSOwnedPointerHashCallBacks: NSHashTableCallBacks; cvar; external;
  NSPointerToStructHashCallBacks: NSHashTableCallBacks; cvar; external;
  NSIntHashCallBacks: NSHashTableCallBacks deprecated 'in 10_0, 10_5'; cvar; external;
{$endif}

{$else}

{$endif}
{$endif}
