{ Parsed from Foundation.framework NSMapTable.h }


{ Types from NSMapTable }
{$ifdef TYPES}

{$endif}


{$ifdef TYPES}
type
  NSMapTablePtr = ^NSMapTable;
{$endif}

{$if  not (defined(__FOUNDATION_NSMAPTABLE__))}
{$ifdef TYPES}
const
  __FOUNDATION_NSMAPTABLE__ = 1;

const
  NSMapTableStrongMemory = 0 { available in 10_5, 6_0 };
  {$if (defined(TARGET_OS_MAC) and not (defined(TARGET_OS_EMBEDDED) or defined(TARGET_OS_IPHONE))) or defined(TARGET_OS_WIN32)}
  NSMapTableZeroingWeakMemory = NSPointerFunctionsZeroingWeakMemory deprecated 'in 10_5, 10_8';
  {$endif}
  NSMapTableCopyIn = NSPointerFunctionsCopyIn { available in 10_5, 6_0 };
  NSMapTableObjectPointerPersonality = NSPointerFunctionsObjectPointerPersonality { available in 10_5, 6_0 };
  NSMapTableWeakMemory = NSPointerFunctionsWeakMemory { available in 10_8, 6_0 };

type
  NSMapTableOptions = NSUInteger;
  NSMapTableOptionsPtr = ^NSMapTableOptions;
{$endif}

{$ifdef CLASSES}

type
  NSMapTable = objcclass external (NSObject, NSCopyingProtocol, NSCodingProtocol, NSFastEnumerationProtocol)
  public
    function initWithKeyOptions_valueOptions_capacity (keyOptions: NSPointerFunctionsOptions; valueOptions: NSPointerFunctionsOptions; initialCapacity: NSUInteger): instancetype; message 'initWithKeyOptions:valueOptions:capacity:'; { NS_DESIGNATED_INITIALIZER }
    function initWithKeyPointerFunctions_valuePointerFunctions_capacity (keyFunctions: NSPointerFunctions; valueFunctions: NSPointerFunctions; initialCapacity: NSUInteger): instancetype; message 'initWithKeyPointerFunctions:valuePointerFunctions:capacity:'; { NS_DESIGNATED_INITIALIZER }
    class function mapTableWithKeyOptions_valueOptions (keyOptions: NSPointerFunctionsOptions; valueOptions: NSPointerFunctionsOptions): NSMapTable; message 'mapTableWithKeyOptions:valueOptions:';
    {$if (defined(TARGET_OS_MAC) and not (defined(TARGET_OS_EMBEDDED) or defined(TARGET_OS_IPHONE))) or defined(TARGET_OS_WIN32)}
    class function mapTableWithStrongToStrongObjects: id; message 'mapTableWithStrongToStrongObjects'; deprecated 'in 10_5, 10_8';
    class function mapTableWithWeakToStrongObjects: id; message 'mapTableWithWeakToStrongObjects'; deprecated 'in 10_5, 10_8';
    class function mapTableWithStrongToWeakObjects: id; message 'mapTableWithStrongToWeakObjects'; deprecated 'in 10_5, 10_8';
    class function mapTableWithWeakToWeakObjects: id; message 'mapTableWithWeakToWeakObjects'; deprecated 'in 10_5, 10_8';
    {$endif}
    class function strongToStrongObjectsMapTable: NSMapTable; message 'strongToStrongObjectsMapTable'; { available in 10_8, 6_0 }
    class function weakToStrongObjectsMapTable: NSMapTable; message 'weakToStrongObjectsMapTable'; { available in 10_8, 6_0 }
    class function strongToWeakObjectsMapTable: NSMapTable; message 'strongToWeakObjectsMapTable'; { available in 10_8, 6_0 }
    class function weakToWeakObjectsMapTable: NSMapTable; message 'weakToWeakObjectsMapTable'; { available in 10_8, 6_0 }
    function keyPointerFunctions: NSPointerFunctions; message 'keyPointerFunctions';
    function valuePointerFunctions: NSPointerFunctions; message 'valuePointerFunctions';
    function objectForKey (aKey: id): id; message 'objectForKey:';
    procedure removeObjectForKey (aKey: id); message 'removeObjectForKey:';
    procedure setObject_forKey (anObject: id; aKey: id); message 'setObject:forKey:';
    function count: NSUInteger; message 'count';
    function keyEnumerator: NSEnumerator; message 'keyEnumerator';
    function objectEnumerator: NSEnumerator; message 'objectEnumerator';
    procedure removeAllObjects; message 'removeAllObjects';
    function dictionaryRepresentation: NSDictionary; message 'dictionaryRepresentation';

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
  NSMapEnumerator = record
    _pi: NSUInteger;
    _si: NSUInteger;
    _bs: pointer;
  end;
type
  NSMapEnumeratorPtr = ^NSMapEnumerator;
{$endif}

{$ifdef FUNCTIONS}
procedure NSFreeMapTable(table: NSMapTable); cdecl; external;
procedure NSResetMapTable(table: NSMapTable); cdecl; external;
function NSCompareMapTables(table1: NSMapTable; table2: NSMapTable): ObjCBOOL; cdecl; external;
function NSCopyMapTableWithZone(table: NSMapTable; zone: NSZonePtr): NSMapTable; cdecl; external;
function NSMapMember(table: NSMapTable; key: pointer; originalKey: pointer; value: pointer): ObjCBOOL; cdecl; external;
function NSMapGet(table: NSMapTable; key: pointer): pointer; cdecl; external;
procedure NSMapInsert(table: NSMapTable; key: pointer; value: pointer); cdecl; external;
procedure NSMapInsertKnownAbsent(table: NSMapTable; key: pointer; value: pointer); cdecl; external;
function NSMapInsertIfAbsent(table: NSMapTable; key: pointer; value: pointer): pointer; cdecl; external;
procedure NSMapRemove(table: NSMapTable; key: pointer); cdecl; external;
function NSEnumerateMapTable(table: NSMapTable): NSMapEnumerator; cdecl; external;
function NSNextMapEnumeratorPair(enumerator: NSMapEnumeratorPtr; key: pointer; value: pointer): ObjCBOOL; cdecl; external;
procedure NSEndMapTableEnumeration(enumerator: NSMapEnumeratorPtr); cdecl; external;
function NSCountMapTable(table: NSMapTable): NSUInteger; cdecl; external;
function NSStringFromMapTable(table: NSMapTable): NSString; cdecl; external;
function NSAllMapTableKeys(table: NSMapTable): NSArray; cdecl; external;
function NSAllMapTableValues(table: NSMapTable): NSArray; cdecl; external;
{$endif}

{$ifdef TYPES}
type
  NSMapTableKeyCallBacks = record
    hash: function (table: NSMapTable; param2: pointer): NSUInteger; cdecl;
    isEqual: function (table: NSMapTable; param2: pointer; param3: pointer): ObjCBOOL; cdecl;
    retain: procedure (table: NSMapTable; param2: pointer); cdecl;
    release: procedure (table: NSMapTable; param2: pointer); cdecl;
    describe: function (table: NSMapTable; param2: pointer): NSString; cdecl;
    notAKeyMarker: pointer;
  end;
type
  NSMapTableKeyCallBacksPtr = ^NSMapTableKeyCallBacks;
type
  NSMapTableValueCallBacks = record
    retain: procedure (table: NSMapTable; param2: pointer); cdecl;
    release: procedure (table: NSMapTable; param2: pointer); cdecl;
    describe: function (table: NSMapTable; param2: pointer): NSString; cdecl;
  end;
type
  NSMapTableValueCallBacksPtr = ^NSMapTableValueCallBacks;
{$endif}

{$ifdef FUNCTIONS}
function NSCreateMapTableWithZone(keyCallBacks: NSMapTableKeyCallBacks; valueCallBacks: NSMapTableValueCallBacks; capacity: NSUInteger; zone: NSZonePtr): NSMapTable; cdecl; external;
function NSCreateMapTable(keyCallBacks: NSMapTableKeyCallBacks; valueCallBacks: NSMapTableValueCallBacks; capacity: NSUInteger): NSMapTable; cdecl; external;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSIntegerMapKeyCallBacks: NSMapTableKeyCallBacks { available in 10_5 }; cvar; external;
  NSNonOwnedPointerMapKeyCallBacks: NSMapTableKeyCallBacks; cvar; external;
  NSNonOwnedPointerOrNullMapKeyCallBacks: NSMapTableKeyCallBacks; cvar; external;
  NSNonRetainedObjectMapKeyCallBacks: NSMapTableKeyCallBacks; cvar; external;
  NSObjectMapKeyCallBacks: NSMapTableKeyCallBacks; cvar; external;
  NSOwnedPointerMapKeyCallBacks: NSMapTableKeyCallBacks; cvar; external;
  NSIntMapKeyCallBacks: NSMapTableKeyCallBacks deprecated 'in 10_0, 10_5'; cvar; external;
  NSIntegerMapValueCallBacks: NSMapTableValueCallBacks { available in 10_5 }; cvar; external;
  NSNonOwnedPointerMapValueCallBacks: NSMapTableValueCallBacks; cvar; external;
  NSObjectMapValueCallBacks: NSMapTableValueCallBacks; cvar; external;
  NSNonRetainedObjectMapValueCallBacks: NSMapTableValueCallBacks; cvar; external;
  NSOwnedPointerMapValueCallBacks: NSMapTableValueCallBacks; cvar; external;
  NSIntMapValueCallBacks: NSMapTableValueCallBacks deprecated 'in 10_0, 10_5'; cvar; external;
{$endif}

{$else}

{$endif}
{$endif}
