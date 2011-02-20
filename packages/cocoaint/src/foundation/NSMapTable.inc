{ Parsed from Foundation.framework NSMapTable.h }

{$ifdef TYPES}
{$ifndef NSMAPTABLE_PAS_T}
{$define NSMAPTABLE_PAS_T}

{ Constants }

const
  NSMapTableStrongMemory = 0;
  NSMapTableZeroingWeakMemory = NSPointerFunctionsZeroingWeakMemory;
  NSMapTableCopyIn = NSPointerFunctionsCopyIn;
  NSMapTableObjectPointerPersonality = NSPointerFunctionsObjectPointerPersonality;

{ Types }
type
  NSMapTableOptions = NSUInteger;
  NSMapTableOptionsPtr = ^NSMapTableOptions;

{ Defines }
const
  NSNotAnIntMapKey = NSIntegerMin;
  NSNotAnIntegerMapKey = NSIntegerMin;
  NSNotAPointerMapKey = UINTPTR_MAX;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSMAPTABLE_PAS_R}
{$define NSMAPTABLE_PAS_R}

{ Records }
type
  
NSMapEnumerator = record
    _pi: NSUInteger;
    _si: NSUInteger;
    _bs: Pointer;
  end;
NSMapEnumeratorPtr = ^NSMapEnumerator;

type
  NSMapTableKeyCallBacks = record
    hash: function (table: NSMapTable; param1: Pointer): NSUInteger; cdecl;
    isEqual: function (table: NSMapTable; param1: Pointer; param2: Pointer): Boolean; cdecl;
    retain: procedure (table: NSMapTable; param1: Pointer); cdecl;
    release: procedure (table: NSMapTable; param1: Pointer); cdecl;
    describe: function (table: NSMapTable; param1: Pointer): NSString; cdecl;
    notAKeyMarker: Pointer;
  end;
NSMapTableKeyCallBacksPtr = ^NSMapTableKeyCallBacks;

type
  NSMapTableValueCallBacks = record
    retain: procedure (table: NSMapTable; param1: Pointer); cdecl;
    release: procedure (table: NSMapTable; param1: Pointer); cdecl;
    describe: function (table: NSMapTable; param1: Pointer): NSString; cdecl;
  end;
NSMapTableValueCallBacksPtr = ^NSMapTableValueCallBacks;


{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSMAPTABLE_PAS_F}
{$define NSMAPTABLE_PAS_F}

{ Functions }
procedure NSFreeMapTable(table: NSMapTable); cdecl; external;
procedure NSResetMapTable(table: NSMapTable); cdecl; external;
function NSCompareMapTables(table1: NSMapTable; table2: NSMapTable): Boolean; cdecl; external;
function NSCopyMapTableWithZone(table: NSMapTable; zone: NSZonePtr): NSMapTable; cdecl; external;
function NSMapMember(table: NSMapTable; key: Pointer; originalKey: PPointer; value: PPointer): Boolean; cdecl; external;
function NSMapGet(table: NSMapTable; key: Pointer): Pointer; cdecl; external;
procedure NSMapInsert(table: NSMapTable; key: Pointer; value: Pointer); cdecl; external;
procedure NSMapInsertKnownAbsent(table: NSMapTable; key: Pointer; value: Pointer); cdecl; external;
function NSMapInsertIfAbsent(table: NSMapTable; key: Pointer; value: Pointer): Pointer; cdecl; external;
procedure NSMapRemove(table: NSMapTable; key: Pointer); cdecl; external;
function NSEnumerateMapTable(table: NSMapTable): NSMapEnumerator; cdecl; external;
function NSNextMapEnumeratorPair(enumerator: NSMapEnumeratorPtr; key: PPointer; value: PPointer): Boolean; cdecl; external;
procedure NSEndMapTableEnumeration(enumerator: NSMapEnumeratorPtr); cdecl; external;
function NSCountMapTable(table: NSMapTable): NSUInteger; cdecl; external;
function NSStringFromMapTable(table: NSMapTable): NSString; cdecl; external;
function NSAllMapTableKeys(table: NSMapTable): NSArray; cdecl; external;
function NSAllMapTableValues(table: NSMapTable): NSArray; cdecl; external;
function NSCreateMapTableWithZone(keyCallBacks: NSMapTableKeyCallBacks; valueCallBacks: NSMapTableValueCallBacks; capacity: NSUInteger; zone: NSZonePtr): NSMapTable; cdecl; external;
function NSCreateMapTable(keyCallBacks: NSMapTableKeyCallBacks; valueCallBacks: NSMapTableValueCallBacks; capacity: NSUInteger): NSMapTable; cdecl; external;

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSMAPTABLE_PAS_S}
{$define NSMAPTABLE_PAS_S}

{ External symbols }
var
  NSIntegerMapKeyCallBacks: NSMapTableKeyCallBacks; cvar; external;
  NSNonOwnedPointerMapKeyCallBacks: NSMapTableKeyCallBacks; cvar; external;
  NSNonOwnedPointerOrNullMapKeyCallBacks: NSMapTableKeyCallBacks; cvar; external;
  NSNonRetainedObjectMapKeyCallBacks: NSMapTableKeyCallBacks; cvar; external;
  NSObjectMapKeyCallBacks: NSMapTableKeyCallBacks; cvar; external;
  NSOwnedPointerMapKeyCallBacks: NSMapTableKeyCallBacks; cvar; external;
  NSIntMapKeyCallBacks: NSMapTableKeyCallBacks deprecated 'in Mac OS X 10.5 and later'; cvar; external;
  NSIntegerMapValueCallBacks: NSMapTableValueCallBacks; cvar; external;
  NSNonOwnedPointerMapValueCallBacks: NSMapTableValueCallBacks; cvar; external;
  NSObjectMapValueCallBacks: NSMapTableValueCallBacks; cvar; external;
  NSNonRetainedObjectMapValueCallBacks: NSMapTableValueCallBacks; cvar; external;
  NSOwnedPointerMapValueCallBacks: NSMapTableValueCallBacks; cvar; external;
  NSIntMapValueCallBacks: NSMapTableValueCallBacks deprecated 'in Mac OS X 10.5 and later'; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  NSMapTable = objcclass;
  NSMapTablePointer = ^NSMapTable;
  NSMapTablePtr = NSMapTablePointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSMAPTABLE_PAS_C}
{$define NSMAPTABLE_PAS_C}

{ NSMapTable }
  NSMapTable = objcclass external (NSObject, NSCopyingProtocol, NSCodingProtocol, NSFastEnumerationProtocol)
    
  public
    function initWithKeyOptions_valueOptions_capacity(keyOptions: NSPointerFunctionsOptions; valueOptions: NSPointerFunctionsOptions; initialCapacity: NSUInteger): id; message 'initWithKeyOptions:valueOptions:capacity:';
    function initWithKeyPointerFunctions_valuePointerFunctions_capacity(keyFunctions: NSPointerFunctions; valueFunctions: NSPointerFunctions; initialCapacity: NSUInteger): id; message 'initWithKeyPointerFunctions:valuePointerFunctions:capacity:';
    class function mapTableWithKeyOptions_valueOptions(keyOptions: NSPointerFunctionsOptions; valueOptions: NSPointerFunctionsOptions): id; message 'mapTableWithKeyOptions:valueOptions:';
    class function mapTableWithStrongToStrongObjects: id; message 'mapTableWithStrongToStrongObjects';
    class function mapTableWithWeakToStrongObjects: id; message 'mapTableWithWeakToStrongObjects';
    class function mapTableWithStrongToWeakObjects: id; message 'mapTableWithStrongToWeakObjects';
    class function mapTableWithWeakToWeakObjects: id; message 'mapTableWithWeakToWeakObjects';
    function keyPointerFunctions: NSPointerFunctions; message 'keyPointerFunctions';
    function valuePointerFunctions: NSPointerFunctions; message 'valuePointerFunctions';
    function objectForKey(aKey: id): id; message 'objectForKey:';
    procedure removeObjectForKey(aKey: id); message 'removeObjectForKey:';
    procedure setObject_forKey(anObject: id; aKey: id); message 'setObject:forKey:';
    function count: NSUInteger; message 'count';
    function keyEnumerator: NSEnumerator; message 'keyEnumerator';
    function objectEnumerator: NSEnumerator; message 'objectEnumerator';
    procedure removeAllObjects; message 'removeAllObjects';
    function dictionaryRepresentation: NSDictionary; message 'dictionaryRepresentation';

    { Adopted Protocols }
    function copyWithZone(zone_: NSZonePtr): id;
    procedure encodeWithCoder(aCoder: NSCoder);
    function initWithCoder(aDecoder: NSCoder): id;
    function countByEnumeratingWithState_objects_count(state: NSFastEnumerationStatePtr; stackbuf: idPtr; len: NSUInteger): NSUInteger;
  end;

{$endif}
{$endif}
