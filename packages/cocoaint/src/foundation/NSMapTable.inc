{ Parsed from Foundation.framework NSMapTable.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:06 ICT 2009 }


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

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSMAPTABLE_PAS_R}
{$define NSMAPTABLE_PAS_R}

{ Records }
type
  NSMapTableKeyCallBacks = record
    hash: function (context: Pointer {bad params!!}): NSUInteger; cdecl;
    isEqual: function (context: Pointer {bad params!!}): Boolean; cdecl;
    retain: procedure (context: Pointer {bad params!!}); cdecl;
    release: procedure (context: Pointer {bad params!!}); cdecl;
    describe: function (context: Pointer {bad params!!}): id; cdecl;
    notAKeyMarker: Pointer;
  end;

type
  NSMapTableValueCallBacks = record
    retain: procedure (context: Pointer {bad params!!}); cdecl;
    release: procedure (context: Pointer {bad params!!}); cdecl;
    describe: function (context: Pointer {bad params!!}): id; cdecl;
  end;


{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSMAPTABLE_PAS_F}
{$define NSMAPTABLE_PAS_F}

{ Functions }
procedure NSFreeMapTable(tablePointer: NSMapTable); cdecl; external name 'NSFreeMapTable';
procedure NSResetMapTable(tablePointer: NSMapTable); cdecl; external name 'NSResetMapTable';
function NSCompareMapTables(table1Pointer: NSMapTable; table2Pointer: NSMapTable): Boolean; cdecl; external name 'NSCompareMapTables';
function NSCopyMapTableWithZone(tablePointer: NSMapTable; zonePointer: NSZone): NSMapTable; cdecl; external name 'NSCopyMapTableWithZone';
function NSMapMember(tablePointer: NSMapTable; keyPointer: Pointer; originalKey: Pointer {Pointer}; value: Pointer {Pointer}): Boolean; cdecl; external name 'NSMapMember';
procedure NSMapGet(tablePointer: NSMapTable; keyPointer: Pointer); cdecl; external name 'NSMapGet';
procedure NSMapInsert(tablePointer: NSMapTable; keyPointer: Pointer; valuePointer: Pointer); cdecl; external name 'NSMapInsert';
procedure NSMapInsertKnownAbsent(tablePointer: NSMapTable; keyPointer: Pointer; valuePointer: Pointer); cdecl; external name 'NSMapInsertKnownAbsent';
procedure NSMapInsertIfAbsent(tablePointer: NSMapTable; keyPointer: Pointer; valuePointer: Pointer); cdecl; external name 'NSMapInsertIfAbsent';
procedure NSMapRemove(tablePointer: NSMapTable; keyPointer: Pointer); cdecl; external name 'NSMapRemove';
function NSEnumerateMapTable(tablePointer: NSMapTable): NSMapEnumerator; cdecl; external name 'NSEnumerateMapTable';
function NSNextMapEnumeratorPair(enumeratorPointer: NSMapEnumerator; key: Pointer {Pointer}; value: Pointer {Pointer}): Boolean; cdecl; external name 'NSNextMapEnumeratorPair';
procedure NSEndMapTableEnumeration(enumeratorPointer: NSMapEnumerator); cdecl; external name 'NSEndMapTableEnumeration';
function NSCountMapTable(tablePointer: NSMapTable): NSUInteger; cdecl; external name 'NSCountMapTable';
function NSStringFromMapTable(tablePointer: NSMapTable): NSString; cdecl; external name 'NSStringFromMapTable';
function NSAllMapTableKeys(tablePointer: NSMapTable): NSArray; cdecl; external name 'NSAllMapTableKeys';
function NSAllMapTableValues(tablePointer: NSMapTable): NSArray; cdecl; external name 'NSAllMapTableValues';
function NSCreateMapTableWithZone(keyCallBacks: NSMapTableKeyCallBacks; valueCallBacks: NSMapTableValueCallBacks; capacity: NSUInteger; zonePointer: NSZone): NSMapTable; cdecl; external name 'NSCreateMapTableWithZone';
function NSCreateMapTable(keyCallBacks: NSMapTableKeyCallBacks; valueCallBacks: NSMapTableValueCallBacks; capacity: NSUInteger): NSMapTable; cdecl; external name 'NSCreateMapTable';

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSMAPTABLE_PAS_S}
{$define NSMAPTABLE_PAS_S}

{ External symbols }
var
  NSIntegerMapKeyCallBacks: NSMapTableKeyCallBacks; external name '_NSIntegerMapKeyCallBacks';
  NSNonOwnedPointerMapKeyCallBacks: NSMapTableKeyCallBacks; external name '_NSNonOwnedPointerMapKeyCallBacks';
  NSNonOwnedPointerOrNullMapKeyCallBacks: NSMapTableKeyCallBacks; external name '_NSNonOwnedPointerOrNullMapKeyCallBacks';
  NSNonRetainedObjectMapKeyCallBacks: NSMapTableKeyCallBacks; external name '_NSNonRetainedObjectMapKeyCallBacks';
  NSObjectMapKeyCallBacks: NSMapTableKeyCallBacks; external name '_NSObjectMapKeyCallBacks';
  NSOwnedPointerMapKeyCallBacks: NSMapTableKeyCallBacks; external name '_NSOwnedPointerMapKeyCallBacks';
  NSIntegerMapValueCallBacks: NSMapTableValueCallBacks; external name '_NSIntegerMapValueCallBacks';
  NSNonOwnedPointerMapValueCallBacks: NSMapTableValueCallBacks; external name '_NSNonOwnedPointerMapValueCallBacks';
  NSObjectMapValueCallBacks: NSMapTableValueCallBacks; external name '_NSObjectMapValueCallBacks';
  NSNonRetainedObjectMapValueCallBacks: NSMapTableValueCallBacks; external name '_NSNonRetainedObjectMapValueCallBacks';
  NSOwnedPointerMapValueCallBacks: NSMapTableValueCallBacks; external name '_NSOwnedPointerMapValueCallBacks';

{$endif}
{$endif}

{$ifdef FORWARD}
  NSMapTable = objcclass;
  NSMapTablePointer = ^NSMapTable;

{$endif}

{$ifdef CLASSES}
{$ifndef NSMAPTABLE_PAS_C}
{$define NSMAPTABLE_PAS_C}

{ NSMapTable }
  NSMapTable = objcclass(NSObject, NSCopyingProtocol, NSCodingProtocol, NSFastEnumerationProtocol)
    
  public
    class function alloc: NSMapTable; message 'alloc';

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
  end; external;

{$endif}
{$endif}
