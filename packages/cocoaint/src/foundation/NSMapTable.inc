{ Parsed from Foundation.framework NSMapTable.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:00 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSMAPTABLE_PAS_H}
{$define NSMAPTABLE_PAS_H}
type
  NSMapTablePointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSMAPTABLE_PAS_T}
{$define NSMAPTABLE_PAS_T}

{ Constants }

const
  NSMapTableZeroingWeakMemory = NSPointerFunctionsZeroingWeakMemory;
  NSMapTableCopyIn = NSPointerFunctionsCopyIn;
  NSMapTableObjectPointerPersonality = NSPointerFunctionsObjectPointerPersonality;

{ Types }
type
  NSMapTableOptions = culong;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSMAPTABLE_PAS_R}
{$define NSMAPTABLE_PAS_R}

{ Records }
type
  NSMapTableKeyCallBacks = record
    hash: function (context: Pointer {bad params!!}): culong; cdecl;
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
procedure NSFreeMapTable(var table: NSMapTable); cdecl; external name 'NSFreeMapTable';
procedure NSResetMapTable(var table: NSMapTable); cdecl; external name 'NSResetMapTable';
function NSCompareMapTables(var table1: NSMapTable; var table2: NSMapTable): Boolean; cdecl; external name 'NSCompareMapTables';
function NSCopyMapTableWithZone(var table: NSMapTable; var zone: NSZone): NSMapTable; cdecl; external name 'NSCopyMapTableWithZone';
function NSMapMember(var table: NSMapTable; var key: Pointer; originalKey: Pointer {Pointer}; value: Pointer {Pointer}): Boolean; cdecl; external name 'NSMapMember';
procedure NSMapGet(var table: NSMapTable; var key: Pointer); cdecl; external name 'NSMapGet';
procedure NSMapInsert(var table: NSMapTable; var key: Pointer; var value: Pointer); cdecl; external name 'NSMapInsert';
procedure NSMapInsertKnownAbsent(var table: NSMapTable; var key: Pointer; var value: Pointer); cdecl; external name 'NSMapInsertKnownAbsent';
procedure NSMapInsertIfAbsent(var table: NSMapTable; var key: Pointer; var value: Pointer); cdecl; external name 'NSMapInsertIfAbsent';
procedure NSMapRemove(var table: NSMapTable; var key: Pointer); cdecl; external name 'NSMapRemove';
function NSEnumerateMapTable(var table: NSMapTable): NSMapEnumerator; cdecl; external name 'NSEnumerateMapTable';
function NSNextMapEnumeratorPair(var enumerator: NSMapEnumerator; key: Pointer {Pointer}; value: Pointer {Pointer}): Boolean; cdecl; external name 'NSNextMapEnumeratorPair';
procedure NSEndMapTableEnumeration(var enumerator: NSMapEnumerator); cdecl; external name 'NSEndMapTableEnumeration';
function NSCountMapTable(var table: NSMapTable): culong; cdecl; external name 'NSCountMapTable';
function NSStringFromMapTable(var table: NSMapTable): NSString; cdecl; external name 'NSStringFromMapTable';
function NSAllMapTableKeys(var table: NSMapTable): NSArray; cdecl; external name 'NSAllMapTableKeys';
function NSAllMapTableValues(var table: NSMapTable): NSArray; cdecl; external name 'NSAllMapTableValues';
function NSCreateMapTableWithZone(keyCallBacks: NSMapTableKeyCallBacks; valueCallBacks: NSMapTableValueCallBacks; capacity: culong; var zone: NSZone): NSMapTable; cdecl; external name 'NSCreateMapTableWithZone';
function NSCreateMapTable(keyCallBacks: NSMapTableKeyCallBacks; valueCallBacks: NSMapTableValueCallBacks; capacity: culong): NSMapTable; cdecl; external name 'NSCreateMapTable';

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSMAPTABLE_PAS_T}
{$define NSMAPTABLE_PAS_T}

{ External symbols }
var
  NSNonOwnedPointerMapKeyCallBacks: NSMapTableKeyCallBacks; external name '_NSNonOwnedPointerMapKeyCallBacks';
  NSNonOwnedPointerOrNullMapKeyCallBacks: NSMapTableKeyCallBacks; external name '_NSNonOwnedPointerOrNullMapKeyCallBacks';
  NSNonRetainedObjectMapKeyCallBacks: NSMapTableKeyCallBacks; external name '_NSNonRetainedObjectMapKeyCallBacks';
  NSObjectMapKeyCallBacks: NSMapTableKeyCallBacks; external name '_NSObjectMapKeyCallBacks';
  NSOwnedPointerMapKeyCallBacks: NSMapTableKeyCallBacks; external name '_NSOwnedPointerMapKeyCallBacks';
  NSNonOwnedPointerMapValueCallBacks: NSMapTableValueCallBacks; external name '_NSNonOwnedPointerMapValueCallBacks';
  NSObjectMapValueCallBacks: NSMapTableValueCallBacks; external name '_NSObjectMapValueCallBacks';
  NSNonRetainedObjectMapValueCallBacks: NSMapTableValueCallBacks; external name '_NSNonRetainedObjectMapValueCallBacks';
  NSOwnedPointerMapValueCallBacks: NSMapTableValueCallBacks; external name '_NSOwnedPointerMapValueCallBacks';

{$endif}
{$endif}

{$ifdef FORWARD}
  NSMapTable = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSMAPTABLE_PAS_C}
{$define NSMAPTABLE_PAS_C}

{ NSMapTable }
  NSMapTable = objcclass(NSObject, NSCopyingProtocol, NSCodingProtocol, NSFastEnumerationProtocol)
    
  public
    class function alloc: NSMapTable; message 'alloc';

    function initWithKeyOptions_valueOptions_capacity(keyOptions: NSPointerFunctionsOptions; valueOptions: NSPointerFunctionsOptions; initialCapacity: culong): id; message 'initWithKeyOptions:valueOptions:capacity:';
    function initWithKeyPointerFunctions_valuePointerFunctions_capacity(keyFunctions: NSPointerFunctions; valueFunctions: NSPointerFunctions; initialCapacity: culong): id; message 'initWithKeyPointerFunctions:valuePointerFunctions:capacity:';
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
    function count: culong; message 'count';
    function keyEnumerator: NSEnumerator; message 'keyEnumerator';
    function objectEnumerator: NSEnumerator; message 'objectEnumerator';
    procedure removeAllObjects; message 'removeAllObjects';
    function dictionaryRepresentation: NSDictionary; message 'dictionaryRepresentation';
  end; external;

{$endif}
{$endif}
