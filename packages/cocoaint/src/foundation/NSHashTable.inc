{ Parsed from Foundation.framework NSHashTable.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Mon Sep 28 15:49:51 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSHASHTABLE_PAS_H}
{$define NSHASHTABLE_PAS_H}
type
  NSHashTablePointer = Pointer;

{$endif}
{$endif}

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

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSHASHTABLE_PAS_R}
{$define NSHASHTABLE_PAS_R}

{ Records }
type
  NSHashTableCallBacks = record
    hash: function (context: Pointer {bad params!!}): NSUInteger; cdecl;
    isEqual: function (context: Pointer {bad params!!}): Boolean; cdecl;
    retain: procedure (context: Pointer {bad params!!}); cdecl;
    release: procedure (context: Pointer {bad params!!}); cdecl;
    describe: function (context: Pointer {bad params!!}): id; cdecl;
  end;


{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSHASHTABLE_PAS_F}
{$define NSHASHTABLE_PAS_F}

{ Functions }
procedure NSFreeHashTable(tablePointer: NSHashTable); cdecl; external name 'NSFreeHashTable';
procedure NSResetHashTable(tablePointer: NSHashTable); cdecl; external name 'NSResetHashTable';
function NSCompareHashTables(table1Pointer: NSHashTable; table2Pointer: NSHashTable): Boolean; cdecl; external name 'NSCompareHashTables';
function NSCopyHashTableWithZone(tablePointer: NSHashTable; zonePointer: NSZone): NSHashTable; cdecl; external name 'NSCopyHashTableWithZone';
procedure NSHashGet(tablePointer: NSHashTable; pointerPointer: Pointer); cdecl; external name 'NSHashGet';
procedure NSHashInsert(tablePointer: NSHashTable; pointerPointer: Pointer); cdecl; external name 'NSHashInsert';
procedure NSHashInsertKnownAbsent(tablePointer: NSHashTable; pointerPointer: Pointer); cdecl; external name 'NSHashInsertKnownAbsent';
procedure NSHashInsertIfAbsent(tablePointer: NSHashTable; pointerPointer: Pointer); cdecl; external name 'NSHashInsertIfAbsent';
procedure NSHashRemove(tablePointer: NSHashTable; pointerPointer: Pointer); cdecl; external name 'NSHashRemove';
function NSEnumerateHashTable(tablePointer: NSHashTable): NSHashEnumerator; cdecl; external name 'NSEnumerateHashTable';
procedure NSNextHashEnumeratorItem(enumeratorPointer: NSHashEnumerator); cdecl; external name 'NSNextHashEnumeratorItem';
procedure NSEndHashTableEnumeration(enumeratorPointer: NSHashEnumerator); cdecl; external name 'NSEndHashTableEnumeration';
function NSCountHashTable(tablePointer: NSHashTable): NSUInteger; cdecl; external name 'NSCountHashTable';
function NSStringFromHashTable(tablePointer: NSHashTable): NSString; cdecl; external name 'NSStringFromHashTable';
function NSAllHashTableObjects(tablePointer: NSHashTable): NSArray; cdecl; external name 'NSAllHashTableObjects';
function NSCreateHashTableWithZone(callBacks: NSHashTableCallBacks; capacity: NSUInteger; zonePointer: NSZone): NSHashTable; cdecl; external name 'NSCreateHashTableWithZone';
function NSCreateHashTable(callBacks: NSHashTableCallBacks; capacity: NSUInteger): NSHashTable; cdecl; external name 'NSCreateHashTable';

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSHASHTABLE_PAS_S}
{$define NSHASHTABLE_PAS_S}

{ External symbols }
var
  NSIntegerHashCallBacks: NSHashTableCallBacks; external name '_NSIntegerHashCallBacks';
  NSNonOwnedPointerHashCallBacks: NSHashTableCallBacks; external name '_NSNonOwnedPointerHashCallBacks';
  NSNonRetainedObjectHashCallBacks: NSHashTableCallBacks; external name '_NSNonRetainedObjectHashCallBacks';
  NSObjectHashCallBacks: NSHashTableCallBacks; external name '_NSObjectHashCallBacks';
  NSOwnedObjectIdentityHashCallBacks: NSHashTableCallBacks; external name '_NSOwnedObjectIdentityHashCallBacks';
  NSOwnedPointerHashCallBacks: NSHashTableCallBacks; external name '_NSOwnedPointerHashCallBacks';
  NSPointerToStructHashCallBacks: NSHashTableCallBacks; external name '_NSPointerToStructHashCallBacks';

{$endif}
{$endif}

{$ifdef FORWARD}
  NSHashTable = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSHASHTABLE_PAS_C}
{$define NSHASHTABLE_PAS_C}

{ NSHashTable }
  NSHashTable = objcclass(NSObject, NSCopyingProtocol, NSCodingProtocol, NSFastEnumerationProtocol)
    
  public
    class function alloc: NSHashTable; message 'alloc';

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
  end; external;

{$endif}
{$endif}
