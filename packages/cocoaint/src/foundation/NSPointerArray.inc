{ Parsed from Foundation.framework NSPointerArray.h }

{$ifdef TYPES}
{$ifndef NSPOINTERARRAY_PAS_T}
{$define NSPOINTERARRAY_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSPOINTERARRAY_PAS_R}
{$define NSPOINTERARRAY_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSPOINTERARRAY_PAS_F}
{$define NSPOINTERARRAY_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSPOINTERARRAY_PAS_S}
{$define NSPOINTERARRAY_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSPointerArray = objcclass;
  NSPointerArrayPointer = ^NSPointerArray;
  NSPointerArrayPtr = NSPointerArrayPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSPOINTERARRAY_PAS_C}
{$define NSPOINTERARRAY_PAS_C}

{ NSPointerArray }
  NSPointerArray = objcclass external (NSObject, NSFastEnumerationProtocol, NSCopyingProtocol, NSCodingProtocol)
    
  public
    function pointerFunctions: NSPointerFunctions; message 'pointerFunctions';
    function pointerAtIndex(index: NSUInteger): Pointer; message 'pointerAtIndex:';
    procedure addPointer(pointer_: Pointer); message 'addPointer:';
    procedure removePointerAtIndex(index: NSUInteger); message 'removePointerAtIndex:';
    procedure insertPointer_atIndex(item: Pointer; index: NSUInteger); message 'insertPointer:atIndex:';
    procedure replacePointerAtIndex_withPointer(index: NSUInteger; item: Pointer); message 'replacePointerAtIndex:withPointer:';
    procedure compact; message 'compact';
    function count: NSUInteger; message 'count';
    procedure setCount(count_: NSUInteger); message 'setCount:';

    { Adopted Protocols }
    function countByEnumeratingWithState_objects_count(state: NSFastEnumerationStatePtr; stackbuf: idPtr; len: NSUInteger): NSUInteger;
    function copyWithZone(zone_: NSZonePtr): id;
    procedure encodeWithCoder(aCoder: NSCoder);
    function initWithCoder(aDecoder: NSCoder): id;
  end;

{ NSArrayConveniencesCategory }
  NSArrayConveniencesCategory = objccategory external (NSPointerArray)
    function allObjects: NSArray; message 'allObjects';
  end;

{$endif}
{$endif}
