{ Parsed from Foundation.framework NSPointerArray.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Mon Sep 28 15:49:51 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSPOINTERARRAY_PAS_H}
{$define NSPOINTERARRAY_PAS_H}
type
  NSPointerArrayPointer = Pointer;

{$endif}
{$endif}

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

{$endif}

{$ifdef CLASSES}
{$ifndef NSPOINTERARRAY_PAS_C}
{$define NSPOINTERARRAY_PAS_C}

{ NSPointerArray }
  NSPointerArray = objcclass(NSObject, NSFastEnumerationProtocol, NSCopyingProtocol, NSCodingProtocol)
    
  public
    class function alloc: NSPointerArray; message 'alloc';

    function pointerFunctions: NSPointerFunctions; message 'pointerFunctions';
    function pointerAtIndex(index: NSUInteger): Pointer; message 'pointerAtIndex:';
    procedure addPointer(pointer_: Pointer); message 'addPointer:';
    procedure removePointerAtIndex(index: NSUInteger); message 'removePointerAtIndex:';
    procedure insertPointer_atIndex(item: Pointer; index: NSUInteger); message 'insertPointer:atIndex:';
    procedure replacePointerAtIndex_withPointer(index: NSUInteger; item: Pointer); message 'replacePointerAtIndex:withPointer:';
    procedure compact; message 'compact';
    function count: NSUInteger; message 'count';
    procedure setCount(count_: NSUInteger); message 'setCount:';

    { Category: NSArrayConveniences }
    function allObjects: NSArray; message 'allObjects';
  end; external;

{$endif}
{$endif}
