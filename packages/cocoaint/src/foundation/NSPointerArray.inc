{ Parsed from Foundation.framework NSPointerArray.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:00 ICT 2009 }

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
{$ifndef NSPOINTERARRAY_PAS_T}
{$define NSPOINTERARRAY_PAS_T}

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
    function pointerAtIndex(index: culong): Pointer; message 'pointerAtIndex:';
    procedure addPointer(pointer_: Pointer); message 'addPointer:';
    procedure removePointerAtIndex(index: culong); message 'removePointerAtIndex:';
    procedure insertPointer_atIndex(item: Pointer; index: culong); message 'insertPointer:atIndex:';
    procedure replacePointerAtIndex_withPointer(index: culong; item: Pointer); message 'replacePointerAtIndex:withPointer:';
    procedure compact; message 'compact';
    function count: culong; message 'count';
    procedure setCount(count_: culong); message 'setCount:';

    { Category: NSArrayConveniences }
    function allObjects: NSArray; message 'allObjects';
  end; external;

{$endif}
{$endif}
