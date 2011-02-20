{ Parsed from Foundation.framework NSPointerFunctions.h }

{$ifdef TYPES}
{$ifndef NSPOINTERFUNCTIONS_PAS_T}
{$define NSPOINTERFUNCTIONS_PAS_T}

{ Constants }

const
  NSPointerFunctionsStrongMemory = 0 shl 0;
  NSPointerFunctionsZeroingWeakMemory = 1 shl 0;
  NSPointerFunctionsOpaqueMemory = 2 shl 0;
  NSPointerFunctionsMallocMemory = 3 shl 0;
  NSPointerFunctionsMachVirtualMemory = 4 shl 0;
  NSPointerFunctionsObjectPersonality = 0 shl 8;
  NSPointerFunctionsOpaquePersonality = 1 shl 8;
  NSPointerFunctionsObjectPointerPersonality = 2 shl 8;
  NSPointerFunctionsCStringPersonality = 3 shl 8;
  NSPointerFunctionsStructPersonality = 4 shl 8;
  NSPointerFunctionsIntegerPersonality = 5 shl 8;
  NSPointerFunctionsCopyIn = 1 shl 16;

{ Types }
type
  NSPointerFunctionsOptions = NSUInteger;
  NSPointerFunctionsOptionsPointer = ^NSPointerFunctionsOptions;
  NSPointerFunctionsOptionsPtr = NSPointerFunctionsOptionsPointer;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSPOINTERFUNCTIONS_PAS_R}
{$define NSPOINTERFUNCTIONS_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSPOINTERFUNCTIONS_PAS_F}
{$define NSPOINTERFUNCTIONS_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSPOINTERFUNCTIONS_PAS_S}
{$define NSPOINTERFUNCTIONS_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSPointerFunctions = objcclass;
  NSPointerFunctionsPtr = ^NSPointerFunctions;

{$endif}

{$ifdef CLASSES}
{$ifndef NSPOINTERFUNCTIONS_PAS_C}
{$define NSPOINTERFUNCTIONS_PAS_C}

{ NSPointerFunctions }
  NSPointerFunctions = objcclass external (NSObject, NSCopyingProtocol)
    
  public
    function initWithOptions(options: NSPointerFunctionsOptions): id; message 'initWithOptions:';
    class function pointerFunctionsWithOptions(options: NSPointerFunctionsOptions): id; message 'pointerFunctionsWithOptions:';
(*
    procedure setItem (newValue: NSUInteger); message 'setItem:';
    function item: NSUInteger; message 'item';
    procedure setItem1 (newValue: Boolean); message 'setItem1:';
    function item1: Boolean; message 'item1';
    procedure setItem2 (newValue: Boolean); message 'setItem2:';
    function item2: Boolean; message 'item2';
    procedure set_ (newValue: NSUInteger); message 'set_:';
    function : NSUInteger; message '';
    procedure setSrc (newValue: Pointer); message 'setSrc:';
    function src: Pointer; message 'src';
*)
    procedure setUsesStrongWriteBarrier (newValue: Boolean); message 'setUsesStrongWriteBarrier:';
    function usesStrongWriteBarrier: Boolean; message 'usesStrongWriteBarrier';
    procedure setUsesWeakReadAndWriteBarriers (newValue: Boolean); message 'setUsesWeakReadAndWriteBarriers:';
    function usesWeakReadAndWriteBarriers: Boolean; message 'usesWeakReadAndWriteBarriers';

    { Adopted Protocols }
    function copyWithZone(zone_: NSZonePtr): id;
  end;

{$endif}
{$endif}
