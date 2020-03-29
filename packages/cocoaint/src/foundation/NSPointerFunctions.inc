{ Parsed from Foundation.framework NSPointerFunctions.h }


{ Callbacks }
{$ifdef TYPES}
type
  NSPointerFunctionsSizeCallback = function (item: pointer): NSUInteger; cdecl;
  NSPointerFunctionsHashFunctionCallback = function (item: pointer; size: NSPointerFunctionsSizeCallback): NSUInteger; cdecl;
  NSPointerFunctionsIsEqualFunctionCallback = function (item1: pointer; item2: pointer; size: NSPointerFunctionsSizeCallback): ObjCBOOL; cdecl;
  NSPointerFunctionsSizeFunctionCallback = function (item: pointer): NSUInteger; cdecl;
  NSPointerFunctionsDescriptionFunctionCallback = function (item: pointer): NSString; cdecl;
  NSPointerFunctionsRelinquishFunctionCallback = procedure (item: pointer; size: NSPointerFunctionsSizeCallback); cdecl;
  NSPointerFunctionsAcquireFunctionCallback = function (size: NSPointerFunctionsSizeCallback; shouldCopy: ObjCBOOL): pointer; cdecl;
{$endif}


{$ifdef TYPES}
type
  NSPointerFunctionsPtr = ^NSPointerFunctions;
{$endif}

{$if  not (defined(__FOUNDATION_NSPOINTERFUNCTIONS__))}
{$ifdef TYPES}
const
  __FOUNDATION_NSPOINTERFUNCTIONS__ = 1;

const
  NSPointerFunctionsStrongMemory = 0 shl 0 { available in 10_5, 6_0 };
  {$if (defined(TARGET_OS_MAC) and not (defined(TARGET_OS_EMBEDDED) or defined(TARGET_OS_IPHONE))) or defined(TARGET_OS_WIN32)}
  NSPointerFunctionsZeroingWeakMemory = 1 shl 0 deprecated 'in 10_5, 10_8';
  {$endif}
  NSPointerFunctionsOpaqueMemory = 2 shl 0 { available in 10_5, 6_0 };
  NSPointerFunctionsMallocMemory = 3 shl 0 { available in 10_5, 6_0 };
  NSPointerFunctionsMachVirtualMemory = 4 shl 0 { available in 10_5, 6_0 };
  NSPointerFunctionsWeakMemory = 5 shl 0 { available in 10_8, 6_0 };
  NSPointerFunctionsObjectPersonality = 0 shl 8 { available in 10_5, 6_0 };
  NSPointerFunctionsOpaquePersonality = 1 shl 8 { available in 10_5, 6_0 };
  NSPointerFunctionsObjectPointerPersonality = 2 shl 8 { available in 10_5, 6_0 };
  NSPointerFunctionsCStringPersonality = 3 shl 8 { available in 10_5, 6_0 };
  NSPointerFunctionsStructPersonality = 4 shl 8 { available in 10_5, 6_0 };
  NSPointerFunctionsIntegerPersonality = 5 shl 8 { available in 10_5, 6_0 };
  NSPointerFunctionsCopyIn = 1 shl 16 { available in 10_5, 6_0 };

type
  NSPointerFunctionsOptions = NSUInteger;
  NSPointerFunctionsOptionsPtr = ^NSPointerFunctionsOptions;
{$endif}

{$ifdef CLASSES}

type
  NSPointerFunctions = objcclass external (NSObject, NSCopyingProtocol)
  public
    function initWithOptions (options: NSPointerFunctionsOptions): instancetype; message 'initWithOptions:'; { NS_DESIGNATED_INITIALIZER }
    class function pointerFunctionsWithOptions (options: NSPointerFunctionsOptions): NSPointerFunctions; message 'pointerFunctionsWithOptions:';
    procedure setHashFunction(newValue: NSPointerFunctionsHashFunctionCallback); message 'setHashFunction:';
    function hashFunction: NSPointerFunctionsHashFunctionCallback; message 'hashFunction';
    procedure setIsEqualFunction(newValue: NSPointerFunctionsIsEqualFunctionCallback); message 'setIsEqualFunction:';
    function isEqualFunction: NSPointerFunctionsIsEqualFunctionCallback; message 'isEqualFunction';
    procedure setSizeFunction(newValue: NSPointerFunctionsSizeFunctionCallback); message 'setSizeFunction:';
    function sizeFunction: NSPointerFunctionsSizeFunctionCallback; message 'sizeFunction';
    procedure setDescriptionFunction(newValue: NSPointerFunctionsDescriptionFunctionCallback); message 'setDescriptionFunction:';
    function descriptionFunction: NSPointerFunctionsDescriptionFunctionCallback; message 'descriptionFunction';
    procedure setRelinquishFunction(newValue: NSPointerFunctionsRelinquishFunctionCallback); message 'setRelinquishFunction:';
    function relinquishFunction: NSPointerFunctionsRelinquishFunctionCallback; message 'relinquishFunction';
    procedure setAcquireFunction(newValue: NSPointerFunctionsAcquireFunctionCallback); message 'setAcquireFunction:';
    function acquireFunction: NSPointerFunctionsAcquireFunctionCallback; message 'acquireFunction';
    procedure setUsesStrongWriteBarrier(newValue: ObjCBOOL); message 'setUsesStrongWriteBarrier:';
    function usesStrongWriteBarrier: ObjCBOOL; message 'usesStrongWriteBarrier';
    procedure setUsesWeakReadAndWriteBarriers(newValue: ObjCBOOL); message 'setUsesWeakReadAndWriteBarriers:';
    function usesWeakReadAndWriteBarriers: ObjCBOOL; message 'usesWeakReadAndWriteBarriers';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
  end;
{$endif}

{$endif}
