{ Parsed from Foundation.framework NSMethodSignature.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Thu Sep 17 14:52:46 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSMETHODSIGNATURE_PAS_H}
{$define NSMETHODSIGNATURE_PAS_H}
type
  NSMethodSignaturePointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSMETHODSIGNATURE_PAS_T}
{$define NSMETHODSIGNATURE_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSMETHODSIGNATURE_PAS_R}
{$define NSMETHODSIGNATURE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSMETHODSIGNATURE_PAS_F}
{$define NSMETHODSIGNATURE_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSMETHODSIGNATURE_PAS_T}
{$define NSMETHODSIGNATURE_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSMethodSignature = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSMETHODSIGNATURE_PAS_C}
{$define NSMETHODSIGNATURE_PAS_C}

{ NSMethodSignature }
  NSMethodSignature = objcclass(NSObject)
  private
    __private: Pointer;
        _reserved: array[0..5] of Pointer;
    
  public
    class function alloc: NSMethodSignature; message 'alloc';

    class function signatureWithObjCTypes(types: PChar): NSMethodSignature; message 'signatureWithObjCTypes:';
    function numberOfArguments: NSUInteger; message 'numberOfArguments';
    function getArgumentTypeAtIndex(idx: NSUInteger): PChar; message 'getArgumentTypeAtIndex:';
    function frameLength: NSUInteger; message 'frameLength';
    function isOneway: Boolean; message 'isOneway';
    function methodReturnType: PChar; message 'methodReturnType';
    function methodReturnLength: NSUInteger; message 'methodReturnLength';
  end; external;

{$endif}
{$endif}
