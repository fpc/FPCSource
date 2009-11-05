{ Parsed from Foundation.framework NSMethodSignature.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:06 ICT 2009 }


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
{$ifndef NSMETHODSIGNATURE_PAS_S}
{$define NSMETHODSIGNATURE_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSMethodSignature = objcclass;
  NSMethodSignaturePointer = ^NSMethodSignature;

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
