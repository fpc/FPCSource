{ Parsed from Foundation.framework NSMethodSignature.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:00 ICT 2009 }

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
    __reserved: Pointer;
    
  public
    class function alloc: NSMethodSignature; message 'alloc';

    class function signatureWithObjCTypes(types: PChar): NSMethodSignature; message 'signatureWithObjCTypes:';
    function numberOfArguments: culong; message 'numberOfArguments';
    function getArgumentTypeAtIndex(idx: culong): char; message 'getArgumentTypeAtIndex:';
    function frameLength: culong; message 'frameLength';
    function isOneway: Boolean; message 'isOneway';
    function methodReturnType: char; message 'methodReturnType';
    function methodReturnLength: culong; message 'methodReturnLength';
  end; external;

{$endif}
{$endif}
