{ Parsed from Foundation.framework NSScriptCoercionHandler.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:00 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSSCRIPTCOERCIONHANDLER_PAS_H}
{$define NSSCRIPTCOERCIONHANDLER_PAS_H}
type
  NSScriptCoercionHandlerPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSSCRIPTCOERCIONHANDLER_PAS_T}
{$define NSSCRIPTCOERCIONHANDLER_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSSCRIPTCOERCIONHANDLER_PAS_R}
{$define NSSCRIPTCOERCIONHANDLER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSSCRIPTCOERCIONHANDLER_PAS_F}
{$define NSSCRIPTCOERCIONHANDLER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSSCRIPTCOERCIONHANDLER_PAS_T}
{$define NSSCRIPTCOERCIONHANDLER_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSScriptCoercionHandler = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSSCRIPTCOERCIONHANDLER_PAS_C}
{$define NSSCRIPTCOERCIONHANDLER_PAS_C}

{ NSScriptCoercionHandler }
  NSScriptCoercionHandler = objcclass(NSObject)
  private
    __coercers: id;
    
  public
    class function alloc: NSScriptCoercionHandler; message 'alloc';

    class function sharedCoercionHandler: NSScriptCoercionHandler; message 'sharedCoercionHandler';
    function coerceValue_toClass(value: id; toClass: Pobjc_class): id; message 'coerceValue:toClass:';
    procedure registerCoercer_selector_toConvertFromClass_toClass(coercer: id; selector: SEL; fromClass: Pobjc_class; toClass: Pobjc_class); message 'registerCoercer:selector:toConvertFromClass:toClass:';
  end; external;

{$endif}
{$endif}
