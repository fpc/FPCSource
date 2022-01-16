{ Parsed from Foundation.framework NSScriptCoercionHandler.h }


{$ifdef TYPES}
type
  NSScriptCoercionHandlerPtr = ^NSScriptCoercionHandler;
{$endif}

{$ifdef CLASSES}

type
  NSScriptCoercionHandler = objcclass external (NSObject)
  private
    _coercers: id;
  public
    class function sharedCoercionHandler: NSScriptCoercionHandler; message 'sharedCoercionHandler';
    function coerceValue_toClass (value: id; toClass: pobjc_class): id; message 'coerceValue:toClass:';
    procedure registerCoercer_selector_toConvertFromClass_toClass (coercer: id; selector: SEL; fromClass: pobjc_class; toClass: pobjc_class); message 'registerCoercer:selector:toConvertFromClass:toClass:';
  end;
{$endif}

