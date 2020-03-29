{ Parsed from LocalAuthentication.framework LAContext.h }


{$ifdef TYPES}
type
  LAContextPtr = ^LAContext;
{$endif}

{$ifdef TYPES}
type
  LAPolicy = NSInteger;
  LAPolicyPtr = ^LAPolicy;

const
  LAPolicyDeviceOwnerAuthenticationWithBiometrics = kLAPolicyDeviceOwnerAuthenticationWithBiometrics { available in NA, 8_0 };
{$endif}

{$ifdef CLASSES}

type
  LAContext = objcclass external (NSObject)
  public
    function canEvaluatePolicy_error (policy: LAPolicy; error: NSErrorPtr): ObjCBOOL; message 'canEvaluatePolicy:error:';
    procedure evaluatePolicy_localizedReason_reply (policy: LAPolicy; localizedReason: NSString; reply: OpaqueCBlock); message 'evaluatePolicy:localizedReason:reply:';
    procedure setLocalizedFallbackTitle(newValue: NSString); message 'setLocalizedFallbackTitle:';
    function localizedFallbackTitle: NSString; message 'localizedFallbackTitle';
  end;
{$endif}

