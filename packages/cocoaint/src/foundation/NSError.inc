{ Parsed from Foundation.framework NSError.h }


{$ifdef TYPES}
type
  NSErrorPtr = ^NSError;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSCocoaErrorDomain: NSString; cvar; external;
  NSPOSIXErrorDomain: NSString; cvar; external;
  NSOSStatusErrorDomain: NSString; cvar; external;
  NSMachErrorDomain: NSString; cvar; external;
  NSUnderlyingErrorKey: NSString; cvar; external;
  NSLocalizedDescriptionKey: NSString; cvar; external;
  NSLocalizedFailureReasonErrorKey: NSString; cvar; external;
  NSLocalizedRecoverySuggestionErrorKey: NSString; cvar; external;
  NSLocalizedRecoveryOptionsErrorKey: NSString; cvar; external;
  NSRecoveryAttempterErrorKey: NSString; cvar; external;
  NSHelpAnchorErrorKey: NSString; cvar; external;
  NSStringEncodingErrorKey: NSString; cvar; external;
  NSURLErrorKey: NSString; cvar; external;
  NSFilePathErrorKey: NSString; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  NSError = objcclass external (NSObject, NSCopyingProtocol, NSSecureCodingProtocol)
  private
    _reserved: pointer;
    _code: NSInteger;
    _domain: NSString;
    _userInfo: NSDictionary;
  public
    function initWithDomain_code_userInfo (domain: NSString; code: NSInteger; dict: NSDictionary): instancetype; message 'initWithDomain:code:userInfo:'; { NS_DESIGNATED_INITIALIZER }
    class function errorWithDomain_code_userInfo (domain: NSString; code: NSInteger; dict: NSDictionary): instancetype; message 'errorWithDomain:code:userInfo:';
    function domain: NSString; message 'domain';
    function code: NSInteger; message 'code';
    function userInfo: NSDictionary; message 'userInfo';
    function localizedDescription: NSString; message 'localizedDescription';
    function localizedFailureReason: NSString; message 'localizedFailureReason';
    function localizedRecoverySuggestion: NSString; message 'localizedRecoverySuggestion';
    function localizedRecoveryOptions: NSArray; message 'localizedRecoveryOptions';
    function recoveryAttempter: id; message 'recoveryAttempter';
    function helpAnchor: NSString; message 'helpAnchor';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
  end;


type
  NSErrorRecoveryAttempting = objccategory external (NSObject)
    procedure attemptRecoveryFromError_optionIndex_delegate_didRecoverSelector_contextInfo (error: NSError; recoveryOptionIndex: NSUInteger; delegate: id; didRecoverSelector: SEL; contextInfo: pointer); message 'attemptRecoveryFromError:optionIndex:delegate:didRecoverSelector:contextInfo:';
    function attemptRecoveryFromError_optionIndex (error: NSError; recoveryOptionIndex: NSUInteger): ObjCBOOL; message 'attemptRecoveryFromError:optionIndex:';
  end;
{$endif}

