{ Parsed from Foundation.framework NSError.h }

{$ifdef TYPES}
{$ifndef NSERROR_PAS_T}
{$define NSERROR_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSERROR_PAS_R}
{$define NSERROR_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSERROR_PAS_F}
{$define NSERROR_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSERROR_PAS_S}
{$define NSERROR_PAS_S}

{ External string constants }
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
{$endif}

{$ifdef FORWARD}
  NSError = objcclass;
  NSErrorPointer = ^NSError;
  NSErrorPtr = NSErrorPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSERROR_PAS_C}
{$define NSERROR_PAS_C}

{ NSError }
  NSError = objcclass external (NSObject, NSCopyingProtocol, NSCodingProtocol)
  private
    _reserved: Pointer;
    _code: NSInteger;
    _domain: NSString;
    _userInfo: NSDictionary;
    
  public
    function initWithDomain_code_userInfo(domain_: NSString; code_: NSInteger; dict: NSDictionary): id; message 'initWithDomain:code:userInfo:';
    class function errorWithDomain_code_userInfo(domain_: NSString; code_: NSInteger; dict: NSDictionary): id; message 'errorWithDomain:code:userInfo:';
    function domain: NSString; message 'domain';
    function code: NSInteger; message 'code';
    function userInfo: NSDictionary; message 'userInfo';
    function localizedDescription: NSString; message 'localizedDescription';
    function localizedFailureReason: NSString; message 'localizedFailureReason';
    function localizedRecoverySuggestion: NSString; message 'localizedRecoverySuggestion';
    function localizedRecoveryOptions: NSArray; message 'localizedRecoveryOptions';
    function recoveryAttempter: id; message 'recoveryAttempter';
    function helpAnchor: NSString; message 'helpAnchor';

    { Adopted Protocols }
    function copyWithZone(zone_: NSZonePtr): id;
    procedure encodeWithCoder(aCoder: NSCoder);
    function initWithCoder(aDecoder: NSCoder): id;
  end;

{ NSErrorRecoveryAttemptingCategory }
  NSErrorRecoveryAttemptingCategory = objccategory external (NSObject)
    procedure attemptRecoveryFromError_optionIndex_delegate_didRecoverSelector_contextInfo(error: NSError; recoveryOptionIndex: NSUInteger; delegate: id; didRecoverSelector: SEL; contextInfo: Pointer); message 'attemptRecoveryFromError:optionIndex:delegate:didRecoverSelector:contextInfo:';
    function attemptRecoveryFromError_optionIndex(error: NSError; recoveryOptionIndex: NSUInteger): Boolean; message 'attemptRecoveryFromError:optionIndex:';
  end;

{$endif}
{$endif}
