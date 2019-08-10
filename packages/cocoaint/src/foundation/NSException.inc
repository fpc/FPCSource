{ Parsed from Foundation.framework NSException.h }


{$ifdef TYPES}
type
  NSExceptionPtr = ^NSException;
  NSAssertionHandlerPtr = ^NSAssertionHandler;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSGenericException: NSString; cvar; external;
  NSRangeException: NSString; cvar; external;
  NSInvalidArgumentException: NSString; cvar; external;
  NSInternalInconsistencyException: NSString; cvar; external;
  NSMallocException: NSString; cvar; external;
  NSObjectInaccessibleException: NSString; cvar; external;
  NSObjectNotAvailableException: NSString; cvar; external;
  NSDestinationInvalidException: NSString; cvar; external;
  NSPortTimeoutException: NSString; cvar; external;
  NSInvalidSendPortException: NSString; cvar; external;
  NSInvalidReceivePortException: NSString; cvar; external;
  NSPortSendException: NSString; cvar; external;
  NSPortReceiveException: NSString; cvar; external;
  NSOldStyleException: NSString; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  NSException = objcclass external (NSObject, NSCopyingProtocol, NSCodingProtocol)
  private
    name: NSString;
    reason: NSString;
    userInfo: NSDictionary;
    reserved: id;
  public
    class function exceptionWithName_reason_userInfo (name_: NSString; reason_: NSString; userInfo_: NSDictionary): NSException; message 'exceptionWithName:reason:userInfo:';
    function initWithName_reason_userInfo (aName: NSString; aReason: NSString; aUserInfo: NSDictionary): instancetype; message 'initWithName:reason:userInfo:'; { NS_DESIGNATED_INITIALIZER }
    function name_: NSString; message 'name';
    function reason_: NSString; message 'reason';
    function userInfo_: NSDictionary; message 'userInfo';
    function callStackReturnAddresses: NSArray; message 'callStackReturnAddresses';
    function callStackSymbols: NSArray; message 'callStackSymbols';
    procedure raise_; message 'raise';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
  end;


type
  NSExceptionRaisingConveniences = objccategory external (NSException)
    class procedure raise_format (name__: NSString; format: NSString); varargs; message 'raise:format:';
    class procedure raise_format_arguments (name__: NSString; format: NSString; argList: va_list); message 'raise:format:arguments:';
  end;
{$endif}

{$ifdef TYPES}
type
  NSUncaughtExceptionHandler = procedure (exception: NSException); cdecl;
{$endif}

{$ifdef FUNCTIONS}
function NSGetUncaughtExceptionHandler: NSUncaughtExceptionHandler; cdecl; external;
procedure NSSetUncaughtExceptionHandler(param1: NSUncaughtExceptionHandler); cdecl; external;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSAssertionHandlerKey: NSString { available in 10_6, 4_0 }; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  NSAssertionHandler = objcclass external (NSObject)
  private
    _reserved: pointer;
  public
    class function currentHandler: NSAssertionHandler; message 'currentHandler';
    procedure handleFailureInMethod_object_file_lineNumber_description (selector: SEL; object_: id; fileName: NSString; line: NSInteger; format: NSString); varargs; message 'handleFailureInMethod:object:file:lineNumber:description:';
    procedure handleFailureInFunction_file_lineNumber_description (functionName: NSString; fileName: NSString; line: NSInteger; format: NSString); varargs; message 'handleFailureInFunction:file:lineNumber:description:';
  end;
{$endif}

