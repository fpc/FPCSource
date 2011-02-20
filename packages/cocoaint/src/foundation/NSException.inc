{ Parsed from Foundation.framework NSException.h }

{$ifdef TYPES}
{$ifndef NSEXCEPTION_PAS_T}
{$define NSEXCEPTION_PAS_T}

{ Callbacks }
type
  NSUncaughtExceptionHandler = procedure (exception: NSException); cdecl;


{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSEXCEPTION_PAS_R}
{$define NSEXCEPTION_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSEXCEPTION_PAS_F}
{$define NSEXCEPTION_PAS_F}

{ Functions }
function NSGetUncaughtExceptionHandler: NSUncaughtExceptionHandler; cdecl; external;
procedure NSSetUncaughtExceptionHandler(param1: NSUncaughtExceptionHandler); cdecl; external;

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSEXCEPTION_PAS_S}
{$define NSEXCEPTION_PAS_S}

{ External string constants }
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
  NSAssertionHandlerKey: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  NSException = objcclass;
  NSExceptionPointer = ^NSException;
  NSExceptionPtr = NSExceptionPointer;
  NSAssertionHandler = objcclass;
  NSAssertionHandlerPointer = ^NSAssertionHandler;
  NSAssertionHandlerPtr = NSAssertionHandlerPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSEXCEPTION_PAS_C}
{$define NSEXCEPTION_PAS_C}

{ NSException }
  NSException = objcclass external (NSObject, NSCopyingProtocol, NSCodingProtocol)
  private
    _name: NSString;
    _reason: NSString;
    _userInfo: NSDictionary;
    reserved: id;
    
  public
    class function exceptionWithName_reason_userInfo(name_: NSString; reason_: NSString; userInfo_: NSDictionary): NSException; message 'exceptionWithName:reason:userInfo:';
    function initWithName_reason_userInfo(aName: NSString; aReason: NSString; aUserInfo: NSDictionary): id; message 'initWithName:reason:userInfo:';
    function name: NSString; message 'name';
    function reason: NSString; message 'reason';
    function userInfo: NSDictionary; message 'userInfo';
    function callStackReturnAddresses: NSArray; message 'callStackReturnAddresses';
    function callStackSymbols: NSArray; message 'callStackSymbols';
    procedure raise_; message 'raise';

    { Adopted Protocols }
    function copyWithZone(zone_: NSZonePtr): id;
    procedure encodeWithCoder(aCoder: NSCoder);
    function initWithCoder(aDecoder: NSCoder): id;
  end;

{ NSAssertionHandler }
  NSAssertionHandler = objcclass external (NSObject)
  private
    _reserved: Pointer;
    
  public
    class function currentHandler: NSAssertionHandler; message 'currentHandler';
    procedure handleFailureInMethod_object_file_lineNumber_description(selector: SEL; object_: id; fileName: NSString; line: NSInteger; firstKey: id); varargs; message 'handleFailureInMethod:object:file:lineNumber:description:';
    procedure handleFailureInFunction_file_lineNumber_description(functionName: NSString; fileName: NSString; line: NSInteger; firstKey: id); varargs; message 'handleFailureInFunction:file:lineNumber:description:';
  end;

{ NSExceptionRaisingConveniencesCategory }
  NSExceptionRaisingConveniencesCategory = objccategory external (NSException)
    class procedure raise_format(name_: NSString; firstKey: id); varargs; message 'raise:format:';
    class procedure raise_format_arguments(name_: NSString; format: NSString; argList: va_list); message 'raise:format:arguments:';
  end;

{$endif}
{$endif}
