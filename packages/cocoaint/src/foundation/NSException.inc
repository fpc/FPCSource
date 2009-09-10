{ Parsed from Foundation.framework NSException.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:30:59 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSEXCEPTION_PAS_H}
{$define NSEXCEPTION_PAS_H}
type
  NSExceptionPointer = Pointer;
  NSAssertionHandlerPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSEXCEPTION_PAS_T}
{$define NSEXCEPTION_PAS_T}

{ CFString constants }
var
  NSGenericException: CFStringRef; external name '_NSGenericException';
  NSRangeException: CFStringRef; external name '_NSRangeException';
  NSInvalidArgumentException: CFStringRef; external name '_NSInvalidArgumentException';
  NSInternalInconsistencyException: CFStringRef; external name '_NSInternalInconsistencyException';
  NSMallocException: CFStringRef; external name '_NSMallocException';
  NSObjectInaccessibleException: CFStringRef; external name '_NSObjectInaccessibleException';
  NSObjectNotAvailableException: CFStringRef; external name '_NSObjectNotAvailableException';
  NSDestinationInvalidException: CFStringRef; external name '_NSDestinationInvalidException';
  NSPortTimeoutException: CFStringRef; external name '_NSPortTimeoutException';
  NSInvalidSendPortException: CFStringRef; external name '_NSInvalidSendPortException';
  NSInvalidReceivePortException: CFStringRef; external name '_NSInvalidReceivePortException';
  NSPortSendException: CFStringRef; external name '_NSPortSendException';
  NSPortReceiveException: CFStringRef; external name '_NSPortReceiveException';
  NSOldStyleException: CFStringRef; external name '_NSOldStyleException';

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
function NSGetUncaughtExceptionHandler: NSUncaughtExceptionHandler; cdecl; external name 'NSGetUncaughtExceptionHandler';
procedure NSSetUncaughtExceptionHandler(var_: NSUncaughtExceptionHandler); cdecl; external name 'NSSetUncaughtExceptionHandler';

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSEXCEPTION_PAS_T}
{$define NSEXCEPTION_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSException = objcclass;
  NSAssertionHandler = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSEXCEPTION_PAS_C}
{$define NSEXCEPTION_PAS_C}

{ NSException }
  NSException = objcclass(NSObject, NSCopyingProtocol, NSCodingProtocol)
  private
    _name: NSString;
    _reason: NSString;
    _userInfo: NSDictionary;
    _reserved: id;
    
  public
    class function alloc: NSException; message 'alloc';

    class function exceptionWithName_reason_userInfo(name_: NSString; reason_: NSString; userInfo_: NSDictionary): NSException; message 'exceptionWithName:reason:userInfo:';
    function initWithName_reason_userInfo(aName: NSString; aReason: NSString; aUserInfo: NSDictionary): id; message 'initWithName:reason:userInfo:';
    function name: NSString; message 'name';
    function reason: NSString; message 'reason';
    function userInfo: NSDictionary; message 'userInfo';
    procedure raise_; message 'raise';

    { Category: NSExceptionRaisingConveniences }
    class procedure raise_format(name_: NSString; format: NSString); message 'raise:format:';
    class procedure raise_format_arguments(name_: NSString; format: NSString; argList: va_list); message 'raise:format:arguments:';
  end; external;

{ NSAssertionHandler }
  NSAssertionHandler = objcclass(NSObject)
  private
    __reserved: Pointer;
    
  public
    class function alloc: NSAssertionHandler; message 'alloc';

    class function currentHandler: NSAssertionHandler; message 'currentHandler';
    procedure handleFailureInMethod_object_file_lineNumber_description(selector: SEL; object_: id; fileName: NSString; line: clong; format: NSString); message 'handleFailureInMethod:object:file:lineNumber:description:';
    procedure handleFailureInFunction_file_lineNumber_description(functionName: NSString; fileName: NSString; line: clong; format: NSString); message 'handleFailureInFunction:file:lineNumber:description:';
  end; external;

{$endif}
{$endif}
