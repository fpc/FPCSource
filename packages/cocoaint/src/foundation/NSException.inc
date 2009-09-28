{ Parsed from Foundation.framework NSException.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Mon Sep 28 15:49:51 ICT 2009 }

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
procedure NSSetUncaughtExceptionHandler(Pointer_: NSUncaughtExceptionHandler); cdecl; external name 'NSSetUncaughtExceptionHandler';

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSEXCEPTION_PAS_S}
{$define NSEXCEPTION_PAS_S}

{ External string constants }
var
  NSGenericException: NSString; external name '_NSGenericException';
  NSRangeException: NSString; external name '_NSRangeException';
  NSInvalidArgumentException: NSString; external name '_NSInvalidArgumentException';
  NSInternalInconsistencyException: NSString; external name '_NSInternalInconsistencyException';
  NSMallocException: NSString; external name '_NSMallocException';
  NSObjectInaccessibleException: NSString; external name '_NSObjectInaccessibleException';
  NSObjectNotAvailableException: NSString; external name '_NSObjectNotAvailableException';
  NSDestinationInvalidException: NSString; external name '_NSDestinationInvalidException';
  NSPortTimeoutException: NSString; external name '_NSPortTimeoutException';
  NSInvalidSendPortException: NSString; external name '_NSInvalidSendPortException';
  NSInvalidReceivePortException: NSString; external name '_NSInvalidReceivePortException';
  NSPortSendException: NSString; external name '_NSPortSendException';
  NSPortReceiveException: NSString; external name '_NSPortReceiveException';
  NSOldStyleException: NSString; external name '_NSOldStyleException';

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
    class procedure raise_format(name_: NSString; format: NSString); varargs; message 'raise:format:';
    class procedure raise_format_arguments(name_: NSString; format: NSString; argList: va_list); message 'raise:format:arguments:';
  end; external;

{ NSAssertionHandler }
  NSAssertionHandler = objcclass(NSObject)
  private
    __reserved: Pointer;
    
  public
    class function alloc: NSAssertionHandler; message 'alloc';

    class function currentHandler: NSAssertionHandler; message 'currentHandler';
    procedure handleFailureInMethod_object_file_lineNumber_description(selector: SEL; object_: id; fileName: NSString; line: NSInteger; format: NSString); varargs; message 'handleFailureInMethod:object:file:lineNumber:description:';
    procedure handleFailureInFunction_file_lineNumber_description(functionName: NSString; fileName: NSString; line: NSInteger; format: NSString); varargs; message 'handleFailureInFunction:file:lineNumber:description:';
  end; external;

{$endif}
{$endif}
