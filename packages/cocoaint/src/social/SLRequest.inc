{ Parsed from Social.framework SLRequest.h }


{ Types from SLRequest }
{$ifdef TYPES}

{$endif}


{$ifdef TYPES}
type
  SLRequestPtr = ^SLRequest;
{$endif}

{$ifdef TYPES}
type
  SLRequestMethod = NSInteger;
  SLRequestMethodPtr = ^SLRequestMethod;

const
  SLRequestMethodGET = 0;
  SLRequestMethodPOST = 1;
  SLRequestMethodDELETE = 2;
  SLRequestMethodPUT = 3;

type
  SLRequestHandler = OpaqueCBlock;
{$endif}

{$ifdef CLASSES}

type
  SLRequest = objcclass external (NSObject)
  public
    class function requestForServiceType_requestMethod_URL_parameters (serviceType: NSString; requestMethod: SLRequestMethod; url: NSURL; parameters: NSDictionary): SLRequest; message 'requestForServiceType:requestMethod:URL:parameters:';
    procedure setAccount(newValue: ACAccount); message 'setAccount:';
    function account: ACAccount; message 'account';
    function requestMethod: SLRequestMethod; message 'requestMethod';
    function URL: NSURL; message 'URL';
    function parameters: NSDictionary; message 'parameters';
    procedure addMultipartData_withName_type_filename (data: NSData; name: NSString; type_: NSString; filename: NSString); message 'addMultipartData:withName:type:filename:';
    {$if  not (defined(TARGET_OS_IPHONE) or defined(TARGET_IPHONE_SIMULATOR))}
    procedure addMultipartData_withName_type (data: NSData; name: NSString; type_: NSString); message 'addMultipartData:withName:type:';
    {$endif}
    function preparedURLRequest: NSURLRequest; message 'preparedURLRequest';
    procedure performRequestWithHandler (handler: SLRequestHandler); message 'performRequestWithHandler:';
  end;
{$endif}

