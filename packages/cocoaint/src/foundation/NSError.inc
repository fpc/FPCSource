{ Parsed from Foundation.framework NSError.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Mon Sep 28 15:49:51 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSERROR_PAS_H}
{$define NSERROR_PAS_H}
type
  NSErrorPointer = Pointer;

{$endif}
{$endif}

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
  NSCocoaErrorDomain: NSString; external name '_NSCocoaErrorDomain';
  NSPOSIXErrorDomain: NSString; external name '_NSPOSIXErrorDomain';
  NSOSStatusErrorDomain: NSString; external name '_NSOSStatusErrorDomain';
  NSMachErrorDomain: NSString; external name '_NSMachErrorDomain';
  NSUnderlyingErrorKey: NSString; external name '_NSUnderlyingErrorKey';
  NSLocalizedDescriptionKey: NSString; external name '_NSLocalizedDescriptionKey';
  NSLocalizedFailureReasonErrorKey: NSString; external name '_NSLocalizedFailureReasonErrorKey';
  NSLocalizedRecoverySuggestionErrorKey: NSString; external name '_NSLocalizedRecoverySuggestionErrorKey';
  NSLocalizedRecoveryOptionsErrorKey: NSString; external name '_NSLocalizedRecoveryOptionsErrorKey';
  NSRecoveryAttempterErrorKey: NSString; external name '_NSRecoveryAttempterErrorKey';
  NSStringEncodingErrorKey: NSString; external name '_NSStringEncodingErrorKey';
  NSURLErrorKey: NSString; external name '_NSURLErrorKey';
  NSFilePathErrorKey: NSString; external name '_NSFilePathErrorKey';

{$endif}
{$endif}

{$ifdef FORWARD}
  NSError = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSERROR_PAS_C}
{$define NSERROR_PAS_C}

{ NSError }
  NSError = objcclass(NSObject, NSCopyingProtocol, NSCodingProtocol)
  private
    __reserved: Pointer;
    __code: NSInteger;
    __domain: NSString;
    __userInfo: NSDictionary;
    
  public
    class function alloc: NSError; message 'alloc';

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
  end; external;

{$endif}
{$endif}
