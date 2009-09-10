{ Parsed from Foundation.framework NSError.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:30:59 ICT 2009 }

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
{$ifndef NSERROR_PAS_T}
{$define NSERROR_PAS_T}

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
    __code: clong;
    __domain: NSString;
    __userInfo: NSDictionary;
    
  public
    class function alloc: NSError; message 'alloc';

    function initWithDomain_code_userInfo(domain_: NSString; code_: clong; dict: NSDictionary): id; message 'initWithDomain:code:userInfo:';
    class function errorWithDomain_code_userInfo(domain_: NSString; code_: clong; dict: NSDictionary): id; message 'errorWithDomain:code:userInfo:';
    function domain: NSString; message 'domain';
    function code: clong; message 'code';
    function userInfo: NSDictionary; message 'userInfo';
    function localizedDescription: NSString; message 'localizedDescription';
    function localizedFailureReason: NSString; message 'localizedFailureReason';
    function localizedRecoverySuggestion: NSString; message 'localizedRecoverySuggestion';
    function localizedRecoveryOptions: NSArray; message 'localizedRecoveryOptions';
    function recoveryAttempter: id; message 'recoveryAttempter';
  end; external;

{$endif}
{$endif}
