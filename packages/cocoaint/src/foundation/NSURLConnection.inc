{ Parsed from Foundation.framework NSURLConnection.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:00 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSURLCONNECTION_PAS_H}
{$define NSURLCONNECTION_PAS_H}
type
  NSURLConnectionPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSURLCONNECTION_PAS_T}
{$define NSURLCONNECTION_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSURLCONNECTION_PAS_R}
{$define NSURLCONNECTION_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSURLCONNECTION_PAS_F}
{$define NSURLCONNECTION_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSURLCONNECTION_PAS_T}
{$define NSURLCONNECTION_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSURLConnection = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSURLCONNECTION_PAS_C}
{$define NSURLCONNECTION_PAS_C}

{ NSURLConnection }
  NSURLConnection = objcclass(NSObject)
  private
    __internal: NSURLConnectionInternal;
    
  public
    class function alloc: NSURLConnection; message 'alloc';

    class function canHandleRequest(request: NSURLRequest): Boolean; message 'canHandleRequest:';
    class function connectionWithRequest_delegate(request: NSURLRequest; delegate: id): NSURLConnection; message 'connectionWithRequest:delegate:';
    function initWithRequest_delegate(request: NSURLRequest; delegate: id): id; message 'initWithRequest:delegate:';
    function initWithRequest_delegate_startImmediately(request: NSURLRequest; delegate: id; startImmediately: Boolean): id; message 'initWithRequest:delegate:startImmediately:';
    procedure cancel; message 'cancel';
    procedure scheduleInRunLoop_forMode(aRunLoop: NSRunLoop; mode: NSString); message 'scheduleInRunLoop:forMode:';
    procedure unscheduleFromRunLoop_forMode(aRunLoop: NSRunLoop; mode: NSString); message 'unscheduleFromRunLoop:forMode:';

    { Category: NSURLConnectionSynchronousLoading }
    class function sendSynchronousRequest_returningResponse_error(request: NSURLRequest; var response: NSURLResponse; var error: NSError): NSData; message 'sendSynchronousRequest:returningResponse:error:';
  end; external;

{$endif}
{$endif}
