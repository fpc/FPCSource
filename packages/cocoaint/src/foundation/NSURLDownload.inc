{ Parsed from Foundation.framework NSURLDownload.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Thu Sep 17 14:52:46 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSURLDOWNLOAD_PAS_H}
{$define NSURLDOWNLOAD_PAS_H}
type
  NSURLDownloadPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSURLDOWNLOAD_PAS_T}
{$define NSURLDOWNLOAD_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSURLDOWNLOAD_PAS_R}
{$define NSURLDOWNLOAD_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSURLDOWNLOAD_PAS_F}
{$define NSURLDOWNLOAD_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSURLDOWNLOAD_PAS_T}
{$define NSURLDOWNLOAD_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSURLDownload = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSURLDOWNLOAD_PAS_C}
{$define NSURLDOWNLOAD_PAS_C}

{ NSURLDownload }
  NSURLDownload = objcclass(NSObject)
  private
    __internal: NSURLDownloadInternal;
    
  public
    class function alloc: NSURLDownload; message 'alloc';

    class function canResumeDownloadDecodedWithEncodingMIMEType(MIMEType: NSString): Boolean; message 'canResumeDownloadDecodedWithEncodingMIMEType:';
    function initWithRequest_delegate(request_: NSURLRequest; delegate: id): id; message 'initWithRequest:delegate:';
    function initWithResumeData_delegate_path(resumeData_: NSData; delegate: id; path: NSString): id; message 'initWithResumeData:delegate:path:';
    procedure cancel; message 'cancel';
    procedure setDestination_allowOverwrite(path: NSString; allowOverwrite: Boolean); message 'setDestination:allowOverwrite:';
    function request: NSURLRequest; message 'request';
    function resumeData: NSData; message 'resumeData';
    procedure setDeletesFileUponFailure(deletesFileUponFailure_: Boolean); message 'setDeletesFileUponFailure:';
    function deletesFileUponFailure: Boolean; message 'deletesFileUponFailure';
  end; external;

{$endif}
{$endif}
