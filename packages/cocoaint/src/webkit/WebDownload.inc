{ Parsed from Webkit.framework WebDownload.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:27 CET 2010 }


{$ifdef TYPES}
{$ifndef WEBDOWNLOAD_PAS_T}
{$define WEBDOWNLOAD_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef WEBDOWNLOAD_PAS_R}
{$define WEBDOWNLOAD_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef WEBDOWNLOAD_PAS_F}
{$define WEBDOWNLOAD_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef WEBDOWNLOAD_PAS_S}
{$define WEBDOWNLOAD_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  WebDownload = objcclass;
  WebDownloadPointer = ^WebDownload;
  WebDownloadPtr = WebDownloadPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef WEBDOWNLOAD_PAS_C}
{$define WEBDOWNLOAD_PAS_C}

{ WebDownload }
  WebDownload = objcclass(NSURLDownload)
  private
    _webInternal: WebDownloadInternal;
    
  public
    class function alloc: WebDownload; message 'alloc';
  end; external;

{ WebDownloadDelegateCategory }
  WebDownloadDelegateCategory = objccategory(NSObject)
    function downloadWindowForAuthenticationSheet(download: WebDownload): NSWindowPtr; message 'downloadWindowForAuthenticationSheet:';
  end; external;

{$endif}
{$endif}
