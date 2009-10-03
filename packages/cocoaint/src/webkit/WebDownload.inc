{ Parsed from Webkit.framework WebDownload.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 3 15:33:51 ICT 2009 }


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

{$endif}

{$ifdef CLASSES}
{$ifndef WEBDOWNLOAD_PAS_C}
{$define WEBDOWNLOAD_PAS_C}

{ WebDownload }
  WebDownload = objcclass(NSURLDownload)
  private
    __webInternal: WebDownloadInternal;
    
  public
    class function alloc: WebDownload; message 'alloc';
  end; external;

{$endif}
{$endif}
