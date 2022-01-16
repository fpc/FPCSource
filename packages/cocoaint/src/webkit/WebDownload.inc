{ Parsed from WebKit.framework WebDownload.h }


{$ifdef TYPES}
type
  WebDownloadPtr = ^WebDownload;
{$endif}

{$ifdef CLASSES}

type
  WebDownload = objcclass external (NSURLDownload)
  private
    _webInternal: WebDownloadInternal;
  end;


type
  WebDownloadDelegate = objccategory external (NSObject)
    function downloadWindowForAuthenticationSheet (download: WebDownload): NSWindow; message 'downloadWindowForAuthenticationSheet:';
  end;
{$endif}

