{ Parsed from PubSub.framework PSEnclosure.h }


{ Types from PSEnclosure }
{$ifdef TYPES}

const
  PSEnclosureDownloadIsIdle = 0 deprecated 'in 10_5, 10_9, NA, NA';
  PSEnclosureDownloadIsQueued = 1 deprecated 'in 10_5, 10_9, NA, NA';
  PSEnclosureDownloadIsActive = 2 deprecated 'in 10_5, 10_9, NA, NA';
  PSEnclosureDownloadDidFinish = 3 deprecated 'in 10_5, 10_9, NA, NA';
  PSEnclosureDownloadDidFail = 4 deprecated 'in 10_5, 10_9, NA, NA';
  PSEnclosureDownloadWasDeleted = 5 deprecated 'in 10_5, 10_9, NA, NA';
type
  PSEnclosureDownloadState = clong;
{$endif}


{$ifdef TYPES}
type
  PSEnclosurePtr = ^PSEnclosure;
{$endif}

{$if (MAC_OS_X_VERSION_10_5 <= MAC_OS_X_VERSION_MAX_ALLOWED)}
{$ifdef CLASSES}

type
  PSEnclosure = objcclass external (NSObject)
  private
    _internal: id;
  public
    function entry: PSEntry; message 'entry';
    function URL: NSURL; message 'URL';
    function MIMEType: NSString; message 'MIMEType';
    function length: clonglong; message 'length';
    function downloadState: PSEnclosureDownloadState; message 'downloadState';
    function downloadProgress: double; message 'downloadProgress';
    function downloadError: NSError; message 'downloadError';
    function downloadedPath: NSString; message 'downloadedPath';
    function download (error: NSErrorPtr): ObjCBOOL; message 'download:';
    procedure cancelDownload; message 'cancelDownload';
    procedure deleteDownload; message 'deleteDownload';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  PSEnclosureDownloadStateDidChangeNotification: NSString deprecated 'in 10_5, 10_9, NA, NA'; cvar; external;
{$endif}

{$endif}
