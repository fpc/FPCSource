{ Parsed from QTKit.framework QTMovieModernizer.h }


{$ifdef TYPES}
type
  QTMovieModernizerPtr = ^QTMovieModernizer;
{$endif}

{$if ((QTKIT_VERSION_MAX_ALLOWED >= QTKIT_VERSION_7_7_3)) and ((MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_9))}
{$ifdef TYPES}
type
  QTMovieModernizerStatus = NSInteger;
  QTMovieModernizerStatusPtr = ^QTMovieModernizerStatus;

const
  QTMovieModernizerStatusUnknown = 0;
  QTMovieModernizerStatusPreparing = 1;
  QTMovieModernizerStatusRunning = 2;
  QTMovieModernizerStatusCancelled = 3;
  QTMovieModernizerStatusFailed = 4;
  QTMovieModernizerStatusCompletedWithSuccess = 5;
  QTMovieModernizerStatusNotRequired = 6;
{$endif}

{$if ((defined(MAC_OS_X_VERSION_10_9)) and ((MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_9)))}
{$ifdef CLASSES}

type
  QTMovieModernizer = objcclass external (NSObject)
  private
    _internal: QTMovieModernizerInternal;
  public
    function initWithSourceURL_destinationURL (source: NSURL; destination: NSURL): id; message 'initWithSourceURL:destinationURL:';
    class function requiresModernization_error (source: NSURL; outError: NSErrorPtr): ObjCBOOL; message 'requiresModernization:error:';
    procedure modernizeWithCompletionHandler (handler: OpaqueCBlock); message 'modernizeWithCompletionHandler:';
    procedure cancel; message 'cancel';
    {function AVAILABLE_QTKIT_VERSION_7_7_3_AND_LATER: NSURL *; message 'AVAILABLE_QTKIT_VERSION_7_7_3_AND_LATER';
    function AVAILABLE_QTKIT_VERSION_7_7_3_AND_LATER_: NSURL *destinationURL; message 'AVAILABLE_QTKIT_VERSION_7_7_3_AND_LATER';
    procedure setAVAILABLE_QTKIT_VERSION_7_7_3_AND_LATER(newValue: NSString *outputFormat); message 'setAVAILABLE_QTKIT_VERSION_7_7_3_AND_LATER:';
    function AVAILABLE_QTKIT_VERSION_7_7_3_AND_LATER__: NSString *outputFormat; message 'AVAILABLE_QTKIT_VERSION_7_7_3_AND_LATER';
    function AVAILABLE_QTKIT_VERSION_7_7_3_AND_LATER___: NSError *error; message 'AVAILABLE_QTKIT_VERSION_7_7_3_AND_LATER';
    procedure setAVAILABLE_QTKIT_VERSION_7_7_3_AND_LATER_(newValue: QTMovieModernizerStatus status); message 'setAVAILABLE_QTKIT_VERSION_7_7_3_AND_LATER:';
    function AVAILABLE_QTKIT_VERSION_7_7_3_AND_LATER____: QTMovieModernizerStatus status; message 'AVAILABLE_QTKIT_VERSION_7_7_3_AND_LATER';}
  end;
{$endif}

{$endif}
{$endif}
