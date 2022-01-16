{ Parsed from QTKit.framework QTCaptureView.h }


{$ifdef TYPES}
type
  QTCaptureViewPtr = ^QTCaptureView;
{$endif}

{$if ((QTKIT_VERSION_MAX_ALLOWED >= QTKIT_VERSION_7_2)) and ((MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4))}
{$ifdef CLASSES}

type
  QTCaptureView = objcclass external (NSView)
  private
    _internal: QTCaptureViewInternal;
    _reserved1: clong;
    _reserved2: clong;
    _reserved3: clong;
  public
    function captureSession: QTCaptureSession; message 'captureSession';
    procedure setCaptureSession (captureSession: QTCaptureSession); message 'setCaptureSession:';
    function availableVideoPreviewConnections: NSArray; message 'availableVideoPreviewConnections';
    function videoPreviewConnection: QTCaptureConnection; message 'videoPreviewConnection';
    procedure setVideoPreviewConnection (videoPreviewConnection: QTCaptureConnection); message 'setVideoPreviewConnection:';
    function fillColor: NSColor; message 'fillColor';
    procedure setFillColor (fillColor: NSColor); message 'setFillColor:';
    function preservesAspectRatio: ObjCBOOL; message 'preservesAspectRatio';
    procedure setPreservesAspectRatio (preservesAspectRatio: ObjCBOOL); message 'setPreservesAspectRatio:';
    function previewBounds: NSRect; message 'previewBounds';
    function delegate: id; message 'delegate';
    procedure setDelegate (delegate: id); message 'setDelegate:';
  end;


type
  QTCaptureView_Delegate = objccategory external (NSObject)
    function view_willDisplayImage (view: QTCaptureView; image: CIImage): CIImage; message 'view:willDisplayImage:';
  end;
{$endif}

{$endif}
