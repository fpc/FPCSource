{ Parsed from AVKit.framework AVCaptureView.h }


{ Types from AVCaptureView }
{$ifdef TYPES}

type
  AVCaptureViewControlsStyle = NSInteger;
type
  AVCaptureViewControlsStylePtr = ^AVCaptureViewControlsStyle;

const
  AVCaptureViewControlsStyleInline = 0;
  AVCaptureViewControlsStyleFloating = 1;
  AVCaptureViewControlsStyleInlineDeviceSelection = 2;
  AVCaptureViewControlsStyleDefault = AVCaptureViewControlsStyleInline;
{$endif}


{$ifdef TYPES}
type
  AVCaptureViewPtr = ^AVCaptureView;
  AVCaptureViewDelegateProtocolPtr = ^AVCaptureViewDelegateProtocol;
{$endif}

{$ifdef CLASSES}

type
  AVCaptureView = objcclass external (NSView)
  public
    function session: AVCaptureSession; message 'session';
    procedure setSession_showVideoPreview_showAudioPreview (session_: AVCaptureSession; showVideoPreview: ObjCBOOL; showAudioPreview: ObjCBOOL); message 'setSession:showVideoPreview:showAudioPreview:';
    function fileOutput: AVCaptureFileOutput; message 'fileOutput';
    procedure setDelegate(newValue: AVCaptureViewDelegateProtocol); message 'setDelegate:';
    function delegate: AVCaptureViewDelegateProtocol; message 'delegate';
    procedure setControlsStyle(newValue: AVCaptureViewControlsStyle); message 'setControlsStyle:';
    function controlsStyle: AVCaptureViewControlsStyle; message 'controlsStyle';
    procedure setVideoGravity(newValue: NSString); message 'setVideoGravity:';
    function videoGravity: NSString; message 'videoGravity';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  AVCaptureViewDelegateProtocol = objcprotocol external name 'AVCaptureViewDelegate' (NSObjectProtocol)
    procedure captureView_startRecordingToFileOutput (captureView: AVCaptureView; fileOutput: AVCaptureFileOutput); message 'captureView:startRecordingToFileOutput:';
  end;
{$endif}

