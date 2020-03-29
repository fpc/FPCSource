{ Parsed from InstantMessage.framework IMAVManager.h }


{ Types from IMAVManager }
{$ifdef TYPES}

{$endif}


{$ifdef TYPES}
type
  IMAVManagerPtr = ^IMAVManager;
{$endif}

{$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5)}
{$ifdef EXTERNAL_SYMBOLS}
var
  IMAVManagerStateChangedNotification: NSString deprecated 'in 10_5, 10_9'; cvar; external;
  IMAVManagerURLToShareChangedNotification: NSString deprecated 'in 10_5, 10_9'; cvar; external;
{$endif}

{$ifdef TYPES}
const
  IMAVInactive = 0;
  IMAVRequested = 1;
  IMAVShuttingDown = 2;
  IMAVStartingUp = 3;
  IMAVPending = 4;
  IMAVRunning = 5;

type
  IMAVManagerState = NSUInteger deprecated 'in 10_5, 10_9';
  IMAVManagerStatePtr = ^IMAVManagerState deprecated 'in 10_5, 10_9';

const
  IMVideoOptimizationDefault = 0;
  IMVideoOptimizationStills = 1 shl 0;
  IMVideoOptimizationReplacement = 1 shl 1;

type
  IMVideoOptimizationOptions = NSUInteger deprecated 'in 10_5, 10_9';
  IMVideoOptimizationOptionsPtr = ^IMVideoOptimizationOptions deprecated 'in 10_5, 10_9';
{$endif}

{$ifdef CLASSES}

type
  IMAVManager = objcclass external (NSObject)
  private
  public
    class function sharedAVManager: IMAVManager; message 'sharedAVManager'; deprecated 'in 10_5, 10_9';
    function state: IMAVManagerState; message 'state'; deprecated 'in 10_5, 10_9';
    function URLToShare: NSURL; message 'URLToShare'; deprecated 'in 10_5, 10_9';
    procedure setVideoDataSource (dataSource: id); message 'setVideoDataSource:'; deprecated 'in 10_5, 10_9';
    function videoDataSource: id; message 'videoDataSource'; deprecated 'in 10_5, 10_9';
    procedure setVideoOptimizationOptions (options: IMVideoOptimizationOptions); message 'setVideoOptimizationOptions:'; deprecated 'in 10_5, 10_9';
    function videoOptimizationOptions: IMVideoOptimizationOptions; message 'videoOptimizationOptions'; deprecated 'in 10_5, 10_9';
    procedure setNumberOfAudioChannels (count: NSInteger); message 'setNumberOfAudioChannels:'; deprecated 'in 10_5, 10_9';
    function numberOfAudioChannels: NSInteger; message 'numberOfAudioChannels'; deprecated 'in 10_5, 10_9';
    function audioDeviceUID: NSString; message 'audioDeviceUID'; deprecated 'in 10_5, 10_9';
    function audioDeviceChannels: NSArray; message 'audioDeviceChannels'; deprecated 'in 10_5, 10_9';
    {$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_6)}
    function controlBar: IMAVControlBar; message 'controlBar'; deprecated 'in 10_5, 10_9';
    {$endif}
    procedure start; message 'start'; deprecated 'in 10_5, 10_9';
    procedure stop; message 'stop'; deprecated 'in 10_5, 10_9';
  end;


type
  IMVideoDataSource = objccategory external (NSObject)
    procedure getPixelBufferPixelFormat (pixelFormatOut: OSTypePtr); message 'getPixelBufferPixelFormat:'; deprecated 'in 10_5, 10_9';
    function renderIntoPixelBuffer_forTime (buffer: CVPixelBufferRef; timeStamp: CVTimeStampPtr): ObjCBOOL; message 'renderIntoPixelBuffer:forTime:'; deprecated 'in 10_5, 10_9';
    procedure getOpenGLBufferContext_pixelFormat (contextOut: CGLContextObjPtr; pixelFormatOut: CGLPixelFormatObjPtr); message 'getOpenGLBufferContext:pixelFormat:'; deprecated 'in 10_5, 10_9';
    function renderIntoOpenGLBuffer_onScreen_forTime (buffer: CVOpenGLBufferRef; screenInOut: pcint; timeStamp: CVTimeStampPtr): ObjCBOOL; message 'renderIntoOpenGLBuffer:onScreen:forTime:'; deprecated 'in 10_5, 10_9';
  end;
{$endif}

{$endif}
