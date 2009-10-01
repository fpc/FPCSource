{ Parsed from Appkit.framework NSMovieView.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:07 ICT 2009 }


{$ifdef TYPES}
{$ifndef NSMOVIEVIEW_PAS_T}
{$define NSMOVIEVIEW_PAS_T}

{ Constants }

const
  NSQTMovieNormalPlayback = 0;
  NSQTMovieLoopingPlayback = 1;
  NSQTMovieLoopingBackAndForthPlayback = 2;

{ Types }
type
  NSQTMovieLoopMode = NSUInteger;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSMOVIEVIEW_PAS_R}
{$define NSMOVIEVIEW_PAS_R}

{ Records }
type
  __MVFlags = bitpacked record
    editable: 0..1;
    loopMode: 0..((1 shl 3)-1);
    playsEveryFrame: 0..1;
    playsSelectionOnly: 0..1;
    controllerVisible: 0..1;
    reserved: 0..((1 shl 25)-1);
  end;
_MVFlags = __MVFlags;


{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSMOVIEVIEW_PAS_F}
{$define NSMOVIEVIEW_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSMOVIEVIEW_PAS_S}
{$define NSMOVIEVIEW_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSMovieView = objcclass;
  NSMovieViewPointer = ^NSMovieView;

{$endif}

{$ifdef CLASSES}
{$ifndef NSMOVIEVIEW_PAS_C}
{$define NSMOVIEVIEW_PAS_C}

{ NSMovieView }
  NSMovieView = objcclass(NSView, NSUserInterfaceValidationsProtocol)
  private
    __fMovie: NSMovie;
    __fRate: single;
    __fVolume: single;
    __fFlags: _MVFlags;
    __fAuxData: Pointer;
    __fReserved1: culong;
    __fReserved2: culong;
    __fReserved3: culong;
    
  public
    class function alloc: NSMovieView; message 'alloc';

    procedure setMovie(movie_: NSMovie); message 'setMovie:';
    function movie: NSMovie; message 'movie';
    function movieController: Pointer; message 'movieController';
    function movieRect: NSRect; message 'movieRect';
    procedure start(sender: id); message 'start:';
    procedure stop(sender: id); message 'stop:';
    function isPlaying: Boolean; message 'isPlaying';
    procedure gotoPosterFrame(sender: id); message 'gotoPosterFrame:';
    procedure gotoBeginning(sender: id); message 'gotoBeginning:';
    procedure gotoEnd(sender: id); message 'gotoEnd:';
    procedure stepForward(sender: id); message 'stepForward:';
    procedure stepBack(sender: id); message 'stepBack:';
    procedure setRate(rate_: single); message 'setRate:';
    function rate: single; message 'rate';
    procedure setVolume(volume_: single); message 'setVolume:';
    function volume: single; message 'volume';
    procedure setMuted(mute: Boolean); message 'setMuted:';
    function isMuted: Boolean; message 'isMuted';
    procedure setLoopMode(mode: NSQTMovieLoopMode); message 'setLoopMode:';
    function loopMode: NSQTMovieLoopMode; message 'loopMode';
    procedure setPlaysSelectionOnly(flag: Boolean); message 'setPlaysSelectionOnly:';
    function playsSelectionOnly: Boolean; message 'playsSelectionOnly';
    procedure setPlaysEveryFrame(flag: Boolean); message 'setPlaysEveryFrame:';
    function playsEveryFrame: Boolean; message 'playsEveryFrame';
    procedure showController_adjustingSize(show: Boolean; adjustSize: Boolean); message 'showController:adjustingSize:';
    function isControllerVisible: Boolean; message 'isControllerVisible';
    procedure resizeWithMagnification(magnification: CGFloat); message 'resizeWithMagnification:';
    function sizeForMagnification(magnification: CGFloat): NSSize; message 'sizeForMagnification:';
    procedure setEditable(editable: Boolean); message 'setEditable:';
    function isEditable: Boolean; message 'isEditable';
    procedure cut(sender: id); message 'cut:';
    procedure copy_(sender: id); message 'copy:';
    procedure paste(sender: id); message 'paste:';
    procedure delete(sender: id); message 'delete:';
    procedure selectAll(sender: id); message 'selectAll:';
    procedure clear(sender: id); message 'clear:';
  end; external;

{$endif}
{$endif}
