{ Parsed from AppKit.framework NSMovieView.h }


{$ifdef TYPES}
type
  NSMovieViewPtr = ^NSMovieView;
{$endif}

{$if  not defined(__LP64__)}
{$ifdef TYPES}
const
  NSQTMovieNormalPlayback = 0;
  NSQTMovieLoopingPlayback = 1;
  NSQTMovieLoopingBackAndForthPlayback = 2;

type
  NSQTMovieLoopMode = NSUInteger;
  NSQTMovieLoopModePtr = ^NSQTMovieLoopMode;

type
  __MVFlags = bitpacked record
    case byte of
      0: (_anonBitField___MVFlags0: cuint);
      1: (
        editable: 0..1;
        loopMode: 0..((1 shl 3)-1);
        playsEveryFrame: 0..1;
        playsSelectionOnly: 0..1;
        controllerVisible: 0..1;
        reserved: 0..((1 shl 25)-1);
      );
    end;
type
  _MVFlags = __MVFlags;
type
  _MVFlagsPtr = ^__MVFlags;
type
  __MVFlagsPtr = ^__MVFlags;
{$endif}

{$ifdef CLASSES}

type
  NSMovieView = objcclass external (NSView, NSUserInterfaceValidationsProtocol)
  private
    _fMovie: NSMovie;
    _fRate: single;
    _fVolume: single;
    _fFlags: _MVFlags;
    _fAuxData: pointer;
    _fReserved1: culong;
    _fReserved2: culong;
    _fReserved3: culong;
  public
    procedure setMovie (movie: NSMovie); message 'setMovie:'; deprecated 'in 10_0, 10_5';
    function movie: NSMovie; message 'movie'; deprecated 'in 10_0, 10_5';
    function movieController: pointer; message 'movieController'; deprecated 'in 10_0, 10_5';
    function movieRect: NSRect; message 'movieRect'; deprecated 'in 10_0, 10_5';
    procedure start (sender: id); message 'start:'; deprecated 'in 10_0, 10_5';
    procedure stop (sender: id); message 'stop:'; deprecated 'in 10_0, 10_5';
    function isPlaying: ObjCBOOL; message 'isPlaying'; deprecated 'in 10_0, 10_5';
    procedure gotoPosterFrame (sender: id); message 'gotoPosterFrame:'; deprecated 'in 10_0, 10_5';
    procedure gotoBeginning (sender: id); message 'gotoBeginning:'; deprecated 'in 10_0, 10_5';
    procedure gotoEnd (sender: id); message 'gotoEnd:'; deprecated 'in 10_0, 10_5';
    procedure stepForward (sender: id); message 'stepForward:'; deprecated 'in 10_0, 10_5';
    procedure stepBack (sender: id); message 'stepBack:'; deprecated 'in 10_0, 10_5';
    procedure setRate (rate: single); message 'setRate:'; deprecated 'in 10_0, 10_5';
    function rate: single; message 'rate'; deprecated 'in 10_0, 10_5';
    procedure setVolume (volume: single); message 'setVolume:'; deprecated 'in 10_0, 10_5';
    function volume: single; message 'volume'; deprecated 'in 10_0, 10_5';
    procedure setMuted (mute: ObjCBOOL); message 'setMuted:'; deprecated 'in 10_0, 10_5';
    function isMuted: ObjCBOOL; message 'isMuted'; deprecated 'in 10_0, 10_5';
    procedure setLoopMode (mode: NSQTMovieLoopMode); message 'setLoopMode:'; deprecated 'in 10_0, 10_5';
    function loopMode: NSQTMovieLoopMode; message 'loopMode'; deprecated 'in 10_0, 10_5';
    procedure setPlaysSelectionOnly (flag: ObjCBOOL); message 'setPlaysSelectionOnly:'; deprecated 'in 10_0, 10_5';
    function playsSelectionOnly: ObjCBOOL; message 'playsSelectionOnly'; deprecated 'in 10_0, 10_5';
    procedure setPlaysEveryFrame (flag: ObjCBOOL); message 'setPlaysEveryFrame:'; deprecated 'in 10_0, 10_5';
    function playsEveryFrame: ObjCBOOL; message 'playsEveryFrame'; deprecated 'in 10_0, 10_5';
    procedure showController_adjustingSize (show: ObjCBOOL; adjustSize: ObjCBOOL); message 'showController:adjustingSize:'; deprecated 'in 10_0, 10_5';
    function isControllerVisible: ObjCBOOL; message 'isControllerVisible'; deprecated 'in 10_0, 10_5';
    procedure resizeWithMagnification (magnification: CGFloat); message 'resizeWithMagnification:'; deprecated 'in 10_0, 10_5';
    function sizeForMagnification (magnification: CGFloat): NSSize; message 'sizeForMagnification:'; deprecated 'in 10_0, 10_5';
    procedure setEditable (editable: ObjCBOOL); message 'setEditable:'; deprecated 'in 10_0, 10_5';
    function isEditable: ObjCBOOL; message 'isEditable'; deprecated 'in 10_0, 10_5';
    procedure cut (sender: id); message 'cut:'; deprecated 'in 10_0, 10_5';
    procedure copy_ (sender: id); message 'copy:'; deprecated 'in 10_0, 10_5';
    procedure paste (sender: id); message 'paste:'; deprecated 'in 10_0, 10_5';
    procedure delete (sender: id); message 'delete:'; deprecated 'in 10_3, 10_5';
    procedure selectAll (sender: id); message 'selectAll:'; deprecated 'in 10_0, 10_5';
    procedure clear (sender: id); message 'clear:'; deprecated 'in 10_0, 10_5';

    { Adopted protocols }
    function validateUserInterfaceItem (anItem: NSValidatedUserInterfaceItemProtocol): ObjCBOOL; message 'validateUserInterfaceItem:';
  end;
{$endif}

{$endif}
