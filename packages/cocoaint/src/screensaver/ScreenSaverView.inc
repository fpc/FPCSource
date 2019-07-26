{ Parsed from ScreenSaver.framework ScreenSaverView.h }


{$ifdef TYPES}
type
  ScreenSaverViewPtr = ^ScreenSaverView;
{$endif}

{$ifdef CLASSES}

type
  ScreenSaverView = objcclass external (NSView)
  private
    _animationTimer: NSTimer;
    _timeInterval: NSTimeInterval;
    _isPreview: ObjCBOOL;
    _reserved1: pointer;
    _reserved2: pointer;
    _reserved3: pointer;
  public
    class function backingStoreType: NSBackingStoreType; message 'backingStoreType';
    class function performGammaFade: ObjCBOOL; message 'performGammaFade';
    function initWithFrame (frame: NSRect): id; message 'initWithFrame:';
    function initWithFrame_isPreview (frame: NSRect; isPreview: ObjCBOOL): id; message 'initWithFrame:isPreview:';
    function animationTimeInterval: NSTimeInterval; message 'animationTimeInterval';
    procedure setAnimationTimeInterval (timeInterval: NSTimeInterval); message 'setAnimationTimeInterval:';
    procedure startAnimation; message 'startAnimation';
    procedure stopAnimation; message 'stopAnimation';
    function isAnimating: ObjCBOOL; message 'isAnimating';
    procedure drawRect (rect: NSRect); message 'drawRect:';
    procedure animateOneFrame; message 'animateOneFrame';
    function hasConfigureSheet: ObjCBOOL; message 'hasConfigureSheet';
    function configureSheet: NSWindow; message 'configureSheet';
    function isPreview: ObjCBOOL; message 'isPreview';
  end;
{$endif}

