{ Parsed from Appkit.framework NSProgressIndicator.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:07 ICT 2009 }


{$ifdef TYPES}
{$ifndef NSPROGRESSINDICATOR_PAS_T}
{$define NSPROGRESSINDICATOR_PAS_T}

{ Types }
type
  __NSProgressIndicatorThreadInfo = Pointer;
  _NSProgressIndicatorThreadInfo = __NSProgressIndicatorThreadInfo;
  NSProgressIndicatorThickness = NSUInteger;
  NSProgressIndicatorStyle = NSUInteger;

{ Constants }

const
  NSProgressIndicatorPreferredThickness = 14;
  NSProgressIndicatorPreferredSmallThickness = 10;
  NSProgressIndicatorPreferredLargeThickness = 18;
  NSProgressIndicatorPreferredAquaThickness = 12;

const
  NSProgressIndicatorBarStyle = 0;
  NSProgressIndicatorSpinningStyle = 1;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSPROGRESSINDICATOR_PAS_R}
{$define NSPROGRESSINDICATOR_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSPROGRESSINDICATOR_PAS_F}
{$define NSPROGRESSINDICATOR_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSPROGRESSINDICATOR_PAS_S}
{$define NSPROGRESSINDICATOR_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSProgressIndicator = objcclass;
  NSProgressIndicatorPointer = ^NSProgressIndicator;

{$endif}

{$ifdef CLASSES}
{$ifndef NSPROGRESSINDICATOR_PAS_C}
{$define NSPROGRESSINDICATOR_PAS_C}

{ NSProgressIndicator }
  NSProgressIndicator = objcclass(NSView)
  private
    __isBezeled: Boolean;
    __isIndeterminate: Boolean;
    __threadedAnimation: Boolean;
    __minimum: double;
    __maximum: double;
    __value: double;
    __animationIndex: cint;
    __animationDelay: NSTimeInterval;
    __timer: id;
    __animationThreadLock: id;
    __cachedImage: id;
    __cachedImageLock: id;
    _isSpinning: 0..1;
    _isVector: 0..1;
    _isLocked: 0..1;
    _controlTint: 0..((1 shl 3)-1);
    _controlSize: 0..((1 shl 2)-1);
    _style: 0..1;
    __delayedStartup: 0..1;
    __orderOutForResize: 0..1;
    _hideWhenStopped: 0..1;
    _revive: 0..1;
    _RESERVED: 0..((1 shl 19)-1);
    __progressIndicatorFlags: record
      end;
    __NSProgressIndicatorReserved1: id;
    
  public
    class function alloc: NSProgressIndicator; message 'alloc';

    function isIndeterminate: Boolean; message 'isIndeterminate';
    procedure setIndeterminate(flag: Boolean); message 'setIndeterminate:';
    function isBezeled: Boolean; message 'isBezeled';
    procedure setBezeled(flag: Boolean); message 'setBezeled:';
    function controlTint: NSControlTint; message 'controlTint';
    procedure setControlTint(tint: NSControlTint); message 'setControlTint:';
    function controlSize: NSControlSize; message 'controlSize';
    procedure setControlSize(size: NSControlSize); message 'setControlSize:';
    function doubleValue: double; message 'doubleValue';
    procedure setDoubleValue(doubleValue_: double); message 'setDoubleValue:';
    procedure incrementBy(delta: double); message 'incrementBy:';
    function minValue: double; message 'minValue';
    function maxValue: double; message 'maxValue';
    procedure setMinValue(newMinimum: double); message 'setMinValue:';
    procedure setMaxValue(newMaximum: double); message 'setMaxValue:';
    function animationDelay: NSTimeInterval; message 'animationDelay';
    procedure setAnimationDelay(delay: NSTimeInterval); message 'setAnimationDelay:';
    function usesThreadedAnimation: Boolean; message 'usesThreadedAnimation';
    procedure setUsesThreadedAnimation(threadedAnimation: Boolean); message 'setUsesThreadedAnimation:';
    procedure startAnimation(sender: id); message 'startAnimation:';
    procedure stopAnimation(sender: id); message 'stopAnimation:';
    procedure animate(sender: id); message 'animate:';
    procedure setStyle(style_: NSProgressIndicatorStyle); message 'setStyle:';
    function style: NSProgressIndicatorStyle; message 'style';
    procedure sizeToFit; message 'sizeToFit';
    function isDisplayedWhenStopped: Boolean; message 'isDisplayedWhenStopped';
    procedure setDisplayedWhenStopped(isDisplayed: Boolean); message 'setDisplayedWhenStopped:';
  end; external;

{$endif}
{$endif}
