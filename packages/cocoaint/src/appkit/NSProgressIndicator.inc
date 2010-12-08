{ Parsed from Appkit.framework NSProgressIndicator.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:06 CET 2010 }


{$ifdef TYPES}
{$ifndef NSPROGRESSINDICATOR_PAS_T}
{$define NSPROGRESSINDICATOR_PAS_T}

{ Types }
type
  __NSProgressIndicatorThreadInfo = record end;
  _NSProgressIndicatorThreadInfo = __NSProgressIndicatorThreadInfo;
  _NSProgressIndicatorThreadInfoPtr = ^_NSProgressIndicatorThreadInfo;
  NSProgressIndicatorThickness = NSUInteger;
  NSProgressIndicatorThicknessPtr = ^NSProgressIndicatorThickness;
  NSProgressIndicatorStyle = NSUInteger;
  NSProgressIndicatorStylePtr = ^NSProgressIndicatorStyle;

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
  NSProgressIndicatorPtr = NSProgressIndicatorPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSPROGRESSINDICATOR_PAS_C}
{$define NSPROGRESSINDICATOR_PAS_C}

{ NSProgressIndicator }
  NSProgressIndicator = objcclass(NSView)
  private
    _isBezeled: Boolean;
    _isIndeterminate: Boolean;
    _threadedAnimation: Boolean;
    _minimum: double;
    _maximum: double;
    _value: double;
    _animationIndex: cint;
    _animationDelay: NSTimeInterval;
    _timer: id;
    _drawingWidth: CGFloat;
    _roundColor: id;
    _reserved: id;
     {garbage collector: volatile }
    __progressIndicatorFlags: bitpacked record
      case byte of
       0: (_anonbitfield_NSProgressIndicator0: cuint);
       1: (
        isSpinning: 0..1;
        isVector: 0..1;
        isLocked: 0..1;
        _controlTint: 0..((1 shl 3)-1);
        _controlSize: 0..((1 shl 2)-1);
        _style: 0..1;
        _delayedStartup: 0..1;
        _orderOutForResize: 0..1;
        hideWhenStopped: 0..1;
        revive: 0..1;
        _temporarilyBlockHeartBeating: 0..1;
        _isHidden: 0..1;
        _isHeartBeatInstalled: 0..1;
        _lastFrame: 0..((1 shl 8)-1);
        RESERVED: 0..((1 shl 8)-1);
      );
      end;
    _NSProgressIndicatorReserved1: id;
    
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
    function usesThreadedAnimation: Boolean; message 'usesThreadedAnimation';
    procedure setUsesThreadedAnimation(threadedAnimation: Boolean); message 'setUsesThreadedAnimation:';
    procedure startAnimation(sender: id); message 'startAnimation:';
    procedure stopAnimation(sender: id); message 'stopAnimation:';
    procedure setStyle(style_: NSProgressIndicatorStyle); message 'setStyle:';
    function style: NSProgressIndicatorStyle; message 'style';
    procedure sizeToFit; message 'sizeToFit';
    function isDisplayedWhenStopped: Boolean; message 'isDisplayedWhenStopped';
    procedure setDisplayedWhenStopped(isDisplayed: Boolean); message 'setDisplayedWhenStopped:';
  end; external;

{ NSProgressIndicatorDeprecatedCategory }
  NSProgressIndicatorDeprecatedCategory = objccategory(NSProgressIndicator)
    function animationDelay: NSTimeInterval; message 'animationDelay';
    procedure setAnimationDelay(delay: NSTimeInterval); message 'setAnimationDelay:';
    procedure animate(sender: id); message 'animate:';
  end; external;

{$endif}
{$endif}
