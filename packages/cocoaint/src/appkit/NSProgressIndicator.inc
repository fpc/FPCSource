{ Parsed from AppKit.framework NSProgressIndicator.h }


{$ifdef TYPES}
type
  NSProgressIndicatorPtr = ^NSProgressIndicator;
{$endif}

{$ifdef TYPES}
type
  _NSProgressIndicatorThreadInfo = OpaqueType;
  _NSProgressIndicatorThreadInfoPtr = ^_NSProgressIndicatorThreadInfo;
  NSProgressIndicatorThickness = NSUInteger;
  NSProgressIndicatorThicknessPtr = ^NSProgressIndicatorThickness;

const
  NSProgressIndicatorPreferredThickness = 14;
  NSProgressIndicatorPreferredSmallThickness = 10;
  NSProgressIndicatorPreferredLargeThickness = 18;
  NSProgressIndicatorPreferredAquaThickness = 12;

type
  NSProgressIndicatorStyle = NSUInteger;
  NSProgressIndicatorStylePtr = ^NSProgressIndicatorStyle;

const
  NSProgressIndicatorBarStyle = 0;
  NSProgressIndicatorSpinningStyle = 1;
{$endif}

{$ifdef CLASSES}

type
  NSProgressIndicator = objcclass external (NSView, NSAccessibilityProgressIndicatorProtocol)
  private
    _isBezeled: ObjCBOOL;
    _isIndeterminate: ObjCBOOL;
    _threadedAnimation: ObjCBOOL;
    _minimum: double;
    _maximum: double;
    _value: double;
    _animationIndex: cuint;
    _animationDelay: NSTimeInterval;
    _timer: id;
    _drawingWidth: CGFloat;
    _roundColor: id;
    _reserved: id;
    __progressIndicatorFlags: bitpacked record
      case byte of
        0: (_anonBitField___progressIndicatorFlags0: cuint);
        1: (
          isSpinning: 0..1;
          isVector: 0..1;
          isLocked: 0..1;
          controlTint: 0..((1 shl 3)-1);
          controlSize: 0..((1 shl 2)-1);
          style: 0..1;
          _delayedStartup: 0..1;
          hideWhenStopped: 0..1;
          revive: 0..1;
          _temporarilyBlockHeartBeating: 0..1;
          _isHidden: 0..1;
          _isHeartBeatInstalled: 0..1;
          _customRenderer: 0..1;
          _lastFrame: 0..((1 shl 8)-1);
          _isDetaching: 0..1;
          RESERVED: 0..((1 shl 7)-1);
        );
      end;
    _NSProgressIndicatorReserved1: id;
  public
    procedure setIndeterminate(newValue: ObjCBOOL); message 'setIndeterminate:';
    function isIndeterminate: ObjCBOOL; message 'isIndeterminate';
    procedure setBezeled(newValue: ObjCBOOL); message 'setBezeled:';
    function isBezeled: ObjCBOOL; message 'isBezeled';
    procedure setControlTint(newValue: NSControlTint); message 'setControlTint:';
    function controlTint: NSControlTint; message 'controlTint';
    procedure setControlSize(newValue: NSControlSize); message 'setControlSize:';
    function controlSize: NSControlSize; message 'controlSize';
    procedure setDoubleValue(newValue: double); message 'setDoubleValue:';
    function doubleValue: double; message 'doubleValue';
    procedure incrementBy (delta: double); message 'incrementBy:';
    procedure setMinValue(newValue: double); message 'setMinValue:';
    function minValue: double; message 'minValue';
    procedure setMaxValue(newValue: double); message 'setMaxValue:';
    function maxValue: double; message 'maxValue';
    procedure setUsesThreadedAnimation(newValue: ObjCBOOL); message 'setUsesThreadedAnimation:';
    function usesThreadedAnimation: ObjCBOOL; message 'usesThreadedAnimation';
    procedure startAnimation (sender: id); message 'startAnimation:';
    procedure stopAnimation (sender: id); message 'stopAnimation:';
    procedure setStyle(newValue: NSProgressIndicatorStyle); message 'setStyle:';
    function style: NSProgressIndicatorStyle; message 'style';
    procedure sizeToFit; message 'sizeToFit';
    procedure setDisplayedWhenStopped(newValue: ObjCBOOL); message 'setDisplayedWhenStopped:';
    function isDisplayedWhenStopped: ObjCBOOL; message 'isDisplayedWhenStopped';

		{ Adopted protocols }
		function accessibilityValue: NSNumber; message 'accessibilityValue';
    function accessibilityFrame: NSRect; message 'accessibilityFrame';
    function accessibilityParent: id; message 'accessibilityParent';
    
  end;


type
  NSProgressIndicatorDeprecated = objccategory external (NSProgressIndicator)
    function animationDelay: NSTimeInterval; message 'animationDelay'; deprecated 'in 10_0, 10_6';
    procedure setAnimationDelay (delay: NSTimeInterval); message 'setAnimationDelay:'; deprecated 'in 10_0, 10_6';
    procedure animate (sender: id); message 'animate:'; deprecated 'in 10_0, 10_6';
  end;
{$endif}

