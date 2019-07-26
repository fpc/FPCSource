{ Parsed from AppKit.framework NSPanGestureRecognizer.h }


{$ifdef TYPES}
type
  NSPanGestureRecognizerPtr = ^NSPanGestureRecognizer;
{$endif}

{$ifdef CLASSES}

type
  NSPanGestureRecognizer = objcclass external (NSGestureRecognizer, NSCodingProtocol)
  private
    _location: NSPoint;
    _translation: NSPoint;
    _buttonMask: NSUInteger;
    _buttonCount: NSInteger;
    _velocityFilter: id;
    private0: CGFloat;
    private1: CGFloat;
    __pgrFlags: bitpacked record
      case byte of
        0: (_anonBitField___pgrFlags0: cuint);
        1: (
          reserved: 0..((1 shl 32)-1);
        );
      end;
    {$ifndef __OBJC2__}
    _reserved0: NSInteger;
    _reserved1: NSInteger;
    {$endif}
  public
    procedure setButtonMask(newValue: NSUInteger); message 'setButtonMask:';
    function buttonMask: NSUInteger; message 'buttonMask';
    function translationInView (view_: NSView): NSPoint; message 'translationInView:';
    procedure setTranslation_inView (translation: NSPoint; view_: NSView); message 'setTranslation:inView:';
    function velocityInView (view_: NSView): NSPoint; message 'velocityInView:';
  end;
{$endif}

