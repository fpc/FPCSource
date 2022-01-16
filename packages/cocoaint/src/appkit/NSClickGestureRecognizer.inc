{ Parsed from AppKit.framework NSClickGestureRecognizer.h }


{$ifdef TYPES}
type
  NSClickGestureRecognizerPtr = ^NSClickGestureRecognizer;
{$endif}

{$ifdef CLASSES}

type
  NSClickGestureRecognizer = objcclass external (NSGestureRecognizer, NSCodingProtocol)
  private
    _flags: NSInteger;
    _location: NSPoint;
    _buttonMask: NSUInteger;
    _numberOfClicksRequired: NSInteger;
    _currentButtonCount: NSInteger;
    _activeButtonCount: NSInteger;
    _currentClickCount: NSInteger;
    {$ifndef __OBJC2__}
    _reserved0: NSInteger;
    _reserved1: NSInteger;
    {$endif}
  public
    procedure setButtonMask(newValue: NSUInteger); message 'setButtonMask:';
    function buttonMask: NSUInteger; message 'buttonMask';
    procedure setNumberOfClicksRequired(newValue: NSInteger); message 'setNumberOfClicksRequired:';
    function numberOfClicksRequired: NSInteger; message 'numberOfClicksRequired';
  end;
{$endif}

