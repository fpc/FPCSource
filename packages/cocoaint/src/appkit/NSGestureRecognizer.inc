{ Parsed from AppKit.framework NSGestureRecognizer.h }


{$ifdef TYPES}
type
  NSGestureRecognizerPtr = ^NSGestureRecognizer;
  NSGestureRecognizerDelegateProtocolPtr = ^NSGestureRecognizerDelegateProtocol;
{$endif}

{$ifdef TYPES}
type
  NSGestureRecognizerState = NSInteger;
  NSGestureRecognizerStatePtr = ^NSGestureRecognizerState;

const
  NSGestureRecognizerStatePossible = 0;
  NSGestureRecognizerStateBegan = 1;
  NSGestureRecognizerStateChanged = 2;
  NSGestureRecognizerStateEnded = 3;
  NSGestureRecognizerStateCancelled = 4;
  NSGestureRecognizerStateFailed = 5;
  NSGestureRecognizerStateRecognized = NSGestureRecognizerStateEnded;
{$endif}

{$ifdef CLASSES}

type
  NSGestureRecognizer = objcclass external (NSObject, NSCodingProtocol)
  private
    _targets: NSMutableArray;
    _target: id;
    _action: SEL;
    _delayedEvents: NSMutableArray;
    _view: NSView;
    _updateEvent: NSEvent;
    _delegate: NSGestureRecognizerDelegateProtocol;
    _friends: NSMutableSet;
    _state: NSGestureRecognizerState;
    _gestureFlags: NSUInteger;
    _delayFlags: NSUInteger;
    _failureRequirements: NSMutableSet;
    _failureDependents: NSMutableSet;
    _dynamicFailureRequirements: NSMutableSet;
    _dynamicFailureDependents: NSMutableSet;
    _failureMap: id;
    _reserved: id;
  public
    function initWithTarget_action (target: id; action: SEL): instancetype; message 'initWithTarget:action:'; { NS_DESIGNATED_INITIALIZER }
    function initWithCoder (coder: NSCoder): instancetype; message 'initWithCoder:'; { NS_DESIGNATED_INITIALIZER }
    procedure setTarget(newValue: id); message 'setTarget:';
    function target: id; message 'target';
    procedure setAction(newValue: SEL); message 'setAction:';
    function action: SEL; message 'action';
    function state: NSGestureRecognizerState; message 'state';
    procedure setDelegate(newValue: NSGestureRecognizerDelegateProtocol); message 'setDelegate:';
    function delegate: NSGestureRecognizerDelegateProtocol; message 'delegate';
    procedure setEnabled(newValue: ObjCBOOL); message 'setEnabled:';
    function isEnabled: ObjCBOOL; message 'isEnabled';
    function view: NSView; message 'view';
    procedure setDelaysPrimaryMouseButtonEvents(newValue: ObjCBOOL); message 'setDelaysPrimaryMouseButtonEvents:';
    function delaysPrimaryMouseButtonEvents: ObjCBOOL; message 'delaysPrimaryMouseButtonEvents';
    procedure setDelaysSecondaryMouseButtonEvents(newValue: ObjCBOOL); message 'setDelaysSecondaryMouseButtonEvents:';
    function delaysSecondaryMouseButtonEvents: ObjCBOOL; message 'delaysSecondaryMouseButtonEvents';
    procedure setDelaysOtherMouseButtonEvents(newValue: ObjCBOOL); message 'setDelaysOtherMouseButtonEvents:';
    function delaysOtherMouseButtonEvents: ObjCBOOL; message 'delaysOtherMouseButtonEvents';
    procedure setDelaysKeyEvents(newValue: ObjCBOOL); message 'setDelaysKeyEvents:';
    function delaysKeyEvents: ObjCBOOL; message 'delaysKeyEvents';
    procedure setDelaysMagnificationEvents(newValue: ObjCBOOL); message 'setDelaysMagnificationEvents:';
    function delaysMagnificationEvents: ObjCBOOL; message 'delaysMagnificationEvents';
    procedure setDelaysRotationEvents(newValue: ObjCBOOL); message 'setDelaysRotationEvents:';
    function delaysRotationEvents: ObjCBOOL; message 'delaysRotationEvents';
    function locationInView (view_: NSView): NSPoint; message 'locationInView:';

    { Adopted protocols }
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  NSGestureRecognizerDelegateProtocol = objcprotocol external name 'NSGestureRecognizerDelegate' (NSObjectProtocol)
  optional
    function gestureRecognizerShouldBegin (gestureRecognizer: NSGestureRecognizer): ObjCBOOL; message 'gestureRecognizerShouldBegin:';
    function gestureRecognizer_shouldRecognizeSimultaneouslyWithGestureRecognizer (gestureRecognizer: NSGestureRecognizer; otherGestureRecognizer: NSGestureRecognizer): ObjCBOOL; message 'gestureRecognizer:shouldRecognizeSimultaneouslyWithGestureRecognizer:';
    function gestureRecognizer_shouldRequireFailureOfGestureRecognizer (gestureRecognizer: NSGestureRecognizer; otherGestureRecognizer: NSGestureRecognizer): ObjCBOOL; message 'gestureRecognizer:shouldRequireFailureOfGestureRecognizer:';
    function gestureRecognizer_shouldBeRequiredToFailByGestureRecognizer (gestureRecognizer: NSGestureRecognizer; otherGestureRecognizer: NSGestureRecognizer): ObjCBOOL; message 'gestureRecognizer:shouldBeRequiredToFailByGestureRecognizer:';
  end;
{$endif}

{$ifdef CLASSES}

type
  NSSubclassUse = objccategory external (NSGestureRecognizer)
    procedure setState(newValue: NSGestureRecognizerState); message 'setState:';
    function state: NSGestureRecognizerState; message 'state';
    procedure reset; message 'reset';
    function canPreventGestureRecognizer (preventedGestureRecognizer: NSGestureRecognizer): ObjCBOOL; message 'canPreventGestureRecognizer:';
    function canBePreventedByGestureRecognizer (preventingGestureRecognizer: NSGestureRecognizer): ObjCBOOL; message 'canBePreventedByGestureRecognizer:';
    function shouldRequireFailureOfGestureRecognizer (otherGestureRecognizer: NSGestureRecognizer): ObjCBOOL; message 'shouldRequireFailureOfGestureRecognizer:';
    function shouldBeRequiredToFailByGestureRecognizer (otherGestureRecognizer: NSGestureRecognizer): ObjCBOOL; message 'shouldBeRequiredToFailByGestureRecognizer:';
    procedure mouseDown (event: NSEvent); message 'mouseDown:';
    procedure rightMouseDown (event: NSEvent); message 'rightMouseDown:';
    procedure otherMouseDown (event: NSEvent); message 'otherMouseDown:';
    procedure mouseUp (event: NSEvent); message 'mouseUp:';
    procedure rightMouseUp (event: NSEvent); message 'rightMouseUp:';
    procedure otherMouseUp (event: NSEvent); message 'otherMouseUp:';
    procedure mouseDragged (event: NSEvent); message 'mouseDragged:';
    procedure rightMouseDragged (event: NSEvent); message 'rightMouseDragged:';
    procedure otherMouseDragged (event: NSEvent); message 'otherMouseDragged:';
    procedure keyDown (event: NSEvent); message 'keyDown:';
    procedure keyUp (event: NSEvent); message 'keyUp:';
    procedure flagsChanged (event: NSEvent); message 'flagsChanged:';
    procedure tabletPoint (event: NSEvent); message 'tabletPoint:';
    procedure magnifyWithEvent (event: NSEvent); message 'magnifyWithEvent:';
    procedure rotateWithEvent (event: NSEvent); message 'rotateWithEvent:';
  end;
{$endif}

