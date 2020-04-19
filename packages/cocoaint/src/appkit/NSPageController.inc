{ Parsed from AppKit.framework NSPageController.h }


{$ifdef TYPES}
type
  NSPageControllerPtr = ^NSPageController;
  NSPageControllerDelegateProtocolPtr = ^NSPageControllerDelegateProtocol;
{$endif}

{$ifdef TYPES}
type
  NSPageControllerTransitionStyle = NSInteger;
  NSPageControllerTransitionStylePtr = ^NSPageControllerTransitionStyle;

const
  NSPageControllerTransitionStyleStackHistory = 0;
  NSPageControllerTransitionStyleStackBook = 1;
  NSPageControllerTransitionStyleHorizontalStrip = 2;
{$endif}

{$ifdef CLASSES}

type
  NSPageController = objcclass external (NSViewController, NSAnimatablePropertyContainerProtocol, NSCodingProtocol)
  private
    _contentView: NSView;
    _selectedViewController: NSViewController;
    _currentIdentifier: NSString;
    _transitionStyle: NSPageControllerTransitionStyle;
    _delegate: NSPageControllerDelegateProtocol;
    _selectedIndex: NSInteger;
    _arrangedObjects: NSMutableArray;
    _snapshots: NSMutableArray;
    _pcDelegateFlags: bitpacked record
      case byte of
        0: (_anonBitField__pcDelegateFlags0: cuint);
        1: (
          delegateRespondsToIdentifierForRepresentedObject: 0..1;
          delegateRespondsToViewControllerForIdentifier: 0..1;
          delegateRespondsToFrameForRepresentedObject: 0..1;
          delegateRespondsToPrepareView: 0..1;
          delegateRespondsToDidTransition: 0..1;
          delegateRespondsToWillLiveTransition: 0..1;
          delegateRespondsToDidLiveTransition: 0..1;
          delegateRespondsToReserved1: 0..1;
          reserved: 0..((1 shl 24)-1);
        );
      end;
    _pcFlags: bitpacked record
      case byte of
        0: (_anonBitField__pcFlags0: cuint);
        1: (
          templateCacheIsInvalid: 0..1;
          private1: 0..1;
          private2: 0..1;
          inSwipeGesture: 0..1;
          reserved: 0..((1 shl 28)-1);
        );
      end;
    _cancelAnimation: pObjCBOOL;
    _reusueQueue: NSMutableDictionary;
    _templateCache: NSMutableDictionary;
    _transitionView: NSView;
    _animator: id;
    _animationsDictionary: NSDictionary;
    _private: id;
  public
    procedure setDelegate(newValue: NSPageControllerDelegateProtocol); message 'setDelegate:';
    function delegate: NSPageControllerDelegateProtocol; message 'delegate';
    function selectedViewController: NSViewController; message 'selectedViewController';
    procedure setTransitionStyle(newValue: NSPageControllerTransitionStyle); message 'setTransitionStyle:';
    function transitionStyle: NSPageControllerTransitionStyle; message 'transitionStyle';
    procedure setArrangedObjects(newValue: NSArray); message 'setArrangedObjects:';
    function arrangedObjects: NSArray; message 'arrangedObjects';
    procedure setSelectedIndex(newValue: NSInteger); message 'setSelectedIndex:';
    function selectedIndex: NSInteger; message 'selectedIndex';
    procedure navigateForwardToObject (object_: id); message 'navigateForwardToObject:';
    procedure completeTransition; message 'completeTransition';
    procedure navigateBack (sender: id); message 'navigateBack:';
    procedure navigateForward (sender: id); message 'navigateForward:';
    procedure takeSelectedIndexFrom (sender: id); message 'takeSelectedIndexFrom:';

    { Adopted protocols }
    function animationForKey (key: NSString): id; message 'animationForKey:'; { available in 10_5 }
    function animations: NSDictionary; message 'animations'; { available in 10_5 }
    function animator: instancetype; message 'animator'; { available in 10_5 }
    class function defaultAnimationForKey (key: NSString): id; message 'defaultAnimationForKey:'; { available in 10_5 }
    procedure setAnimations (animations_: NSDictionary); message 'setAnimations:'; { available in 10_5 }
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  NSPageControllerDelegateProtocol = objcprotocol external name 'NSPageControllerDelegate' (NSObjectProtocol)
  required
  optional
    function pageController_identifierForObject (pageController: NSPageController; object_: id): NSString; message 'pageController:identifierForObject:';
    function pageController_viewControllerForIdentifier (pageController: NSPageController; identifier: NSString): NSViewController; message 'pageController:viewControllerForIdentifier:';
    function pageController_frameForObject (pageController: NSPageController; object_: id): NSRect; message 'pageController:frameForObject:';
    procedure pageController_prepareViewController_withObject (pageController: NSPageController; viewController: NSViewController; object_: id); message 'pageController:prepareViewController:withObject:';
    procedure pageController_didTransitionToObject (pageController: NSPageController; object_: id); message 'pageController:didTransitionToObject:';
    procedure pageControllerWillStartLiveTransition (pageController: NSPageController); message 'pageControllerWillStartLiveTransition:';
    procedure pageControllerDidEndLiveTransition (pageController: NSPageController); message 'pageControllerDidEndLiveTransition:';
  end;
{$endif}

