{ Parsed from AppKit.framework NSStackView.h }


{$ifdef TYPES}
type
  NSStackViewPtr = ^NSStackView;
  NSStackViewDelegateProtocolPtr = ^NSStackViewDelegateProtocol;
{$endif}

{$ifdef TYPES}
type
  NSUserInterfaceLayoutOrientation = NSInteger;
  NSUserInterfaceLayoutOrientationPtr = ^NSUserInterfaceLayoutOrientation;

const
  NSUserInterfaceLayoutOrientationHorizontal = 0;
  NSUserInterfaceLayoutOrientationVertical = 1;

type
  NSStackViewGravity = NSInteger;
  NSStackViewGravityPtr = ^NSStackViewGravity;

const
  NSStackViewGravityTop = 1;
  NSStackViewGravityLeading = 1;
  NSStackViewGravityCenter = 2;
  NSStackViewGravityBottom = 3;
  NSStackViewGravityTrailing = 3;

type
  NSStackViewVisibilityPriority = single { available in 10_9 };
  NSStackViewVisibilityPriorityPtr = ^NSStackViewVisibilityPriority { available in 10_9 };
{$endif}

{$ifdef CLASSES}

type
  NSStackView = objcclass external (NSView)
  private
    _delegate: NSStackViewDelegateProtocol;
    _orientation: NSUserInterfaceLayoutOrientation;
    _alignment: NSLayoutAttribute;
    _edgeInsets: NSEdgeInsets;
    _spacing: CGFloat;
    _verticalClippingResistancePriority: NSLayoutPriority;
    _horizontalClippingResistancePriority: NSLayoutPriority;
    _verticalHuggingPriority: NSLayoutPriority;
    _horizontalHuggingPriority: NSLayoutPriority;
    _hasEqualSpacing: ObjCBOOL;
    _animator: id;
    _animationsDictionary: NSDictionary;
    _private: id;
    _flags: bitpacked record
      case byte of
        0: (_anonBitField__flags0: cuint);
        1: (
          _inDealloc: 0..1;
          _reserved: 0..((1 shl 31)-1);
        );
      end;
  public
    class function stackViewWithViews (views: NSArray): instancetype; message 'stackViewWithViews:';
    procedure setDelegate(newValue: NSStackViewDelegateProtocol); message 'setDelegate:';
    function delegate: NSStackViewDelegateProtocol; message 'delegate';
    procedure setOrientation(newValue: NSUserInterfaceLayoutOrientation); message 'setOrientation:';
    function orientation: NSUserInterfaceLayoutOrientation; message 'orientation';
    procedure setAlignment(newValue: NSLayoutAttribute); message 'setAlignment:';
    function alignment: NSLayoutAttribute; message 'alignment';
    procedure setEdgeInsets(newValue: NSEdgeInsets); message 'setEdgeInsets:';
    function edgeInsets: NSEdgeInsets; message 'edgeInsets';
    procedure addView_inGravity (aView: NSView; gravity: NSStackViewGravity); message 'addView:inGravity:';
    procedure insertView_atIndex_inGravity (aView: NSView; index: NSUInteger; gravity: NSStackViewGravity); message 'insertView:atIndex:inGravity:';
    procedure removeView (aView: NSView); message 'removeView:';
    function viewsInGravity (gravity: NSStackViewGravity): NSArray; message 'viewsInGravity:';
    procedure setViews_inGravity (views: NSArray; gravity: NSStackViewGravity); message 'setViews:inGravity:';
    function views: NSArray; message 'views';
    function detachedViews: NSArray; message 'detachedViews';
    procedure setVisibilityPriority_forView (priority: NSStackViewVisibilityPriority; aView: NSView); message 'setVisibilityPriority:forView:';
    function visibilityPriorityForView (aView: NSView): NSStackViewVisibilityPriority; message 'visibilityPriorityForView:';
    procedure setSpacing(newValue: CGFloat); message 'setSpacing:';
    function spacing: CGFloat; message 'spacing';
    procedure setCustomSpacing_afterView (spacing_: CGFloat; aView: NSView); message 'setCustomSpacing:afterView:';
    function customSpacingAfterView (aView: NSView): CGFloat; message 'customSpacingAfterView:';
    procedure setHasEqualSpacing(newValue: ObjCBOOL); message 'setHasEqualSpacing:';
    function hasEqualSpacing: ObjCBOOL; message 'hasEqualSpacing';
    function clippingResistancePriorityForOrientation (orientation_: NSLayoutConstraintOrientation): NSLayoutPriority; message 'clippingResistancePriorityForOrientation:';
    procedure setClippingResistancePriority_forOrientation (clippingResistancePriority: NSLayoutPriority; orientation_: NSLayoutConstraintOrientation); message 'setClippingResistancePriority:forOrientation:';
    function huggingPriorityForOrientation (orientation_: NSLayoutConstraintOrientation): NSLayoutPriority; message 'huggingPriorityForOrientation:';
    procedure setHuggingPriority_forOrientation (huggingPriority: NSLayoutPriority; orientation_: NSLayoutConstraintOrientation); message 'setHuggingPriority:forOrientation:';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  NSStackViewDelegateProtocol = objcprotocol external name 'NSStackViewDelegate' (NSObjectProtocol)
  optional
    procedure stackView_willDetachViews (stackView: NSStackView; views: NSArray); message 'stackView:willDetachViews:';
    procedure stackView_didReattachViews (stackView: NSStackView; views: NSArray); message 'stackView:didReattachViews:';
  end;
{$endif}

