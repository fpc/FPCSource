{ Parsed from AppKit.framework NSLayoutConstraint.h }


{$ifdef TYPES}
type
  NSLayoutConstraintPtr = ^NSLayoutConstraint;
{$endif}

{$ifdef TYPES}
type
  NSLayoutRelation = NSInteger;
  NSLayoutRelationPtr = ^NSLayoutRelation;

const
  NSLayoutRelationLessThanOrEqual = -1;
  NSLayoutRelationEqual = 0;
  NSLayoutRelationGreaterThanOrEqual = 1;

type
  NSLayoutAttribute = NSInteger;
  NSLayoutAttributePtr = ^NSLayoutAttribute;

const
  NSLayoutAttributeLeft = 1;
  NSLayoutAttributeRight = 2;
  NSLayoutAttributeTop = 3;
  NSLayoutAttributeBottom = 4;
  NSLayoutAttributeLeading = 5;
  NSLayoutAttributeTrailing = 6;
  NSLayoutAttributeWidth = 7;
  NSLayoutAttributeHeight = 8;
  NSLayoutAttributeCenterX = 9;
  NSLayoutAttributeCenterY = 10;
  NSLayoutAttributeBaseline = 11;
  NSLayoutAttributeNotAnAttribute = 0;

type
  NSLayoutFormatOptions = NSUInteger;
  NSLayoutFormatOptionsPtr = ^NSLayoutFormatOptions;

const
  NSLayoutFormatAlignAllLeft = 1 shl NSLayoutAttributeLeft;
  NSLayoutFormatAlignAllRight = 1 shl NSLayoutAttributeRight;
  NSLayoutFormatAlignAllTop = 1 shl NSLayoutAttributeTop;
  NSLayoutFormatAlignAllBottom = 1 shl NSLayoutAttributeBottom;
  NSLayoutFormatAlignAllLeading = 1 shl NSLayoutAttributeLeading;
  NSLayoutFormatAlignAllTrailing = 1 shl NSLayoutAttributeTrailing;
  NSLayoutFormatAlignAllCenterX = 1 shl NSLayoutAttributeCenterX;
  NSLayoutFormatAlignAllCenterY = 1 shl NSLayoutAttributeCenterY;
  NSLayoutFormatAlignAllBaseline = 1 shl NSLayoutAttributeBaseline;
  NSLayoutFormatAlignmentMask = $FFFF;
  NSLayoutFormatDirectionLeadingToTrailing = 0 shl 16;
  NSLayoutFormatDirectionLeftToRight = 1 shl 16;
  NSLayoutFormatDirectionRightToLeft = 2 shl 16;
  NSLayoutFormatDirectionMask = $3 shl 16;

type
  NSLayoutConstraintOrientation = NSInteger;
  NSLayoutConstraintOrientationPtr = ^NSLayoutConstraintOrientation;

const
  NSLayoutConstraintOrientationHorizontal = 0;
  NSLayoutConstraintOrientationVertical = 1;
{$endif}

{$ifndef NSEDGEINSETS_DEFINED}
{$ifdef TYPES}
type
  NSEdgeInsets = record
    top: CGFloat;
    left: CGFloat;
    bottom: CGFloat;
    right: CGFloat;
  end;
type
  NSEdgeInsetsPtr = ^NSEdgeInsets;
{$endif}

{$endif}
{$ifdef TYPES}
type
  NSLayoutPriority = single { available in 10_7 };
  NSLayoutPriorityPtr = ^NSLayoutPriority { available in 10_7 };
{$endif}

{$ifdef CLASSES}

type
  NSLayoutConstraint = objcclass external (NSObject, NSAnimatablePropertyContainerProtocol)
  private
    _container: id;
    _firstItem: id;
    _secondItem: id;
    _constant: CGFloat;
    _loweredConstant: CGFloat;
    _markerAndPositiveExtraVar: id;
    _negativeExtraVar: id;
    {$if defined(__OBJC2__)}
    _layoutConstraintFlags: cuint64;
    _coefficient: single;
    _priority: NSLayoutPriority;
    {$else}
    _coefficient_: single;
    _priority_: NSLayoutPriority;
    _layoutConstraintFlags_: cuint64;
    _flange: id;
    {$endif}
  public
    class function constraintsWithVisualFormat_options_metrics_views (format: NSString; opts: NSLayoutFormatOptions; metrics: NSDictionary; views: NSDictionary): NSArray; message 'constraintsWithVisualFormat:options:metrics:views:';
    class function constraintWithItem_attribute_relatedBy_toItem_attribute_multiplier_constant (view1: id; attr1: NSLayoutAttribute; relation: NSLayoutRelation; view2: id; attr2: NSLayoutAttribute; multiplier: CGFloat; c: CGFloat): instancetype; message 'constraintWithItem:attribute:relatedBy:toItem:attribute:multiplier:constant:';
    procedure setPriority(newValue: NSLayoutPriority); message 'setPriority:';
    function priority: NSLayoutPriority; message 'priority';
    procedure setShouldBeArchived(newValue: ObjCBOOL); message 'setShouldBeArchived:';
    function shouldBeArchived: ObjCBOOL; message 'shouldBeArchived';
    function firstItem: id; message 'firstItem';
    function firstAttribute: NSLayoutAttribute; message 'firstAttribute';
    function relation: NSLayoutRelation; message 'relation';
    function secondItem: id; message 'secondItem';
    function secondAttribute: NSLayoutAttribute; message 'secondAttribute';
    function multiplier: CGFloat; message 'multiplier';
    procedure setConstant(newValue: CGFloat); message 'setConstant:';
    function constant: CGFloat; message 'constant';
    procedure setActive(newValue: ObjCBOOL); message 'setActive:';
    function isActive: ObjCBOOL; message 'isActive';
    class procedure activateConstraints (constraints: NSArray); message 'activateConstraints:'; { available in 10_10, 8_0 }
    class procedure deactivateConstraints (constraints: NSArray); message 'deactivateConstraints:'; { available in 10_10, 8_0 }

    { Adopted protocols }
    function animationForKey (key: NSString): id; message 'animationForKey:'; { available in 10_5 }
    function animations: NSDictionary; message 'animations'; { available in 10_5 }
    function animator: instancetype; message 'animator'; { available in 10_5 }
    class function defaultAnimationForKey (key: NSString): id; message 'defaultAnimationForKey:'; { available in 10_5 }
    procedure setAnimations (animations_: NSDictionary); message 'setAnimations:'; { available in 10_5 }
  end;


type
  NSIdentifier = objccategory external (NSLayoutConstraint)
    procedure setIdentifier(newValue: NSString); message 'setIdentifier:';
    function identifier: NSString; message 'identifier';
  end;

type
  NSConstraintBasedLayoutInstallingConstraints = objccategory external (NSView)
    function constraints: NSArray; message 'constraints';
    procedure addConstraint (constraint: NSLayoutConstraint); message 'addConstraint:'; { available in 10_7 }
    procedure addConstraints (constraints_: NSArray); message 'addConstraints:'; { available in 10_7 }
    procedure removeConstraint (constraint: NSLayoutConstraint); message 'removeConstraint:'; { available in 10_7 }
    procedure removeConstraints (constraints_: NSArray); message 'removeConstraints:'; { available in 10_7 }
  end;

type
  NSWindow_NSConstraintBasedLayoutCoreMethods = objccategory external name 'NSConstraintBasedLayoutCoreMethods' (NSWindow)
    procedure updateConstraintsIfNeeded; message 'updateConstraintsIfNeeded'; { available in 10_7 }
    procedure layoutIfNeeded; message 'layoutIfNeeded'; { available in 10_7 }
  end;

type
  NSConstraintBasedLayoutCoreMethods = objccategory external (NSView)
    procedure updateConstraintsForSubtreeIfNeeded; message 'updateConstraintsForSubtreeIfNeeded'; { available in 10_7 }
    procedure updateConstraints; message 'updateConstraints'; { available in 10_7 }
    procedure setNeedsUpdateConstraints(newValue: ObjCBOOL); message 'setNeedsUpdateConstraints:';
    function needsUpdateConstraints: ObjCBOOL; message 'needsUpdateConstraints';
    procedure layoutSubtreeIfNeeded; message 'layoutSubtreeIfNeeded'; { available in 10_7 }
    procedure layout; message 'layout'; { available in 10_7 }
    procedure setNeedsLayout(newValue: ObjCBOOL); message 'setNeedsLayout:';
    function needsLayout: ObjCBOOL; message 'needsLayout';
  end;

type
  NSConstraintBasedCompatibility = objccategory external (NSView)
    procedure setTranslatesAutoresizingMaskIntoConstraints(newValue: ObjCBOOL); message 'setTranslatesAutoresizingMaskIntoConstraints:';
    function translatesAutoresizingMaskIntoConstraints: ObjCBOOL; message 'translatesAutoresizingMaskIntoConstraints';
    class function requiresConstraintBasedLayout: ObjCBOOL; message 'requiresConstraintBasedLayout'; { available in 10_7 }
  end;

type
  NSView_NSConstraintBasedLayoutLayering = objccategory external name 'NSConstraintBasedLayoutLayering' (NSView)
    function alignmentRectForFrame (frame_: NSRect): NSRect; message 'alignmentRectForFrame:'; { available in 10_7 }
    function frameForAlignmentRect (alignmentRect: NSRect): NSRect; message 'frameForAlignmentRect:'; { available in 10_7 }
    function alignmentRectInsets: NSEdgeInsets; message 'alignmentRectInsets';
    function baselineOffsetFromBottom: CGFloat; message 'baselineOffsetFromBottom';
    function intrinsicContentSize: NSSize; message 'intrinsicContentSize';
    procedure invalidateIntrinsicContentSize; message 'invalidateIntrinsicContentSize'; { available in 10_7 }
    function contentHuggingPriorityForOrientation (orientation: NSLayoutConstraintOrientation): NSLayoutPriority; message 'contentHuggingPriorityForOrientation:'; { available in 10_7 }
    procedure setContentHuggingPriority_forOrientation (priority: NSLayoutPriority; orientation: NSLayoutConstraintOrientation); message 'setContentHuggingPriority:forOrientation:'; { available in 10_7 }
    function contentCompressionResistancePriorityForOrientation (orientation: NSLayoutConstraintOrientation): NSLayoutPriority; message 'contentCompressionResistancePriorityForOrientation:'; { available in 10_7 }
    procedure setContentCompressionResistancePriority_forOrientation (priority: NSLayoutPriority; orientation: NSLayoutConstraintOrientation); message 'setContentCompressionResistancePriority:forOrientation:'; { available in 10_7 }
  end;

type
  NSConstraintBasedLayoutLayering = objccategory external (NSControl)
    procedure invalidateIntrinsicContentSizeForCell (cell: NSCell); message 'invalidateIntrinsicContentSizeForCell:'; { available in 10_7 }
  end;

type
  NSConstraintBasedLayoutAnchoring = objccategory external (NSWindow)
    function anchorAttributeForOrientation (orientation: NSLayoutConstraintOrientation): NSLayoutAttribute; message 'anchorAttributeForOrientation:';
    procedure setAnchorAttribute_forOrientation (attr: NSLayoutAttribute; orientation: NSLayoutConstraintOrientation); message 'setAnchorAttribute:forOrientation:';
  end;

type
  NSConstraintBasedLayoutFittingSize = objccategory external (NSView)
    function fittingSize: NSSize; message 'fittingSize';
  end;

type
  NSView_NSConstraintBasedLayoutDebugging = objccategory external name 'NSConstraintBasedLayoutDebugging' (NSView)
    function constraintsAffectingLayoutForOrientation (orientation: NSLayoutConstraintOrientation): NSArray; message 'constraintsAffectingLayoutForOrientation:'; { available in 10_7 }
    function hasAmbiguousLayout: ObjCBOOL; message 'hasAmbiguousLayout';
    procedure exerciseAmbiguityInLayout; message 'exerciseAmbiguityInLayout'; { available in 10_7 }
  end;

type
  NSConstraintBasedLayoutDebugging = objccategory external (NSWindow)
    procedure visualizeConstraints (constraints: NSArray); message 'visualizeConstraints:'; { available in 10_7 }
  end;
{$endif}

