{ Parsed from AppKit.framework NSSplitViewController.h }


{$ifdef TYPES}
type
  NSSplitViewControllerPtr = ^NSSplitViewController;
  NSSplitViewItemPtr = ^NSSplitViewItem;
{$endif}

{$ifdef CLASSES}

type
  NSSplitViewController = objcclass external (NSViewController, NSSplitViewDelegateProtocol)
  private
    _splitView: NSSplitView;
    _splitViewControllerPrivateData: id;
    _splitViewControllerFlags: bitpacked record
      case byte of
        0: (_anonBitField__splitViewControllerFlags0: cuint);
        1: (
          _reserved: 0..((1 shl 32)-1);
        );
      end;
  public
    procedure setSplitView(newValue: NSSplitView); message 'setSplitView:';
    function splitView: NSSplitView; message 'splitView';
    procedure setSplitViewItems(newValue: NSArray); message 'setSplitViewItems:';
    function splitViewItems: NSArray; message 'splitViewItems';
    procedure addSplitViewItem (splitViewItem: NSSplitViewItem); message 'addSplitViewItem:';
    procedure insertSplitViewItem_atIndex (splitViewItem: NSSplitViewItem; index: NSInteger); message 'insertSplitViewItem:atIndex:';
    procedure removeSplitViewItem (splitViewItem: NSSplitViewItem); message 'removeSplitViewItem:';
    function splitViewItemForViewController (viewController: NSViewController): NSSplitViewItem; message 'splitViewItemForViewController:';
    procedure viewDidLoad; message 'viewDidLoad'; { NS_REQUIRES_SUPER }
    function splitView_canCollapseSubview (splitView_: NSSplitView; subview: NSView): ObjCBOOL; message 'splitView:canCollapseSubview:'; { NS_REQUIRES_SUPER }
    function splitView_shouldCollapseSubview_forDoubleClickOnDividerAtIndex (splitView_: NSSplitView; subview: NSView; dividerIndex: NSInteger): ObjCBOOL; message 'splitView:shouldCollapseSubview:forDoubleClickOnDividerAtIndex:'; { NS_REQUIRES_SUPER }
    function splitView_shouldHideDividerAtIndex (splitView_: NSSplitView; dividerIndex: NSInteger): ObjCBOOL; message 'splitView:shouldHideDividerAtIndex:'; { NS_REQUIRES_SUPER }
    function splitView_effectiveRect_forDrawnRect_ofDividerAtIndex (splitView_: NSSplitView; proposedEffectiveRect: NSRect; drawnRect: NSRect; dividerIndex: NSInteger): NSRect; message 'splitView:effectiveRect:forDrawnRect:ofDividerAtIndex:'; { NS_REQUIRES_SUPER }
    function splitView_additionalEffectiveRectOfDividerAtIndex (splitView_: NSSplitView; dividerIndex: NSInteger): NSRect; message 'splitView:additionalEffectiveRectOfDividerAtIndex:'; { NS_REQUIRES_SUPER }

    { Adopted protocols }
    function splitView_constrainMaxCoordinate_ofSubviewAt (splitView: NSSplitView; proposedMaximumPosition: CGFloat; dividerIndex: NSInteger): CGFloat; message 'splitView:constrainMaxCoordinate:ofSubviewAt:';
    function splitView_constrainMinCoordinate_ofSubviewAt (splitView: NSSplitView; proposedMinimumPosition: CGFloat; dividerIndex: NSInteger): CGFloat; message 'splitView:constrainMinCoordinate:ofSubviewAt:';
    function splitView_constrainSplitPosition_ofSubviewAt (splitView: NSSplitView; proposedPosition: CGFloat; dividerIndex: NSInteger): CGFloat; message 'splitView:constrainSplitPosition:ofSubviewAt:';
    procedure splitView_resizeSubviewsWithOldSize (splitView: NSSplitView; oldSize: NSSize); message 'splitView:resizeSubviewsWithOldSize:';
    function splitView_shouldAdjustSizeOfSubview (splitView: NSSplitView; view: NSView): ObjCBOOL; message 'splitView:shouldAdjustSizeOfSubview:'; { available in 10_6 }
    procedure splitViewDidResizeSubviews (notification: NSNotification); message 'splitViewDidResizeSubviews:';
    procedure splitViewWillResizeSubviews (notification: NSNotification); message 'splitViewWillResizeSubviews:';
  end;

type
  NSSplitViewItem = objcclass external (NSObject, NSAnimatablePropertyContainerProtocol, NSCodingProtocol)
  private
    _splitViewItemPrivateData: id;
    _flags: bitpacked record
      case byte of
        0: (_anonBitField__flags0: cuint);
        1: (
          _collapsed: 0..1;
          _canCollapseFromDrag: 0..1;
          _canCollapseFromDoubleClickOnDivider: 0..1;
          _reserved: 0..((1 shl 29)-1);
        );
      end;
  public
    class function splitViewItemWithViewController (viewController: NSViewController): instancetype; message 'splitViewItemWithViewController:';
    procedure setViewController(newValue: NSViewController); message 'setViewController:';
    function viewController: NSViewController; message 'viewController';
    procedure setCollapsed(newValue: ObjCBOOL); message 'setCollapsed:';
    function isCollapsed: ObjCBOOL; message 'isCollapsed';
    procedure setCanCollapse(newValue: ObjCBOOL); message 'setCanCollapse:';
    function canCollapse: ObjCBOOL; message 'canCollapse';
    procedure setHoldingPriority(newValue: NSLayoutPriority); message 'setHoldingPriority:';
    function holdingPriority: NSLayoutPriority; message 'holdingPriority';

    { Adopted protocols }
    function animationForKey (key: NSString): id; message 'animationForKey:'; { available in 10_5 }
    function animations: NSDictionary; message 'animations'; { available in 10_5 }
    function animator: instancetype; message 'animator'; { available in 10_5 }
    class function defaultAnimationForKey (key: NSString): id; message 'defaultAnimationForKey:'; { available in 10_5 }
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
    procedure setAnimations (animations_: NSDictionary); message 'setAnimations:'; { available in 10_5 }
  end;
{$endif}

