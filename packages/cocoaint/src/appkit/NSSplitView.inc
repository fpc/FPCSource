{ Parsed from AppKit.framework NSSplitView.h }


{$ifdef TYPES}
type
  NSSplitViewPtr = ^NSSplitView;
  NSSplitViewDelegateProtocolPtr = ^NSSplitViewDelegateProtocol;
{$endif}

{$ifdef TYPES}
type
  NSSplitViewDividerStyle = NSInteger;
  NSSplitViewDividerStylePtr = ^NSSplitViewDividerStyle;

const
  NSSplitViewDividerStyleThick = 1;
  NSSplitViewDividerStyleThin = 2;
  NSSplitViewDividerStylePaneSplitter = 3 { available in 10_6 };
{$endif}

{$ifdef CLASSES}

type
  NSSplitView = objcclass external (NSView)
  private
    _variables: id;
  public
    procedure setVertical(newValue: ObjCBOOL); message 'setVertical:';
    function isVertical: ObjCBOOL; message 'isVertical';
    procedure setDividerStyle(newValue: NSSplitViewDividerStyle); message 'setDividerStyle:';
    function dividerStyle: NSSplitViewDividerStyle; message 'dividerStyle';
    procedure setAutosaveName(newValue: NSString); message 'setAutosaveName:';
    function autosaveName: NSString; message 'autosaveName';
    procedure setDelegate(newValue: NSSplitViewDelegateProtocol); message 'setDelegate:';
    function delegate: NSSplitViewDelegateProtocol; message 'delegate';
    procedure drawDividerInRect (rect: NSRect); message 'drawDividerInRect:';
    function dividerColor: NSColor; message 'dividerColor';
    function dividerThickness: CGFloat; message 'dividerThickness';
    procedure adjustSubviews; message 'adjustSubviews';
    function isSubviewCollapsed (subview: NSView): ObjCBOOL; message 'isSubviewCollapsed:';
    function minPossiblePositionOfDividerAtIndex (dividerIndex: NSInteger): CGFloat; message 'minPossiblePositionOfDividerAtIndex:'; { available in 10_5 }
    function maxPossiblePositionOfDividerAtIndex (dividerIndex: NSInteger): CGFloat; message 'maxPossiblePositionOfDividerAtIndex:'; { available in 10_5 }
    procedure setPosition_ofDividerAtIndex (position: CGFloat; dividerIndex: NSInteger); message 'setPosition:ofDividerAtIndex:'; { available in 10_5 }
    function holdingPriorityForSubviewAtIndex (subviewIndex: NSInteger): NSLayoutPriority; message 'holdingPriorityForSubviewAtIndex:'; { available in 10_8 }
    procedure setHoldingPriority_forSubviewAtIndex (priority: NSLayoutPriority; subviewIndex: NSInteger); message 'setHoldingPriority:forSubviewAtIndex:'; { available in 10_8 }
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  NSSplitViewDelegateProtocol = objcprotocol external name 'NSSplitViewDelegate' (NSObjectProtocol)
  optional
    function splitView_canCollapseSubview (splitView: NSSplitView; subview: NSView): ObjCBOOL; message 'splitView:canCollapseSubview:';
    function splitView_shouldCollapseSubview_forDoubleClickOnDividerAtIndex (splitView: NSSplitView; subview: NSView; dividerIndex: NSInteger): ObjCBOOL; message 'splitView:shouldCollapseSubview:forDoubleClickOnDividerAtIndex:'; { available in 10_5 }
    function splitView_constrainMinCoordinate_ofSubviewAt (splitView: NSSplitView; proposedMinimumPosition: CGFloat; dividerIndex: NSInteger): CGFloat; message 'splitView:constrainMinCoordinate:ofSubviewAt:';
    function splitView_constrainMaxCoordinate_ofSubviewAt (splitView: NSSplitView; proposedMaximumPosition: CGFloat; dividerIndex: NSInteger): CGFloat; message 'splitView:constrainMaxCoordinate:ofSubviewAt:';
    function splitView_constrainSplitPosition_ofSubviewAt (splitView: NSSplitView; proposedPosition: CGFloat; dividerIndex: NSInteger): CGFloat; message 'splitView:constrainSplitPosition:ofSubviewAt:';
    procedure splitView_resizeSubviewsWithOldSize (splitView: NSSplitView; oldSize: NSSize); message 'splitView:resizeSubviewsWithOldSize:';
    function splitView_shouldAdjustSizeOfSubview (splitView: NSSplitView; view: NSView): ObjCBOOL; message 'splitView:shouldAdjustSizeOfSubview:'; { available in 10_6 }
    function splitView_shouldHideDividerAtIndex (splitView: NSSplitView; dividerIndex: NSInteger): ObjCBOOL; message 'splitView:shouldHideDividerAtIndex:'; { available in 10_5 }
    function splitView_effectiveRect_forDrawnRect_ofDividerAtIndex (splitView: NSSplitView; proposedEffectiveRect: NSRect; drawnRect: NSRect; dividerIndex: NSInteger): NSRect; message 'splitView:effectiveRect:forDrawnRect:ofDividerAtIndex:'; { available in 10_5 }
    function splitView_additionalEffectiveRectOfDividerAtIndex (splitView: NSSplitView; dividerIndex: NSInteger): NSRect; message 'splitView:additionalEffectiveRectOfDividerAtIndex:'; { available in 10_5 }
    procedure splitViewWillResizeSubviews (notification: NSNotification); message 'splitViewWillResizeSubviews:';
    procedure splitViewDidResizeSubviews (notification: NSNotification); message 'splitViewDidResizeSubviews:';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSSplitViewWillResizeSubviewsNotification: NSString; cvar; external;
  NSSplitViewDidResizeSubviewsNotification: NSString; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  NSSplitView_NSDeprecated = objccategory external name 'NSDeprecated' (NSSplitView)
    procedure setIsPaneSplitter (flag: ObjCBOOL); message 'setIsPaneSplitter:'; deprecated 'in 10_0, 10_6';
    function isPaneSplitter: ObjCBOOL; message 'isPaneSplitter'; deprecated 'in 10_0, 10_6';
  end;
{$endif}

