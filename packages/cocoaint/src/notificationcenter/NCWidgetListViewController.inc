{ Parsed from NotificationCenter.framework NCWidgetListViewController.h }


{$ifdef TYPES}
type
  NCWidgetListViewControllerPtr = ^NCWidgetListViewController;
  NCWidgetListViewDelegateProtocolPtr = ^NCWidgetListViewDelegateProtocol;
{$endif}

{$ifdef CLASSES}

type
  NCWidgetListViewController = objcclass external (NSViewController)
  public
    procedure setDelegate(newValue: NCWidgetListViewDelegateProtocol); message 'setDelegate:';
    function delegate: NCWidgetListViewDelegateProtocol; message 'delegate';
    procedure setContents(newValue: NSArray); message 'setContents:';
    function contents: NSArray; message 'contents';
    procedure setMinimumVisibleRowCount(newValue: NSUInteger); message 'setMinimumVisibleRowCount:';
    function minimumVisibleRowCount: NSUInteger; message 'minimumVisibleRowCount';
    procedure setHasDividerLines(newValue: ObjCBOOL); message 'setHasDividerLines:';
    function hasDividerLines: ObjCBOOL; message 'hasDividerLines';
    procedure setEditing(newValue: ObjCBOOL); message 'setEditing:';
    function editing: ObjCBOOL; message 'editing';
    procedure setShowsAddButtonWhenEditing(newValue: ObjCBOOL); message 'setShowsAddButtonWhenEditing:';
    function showsAddButtonWhenEditing: ObjCBOOL; message 'showsAddButtonWhenEditing';
    function viewControllerAtRow_makeIfNecessary (row: NSUInteger; makeIfNecesary: ObjCBOOL): NSViewController; message 'viewControllerAtRow:makeIfNecessary:';
    function rowForViewController (viewController: NSViewController): NSUInteger; message 'rowForViewController:';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  NCWidgetListViewDelegateProtocol = objcprotocol external name 'NCWidgetListViewDelegate' (NSObjectProtocol)
    function widgetList_viewControllerForRow (list: NCWidgetListViewController; row: NSUInteger): NSViewController; message 'widgetList:viewControllerForRow:';
  optional
    procedure widgetListPerformAddAction (list: NCWidgetListViewController); message 'widgetListPerformAddAction:';
    function widgetList_shouldReorderRow (list: NCWidgetListViewController; row: NSUInteger): ObjCBOOL; message 'widgetList:shouldReorderRow:';
    procedure widgetList_didReorderRow_toRow (list: NCWidgetListViewController; row: NSUInteger; newIndex: NSUInteger); message 'widgetList:didReorderRow:toRow:';
    function widgetList_shouldRemoveRow (list: NCWidgetListViewController; row: NSUInteger): ObjCBOOL; message 'widgetList:shouldRemoveRow:';
    procedure widgetList_didRemoveRow (list: NCWidgetListViewController; row: NSUInteger); message 'widgetList:didRemoveRow:';
  end;
{$endif}

