{ Parsed from AppKit.framework NSComboBox.h }


{$ifdef TYPES}
type
  NSComboBoxPtr = ^NSComboBox;
  NSComboBoxDataSourceProtocolPtr = ^NSComboBoxDataSourceProtocol;
  NSComboBoxDelegateProtocolPtr = ^NSComboBoxDelegateProtocol;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSComboBoxWillPopUpNotification: NSString; cvar; external;
  NSComboBoxWillDismissNotification: NSString; cvar; external;
  NSComboBoxSelectionDidChangeNotification: NSString; cvar; external;
  NSComboBoxSelectionIsChangingNotification: NSString; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  NSComboBox = objcclass external (NSTextField)
  private
    _dataSource: id;
  public
    procedure setHasVerticalScroller(newValue: ObjCBOOL); message 'setHasVerticalScroller:';
    function hasVerticalScroller: ObjCBOOL; message 'hasVerticalScroller';
    procedure setIntercellSpacing(newValue: NSSize); message 'setIntercellSpacing:';
    function intercellSpacing: NSSize; message 'intercellSpacing';
    procedure setItemHeight(newValue: CGFloat); message 'setItemHeight:';
    function itemHeight: CGFloat; message 'itemHeight';
    procedure setNumberOfVisibleItems(newValue: NSInteger); message 'setNumberOfVisibleItems:';
    function numberOfVisibleItems: NSInteger; message 'numberOfVisibleItems';
    procedure setButtonBordered(newValue: ObjCBOOL); message 'setButtonBordered:';
    function isButtonBordered: ObjCBOOL; message 'isButtonBordered';
    procedure reloadData; message 'reloadData';
    procedure noteNumberOfItemsChanged; message 'noteNumberOfItemsChanged';
    procedure setUsesDataSource(newValue: ObjCBOOL); message 'setUsesDataSource:';
    function usesDataSource: ObjCBOOL; message 'usesDataSource';
    procedure scrollItemAtIndexToTop (index: NSInteger); message 'scrollItemAtIndexToTop:';
    procedure scrollItemAtIndexToVisible (index: NSInteger); message 'scrollItemAtIndexToVisible:';
    procedure selectItemAtIndex (index: NSInteger); message 'selectItemAtIndex:';
    procedure deselectItemAtIndex (index: NSInteger); message 'deselectItemAtIndex:';
    function indexOfSelectedItem: NSInteger; message 'indexOfSelectedItem';
    function numberOfItems: NSInteger; message 'numberOfItems';
    procedure setCompletes(newValue: ObjCBOOL); message 'setCompletes:';
    function completes: ObjCBOOL; message 'completes';
    function delegate_: NSComboBoxDelegateProtocol; message 'delegate';
    procedure setDelegate (anObject: NSComboBoxDelegateProtocol); message 'setDelegate:';
    procedure setDataSource(newValue: NSComboBoxDataSourceProtocol); message 'setDataSource:';
    function dataSource: NSComboBoxDataSourceProtocol; message 'dataSource';
    procedure addItemWithObjectValue (object_: id); message 'addItemWithObjectValue:';
    procedure addItemsWithObjectValues (objects: NSArray); message 'addItemsWithObjectValues:';
    procedure insertItemWithObjectValue_atIndex (object_: id; index: NSInteger); message 'insertItemWithObjectValue:atIndex:';
    procedure removeItemWithObjectValue (object_: id); message 'removeItemWithObjectValue:';
    procedure removeItemAtIndex (index: NSInteger); message 'removeItemAtIndex:';
    procedure removeAllItems; message 'removeAllItems';
    procedure selectItemWithObjectValue (object_: id); message 'selectItemWithObjectValue:';
    function itemObjectValueAtIndex (index: NSInteger): id; message 'itemObjectValueAtIndex:';
    function objectValueOfSelectedItem: id; message 'objectValueOfSelectedItem';
    function indexOfItemWithObjectValue (object_: id): NSInteger; message 'indexOfItemWithObjectValue:';
    function objectValues: NSArray; message 'objectValues';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  NSComboBoxDataSourceProtocol = objcprotocol external name 'NSComboBoxDataSource' (NSObjectProtocol)
  optional
    function numberOfItemsInComboBox (aComboBox: NSComboBox): NSInteger; message 'numberOfItemsInComboBox:';
    function comboBox_objectValueForItemAtIndex (aComboBox: NSComboBox; index: NSInteger): id; message 'comboBox:objectValueForItemAtIndex:';
    function comboBox_indexOfItemWithStringValue (aComboBox: NSComboBox; string_: NSString): NSUInteger; message 'comboBox:indexOfItemWithStringValue:';
    function comboBox_completedString (aComboBox: NSComboBox; string_: NSString): NSString; message 'comboBox:completedString:';
  end;

type
  NSComboBoxDelegateProtocol = objcprotocol external name 'NSComboBoxDelegate' (NSTextFieldDelegateProtocol)
  optional
    procedure comboBoxWillPopUp (notification: NSNotification); message 'comboBoxWillPopUp:';
    procedure comboBoxWillDismiss (notification: NSNotification); message 'comboBoxWillDismiss:';
    procedure comboBoxSelectionDidChange (notification: NSNotification); message 'comboBoxSelectionDidChange:';
    procedure comboBoxSelectionIsChanging (notification: NSNotification); message 'comboBoxSelectionIsChanging:';
  end;
{$endif}

