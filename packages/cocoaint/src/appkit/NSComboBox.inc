{ Parsed from Appkit.framework NSComboBox.h }

{$ifdef TYPES}
{$ifndef NSCOMBOBOX_PAS_T}
{$define NSCOMBOBOX_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSCOMBOBOX_PAS_R}
{$define NSCOMBOBOX_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSCOMBOBOX_PAS_F}
{$define NSCOMBOBOX_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSCOMBOBOX_PAS_S}
{$define NSCOMBOBOX_PAS_S}

{ External string constants }
var
  NSComboBoxWillPopUpNotification: NSString; cvar; external;
  NSComboBoxWillDismissNotification: NSString; cvar; external;
  NSComboBoxSelectionDidChangeNotification: NSString; cvar; external;
  NSComboBoxSelectionIsChangingNotification: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  NSComboBoxDataSourceProtocol = objcprotocol;
  NSComboBoxDelegateProtocol = objcprotocol;
  NSComboBox = objcclass;
  NSComboBoxPointer = ^NSComboBox;
  NSComboBoxPtr = NSComboBoxPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSCOMBOBOX_PAS_C}
{$define NSCOMBOBOX_PAS_C}

{ NSComboBox }
  NSComboBox = objcclass external (NSTextField)
  private
    _dataSource: id;
    
  public
    function hasVerticalScroller: Boolean; message 'hasVerticalScroller';
    procedure setHasVerticalScroller(flag: Boolean); message 'setHasVerticalScroller:';
    function intercellSpacing: NSSize; message 'intercellSpacing';
    procedure setIntercellSpacing(aSize: NSSize); message 'setIntercellSpacing:';
    function itemHeight: CGFloat; message 'itemHeight';
    procedure setItemHeight(itemHeight_: CGFloat); message 'setItemHeight:';
    function numberOfVisibleItems: NSInteger; message 'numberOfVisibleItems';
    procedure setNumberOfVisibleItems(visibleItems: NSInteger); message 'setNumberOfVisibleItems:';
    procedure setButtonBordered(flag: Boolean); message 'setButtonBordered:';
    function isButtonBordered: Boolean; message 'isButtonBordered';
    procedure reloadData; message 'reloadData';
    procedure noteNumberOfItemsChanged; message 'noteNumberOfItemsChanged';
    procedure setUsesDataSource(flag: Boolean); message 'setUsesDataSource:';
    function usesDataSource: Boolean; message 'usesDataSource';
    procedure scrollItemAtIndexToTop(index: NSInteger); message 'scrollItemAtIndexToTop:';
    procedure scrollItemAtIndexToVisible(index: NSInteger); message 'scrollItemAtIndexToVisible:';
    procedure selectItemAtIndex(index: NSInteger); message 'selectItemAtIndex:';
    procedure deselectItemAtIndex(index: NSInteger); message 'deselectItemAtIndex:';
    function indexOfSelectedItem: NSInteger; message 'indexOfSelectedItem';
    function numberOfItems: NSInteger; message 'numberOfItems';
    function completes: Boolean; message 'completes';
    procedure setCompletes(completes_: Boolean); message 'setCompletes:';
    function delegate: NSComboBoxDelegateProtocol; message 'delegate';
    procedure setDelegate(anObject: NSComboBoxDelegateProtocol); message 'setDelegate:';
    function dataSource: NSComboBoxDataSourceProtocol; message 'dataSource';
    procedure setDataSource(aSource: NSComboBoxDataSourceProtocol); message 'setDataSource:';
    procedure addItemWithObjectValue(object_: id); message 'addItemWithObjectValue:';
    procedure addItemsWithObjectValues(objects: NSArray); message 'addItemsWithObjectValues:';
    procedure insertItemWithObjectValue_atIndex(object_: id; index: NSInteger); message 'insertItemWithObjectValue:atIndex:';
    procedure removeItemWithObjectValue(object_: id); message 'removeItemWithObjectValue:';
    procedure removeItemAtIndex(index: NSInteger); message 'removeItemAtIndex:';
    procedure removeAllItems; message 'removeAllItems';
    procedure selectItemWithObjectValue(object_: id); message 'selectItemWithObjectValue:';
    function itemObjectValueAtIndex(index: NSInteger): id; message 'itemObjectValueAtIndex:';
    function objectValueOfSelectedItem: id; message 'objectValueOfSelectedItem';
    function indexOfItemWithObjectValue(object_: id): NSInteger; message 'indexOfItemWithObjectValue:';
    function objectValues: NSArray; message 'objectValues';
  end;

{$endif}
{$endif}
{$ifdef PROTOCOLS}
{$ifndef NSCOMBOBOX_PAS_P}
{$define NSCOMBOBOX_PAS_P}
  
{ NSComboBoxDataSource Protocol }
  NSComboBoxDataSourceProtocol = objcprotocol external name 'NSComboBoxDataSource'
  optional
    function numberOfItemsInComboBox(aComboBox: NSComboBox): NSInteger; message 'numberOfItemsInComboBox:';
    function comboBox_objectValueForItemAtIndex(aComboBox: NSComboBox; index: NSInteger): id; message 'comboBox:objectValueForItemAtIndex:';
    function comboBox_indexOfItemWithStringValue(aComboBox: NSComboBox; string_: NSString): NSUInteger; message 'comboBox:indexOfItemWithStringValue:';
    function comboBox_completedString(aComboBox: NSComboBox; string_: NSString): NSString; message 'comboBox:completedString:';
  end;
  
{ NSComboBoxDelegate Protocol }
  NSComboBoxDelegateProtocol = objcprotocol external name 'NSComboBoxDelegate'
  optional
    procedure comboBoxWillPopUp(notification: NSNotification); message 'comboBoxWillPopUp:';
    procedure comboBoxWillDismiss(notification: NSNotification); message 'comboBoxWillDismiss:';
    procedure comboBoxSelectionDidChange(notification: NSNotification); message 'comboBoxSelectionDidChange:';
    procedure comboBoxSelectionIsChanging(notification: NSNotification); message 'comboBoxSelectionIsChanging:';
  end;
{$endif}
{$endif}
