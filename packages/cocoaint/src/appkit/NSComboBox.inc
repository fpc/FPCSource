{ Parsed from Appkit.framework NSComboBox.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:07 ICT 2009 }


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
  NSComboBoxWillPopUpNotification: NSString; external name '_NSComboBoxWillPopUpNotification';
  NSComboBoxWillDismissNotification: NSString; external name '_NSComboBoxWillDismissNotification';
  NSComboBoxSelectionDidChangeNotification: NSString; external name '_NSComboBoxSelectionDidChangeNotification';
  NSComboBoxSelectionIsChangingNotification: NSString; external name '_NSComboBoxSelectionIsChangingNotification';

{$endif}
{$endif}

{$ifdef FORWARD}
  NSComboBox = objcclass;
  NSComboBoxPointer = ^NSComboBox;

{$endif}

{$ifdef CLASSES}
{$ifndef NSCOMBOBOX_PAS_C}
{$define NSCOMBOBOX_PAS_C}

{ NSComboBox }
  NSComboBox = objcclass(NSTextField)
  private
    __dataSource: id;
    
  public
    class function alloc: NSComboBox; message 'alloc';

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
    function dataSource: id; message 'dataSource';
    procedure setDataSource(aSource: id); message 'setDataSource:';
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
  end; external;

{$endif}
{$endif}
