{ Parsed from AppKit.framework NSComboBoxCell.h }


{$ifdef TYPES}
type
  NSComboBoxCellPtr = ^NSComboBoxCell;
  NSComboBoxCellDataSourceProtocolPtr = ^NSComboBoxCellDataSourceProtocol;
{$endif}

{$ifdef CLASSES}

type
  NSComboBoxCell = objcclass external (NSTextFieldCell)
  private
    _delegate: id;
    _dataSource: id;
    _cbcFlags: bitpacked record
      case byte of
        0: (_anonBitField__cbcFlags0: cuint);
        1: (
          usesDataSource: 0..1;
          completes: 0..1;
          buttonBordered: 0..1;
          popUpIsUp: 0..1;
          filteringEvents: 0..1;
          drawing: 0..1;
          synchronizingSelection: 0..1;
          reserved: 0..((1 shl 9)-1);
          visibleItems: 0..((1 shl 16)-1);
        );
      end;
    _buttonCell: NSButtonCell;
    _tableView: NSTableView;
    _scrollView: NSScrollView;
    _popUp: NSWindow;
    _popUpList: NSMutableArray;
    _cellFrame: NSRectPtr;
    _reserved: pointer;
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
    function completedString (string_: NSString): NSString; message 'completedString:';
    procedure setDataSource(newValue: NSComboBoxCellDataSourceProtocol); message 'setDataSource:';
    function dataSource: NSComboBoxCellDataSourceProtocol; message 'dataSource';
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
  NSComboBoxCellDataSourceProtocol = objcprotocol external name 'NSComboBoxCellDataSource' (NSObjectProtocol)
  optional
    function numberOfItemsInComboBoxCell (comboBoxCell: NSComboBoxCell): NSInteger; message 'numberOfItemsInComboBoxCell:';
    function comboBoxCell_objectValueForItemAtIndex (aComboBoxCell: NSComboBoxCell; index: NSInteger): id; message 'comboBoxCell:objectValueForItemAtIndex:';
    function comboBoxCell_indexOfItemWithStringValue (aComboBoxCell: NSComboBoxCell; string_: NSString): NSUInteger; message 'comboBoxCell:indexOfItemWithStringValue:';
    function comboBoxCell_completedString (aComboBoxCell: NSComboBoxCell; uncompletedString: NSString): NSString; message 'comboBoxCell:completedString:';
  end;
{$endif}

