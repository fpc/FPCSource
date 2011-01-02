{ Parsed from Appkit.framework NSComboBoxCell.h }
{ Version: 2.1.4 - Sun Jan 2 15:08:19 CET 2011 }


{$ifdef TYPES}
{$ifndef NSCOMBOBOXCELL_PAS_T}
{$define NSCOMBOBOXCELL_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSCOMBOBOXCELL_PAS_R}
{$define NSCOMBOBOXCELL_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSCOMBOBOXCELL_PAS_F}
{$define NSCOMBOBOXCELL_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSCOMBOBOXCELL_PAS_S}
{$define NSCOMBOBOXCELL_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSComboBoxCellDataSourceProtocol = objcprotocol;
  NSComboBoxCell = objcclass;
  NSComboBoxCellPointer = ^NSComboBoxCell;
  NSComboBoxCellPtr = NSComboBoxCellPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSCOMBOBOXCELL_PAS_C}
{$define NSCOMBOBOXCELL_PAS_C}

{ NSComboBoxCell }
  NSComboBoxCell = objcclass external (NSTextFieldCell)
  private
    _delegate: id;
    _dataSource: id;
    __cbcFlags: bitpacked record
      case byte of
       0: (_anonbitfield_NSComboBoxCell0: cuint);
       1: (
        _usesDataSource: 0..1;
        _completes: 0..1;
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
    _cellFrame: NSRectPtr; {garbage collector: __strong }
    _reserved: Pointer;
    
  public
  end;

{$endif}
{$endif}
{$ifdef PROTOCOLS}
{$ifndef NSCOMBOBOXCELL_PAS_P}
{$define NSCOMBOBOXCELL_PAS_P}
  
{ NSComboBoxCellDataSource Protocol }
  NSComboBoxCellDataSourceProtocol = objcprotocol external name 'NSComboBoxCellDataSource'
    function numberOfItemsInComboBoxCell(comboBoxCell: NSComboBoxCell): NSInteger; message 'numberOfItemsInComboBoxCell:';
    function comboBoxCell_objectValueForItemAtIndex(aComboBoxCell: NSComboBoxCell; index: NSInteger): id; message 'comboBoxCell:objectValueForItemAtIndex:';
    function comboBoxCell_indexOfItemWithStringValue(aComboBoxCell: NSComboBoxCell; string_: NSString): NSUInteger; message 'comboBoxCell:indexOfItemWithStringValue:';
    function comboBoxCell_completedString(aComboBoxCell: NSComboBoxCell; uncompletedString: NSString): NSString; message 'comboBoxCell:completedString:';
  end;
{$endif}
{$endif}
