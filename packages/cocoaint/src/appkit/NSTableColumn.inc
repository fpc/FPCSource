{ Parsed from AppKit.framework NSTableColumn.h }


{$ifdef TYPES}
type
  NSTableColumnPtr = ^NSTableColumn;
{$endif}

{$ifdef TYPES}
type
  NSTableColumnResizingOptions = NSUInteger;
  NSTableColumnResizingOptionsPtr = ^NSTableColumnResizingOptions;

const
  NSTableColumnNoResizing = 0;
  NSTableColumnAutoresizingMask = 1 shl 0;
  NSTableColumnUserResizingMask = 1 shl 1;
{$endif}

{$ifdef CLASSES}

type
  NSTableColumn = objcclass external (NSObject, NSCodingProtocol, NSUserInterfaceItemIdentificationProtocol)
  private
    _identifier: id;
    _width: CGFloat;
    _minWidth: CGFloat;
    _maxWidth: CGFloat;
    _tableView: NSTableView;
    _headerCell: NSCell;
    _dataCell: NSCell;
    _cFlags: bitpacked record
      case byte of
        0: (_anonBitField__cFlags0: cuint);
        1: (
          oldIsResizable: 0..1;
          isEditable: 0..1;
          resizedPostingDisableCount: 0..((1 shl 8)-1);
          canUseReorderResizeImageCache: 0..1;
          userResizingAllowed: 0..1;
          autoResizingAllowed: 0..1;
          hidden: 0..1;
          RESERVED: 0..((1 shl 18)-1);
        );
      end;
    _tcAuxiliaryStorage: id;
  public
    function initWithIdentifier (identifier: NSString): instancetype; message 'initWithIdentifier:';
    procedure setIdentifier(newValue: NSString); message 'setIdentifier:';
    function identifier: NSString; message 'identifier';
    procedure setTableView(newValue: NSTableView); message 'setTableView:';
    function tableView: NSTableView; message 'tableView';
    procedure setWidth(newValue: CGFloat); message 'setWidth:';
    function width: CGFloat; message 'width';
    procedure setMinWidth(newValue: CGFloat); message 'setMinWidth:';
    function minWidth: CGFloat; message 'minWidth';
    procedure setMaxWidth(newValue: CGFloat); message 'setMaxWidth:';
    function maxWidth: CGFloat; message 'maxWidth';
    procedure setTitle(newValue: NSString); message 'setTitle:';
    function title: NSString; message 'title';
    procedure setHeaderCell(newValue: id); message 'setHeaderCell:';
    function headerCell: id; message 'headerCell';
    procedure setEditable(newValue: ObjCBOOL); message 'setEditable:';
    function isEditable: ObjCBOOL; message 'isEditable';
    procedure sizeToFit; message 'sizeToFit';
    procedure setSortDescriptorPrototype(newValue: NSSortDescriptor); message 'setSortDescriptorPrototype:';
    function sortDescriptorPrototype: NSSortDescriptor; message 'sortDescriptorPrototype';
    procedure setResizingMask(newValue: NSTableColumnResizingOptions); message 'setResizingMask:';
    function resizingMask: NSTableColumnResizingOptions; message 'resizingMask';
    procedure setHeaderToolTip(newValue: NSString); message 'setHeaderToolTip:';
    function headerToolTip: NSString; message 'headerToolTip';
    procedure setHidden(newValue: ObjCBOOL); message 'setHidden:';
    function isHidden: ObjCBOOL; message 'isHidden';

    { Adopted protocols }
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
  end;


type
  NSTableColumn_NSDeprecated = objccategory external name 'NSDeprecated' (NSTableColumn)
    procedure setResizable (flag: ObjCBOOL); message 'setResizable:'; deprecated 'in 10_0, 10_4';
    function isResizable: ObjCBOOL; message 'isResizable'; deprecated 'in 10_0, 10_4';
    procedure setDataCell(newValue: id); message 'setDataCell:';
    function dataCell: id; message 'dataCell';
    function dataCellForRow (row: NSInteger): id; message 'dataCellForRow:';
  end;
{$endif}

