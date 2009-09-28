{ Parsed from Appkit.framework NSTableColumn.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Mon Sep 28 15:49:53 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSTABLECOLUMN_PAS_H}
{$define NSTABLECOLUMN_PAS_H}
type
  NSTableColumnPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSTABLECOLUMN_PAS_T}
{$define NSTABLECOLUMN_PAS_T}

{ Constants }

const
  NSTableColumnNoResizing = 0;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSTABLECOLUMN_PAS_R}
{$define NSTABLECOLUMN_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSTABLECOLUMN_PAS_F}
{$define NSTABLECOLUMN_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSTABLECOLUMN_PAS_S}
{$define NSTABLECOLUMN_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSTableColumn = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSTABLECOLUMN_PAS_C}
{$define NSTABLECOLUMN_PAS_C}

{ NSTableColumn }
  NSTableColumn = objcclass(NSObject, NSCodingProtocol)
  private
    __identifier: id;
    __width: CGFloat;
    __minWidth: CGFloat;
    __maxWidth: CGFloat;
    __tableView: NSTableView;
    __headerCell: NSCell;
    __dataCell: NSCell;
    __cFlags: bitpacked record
        oldIsResizable: 0..1;
        isEditable: 0..1;
        resizedPostingDisableCount: 0..((1 shl 8)-1);
        canUseReorderResizeImageCache: 0..1;
        userResizingAllowed: 0..1;
        autoResizingAllowed: 0..1;
        hidden: 0..1;
        RESERVED: 0..((1 shl 18)-1);
      end;
    __tcAuxiliaryStorage: id;
    
  public
    class function alloc: NSTableColumn; message 'alloc';

    function initWithIdentifier(identifier_: id): id; message 'initWithIdentifier:';
    procedure setIdentifier(identifier_: id); message 'setIdentifier:';
    function identifier: id; message 'identifier';
    procedure setTableView(tableView_: NSTableView); message 'setTableView:';
    function tableView: NSTableView; message 'tableView';
    procedure setWidth(width_: CGFloat); message 'setWidth:';
    function width: CGFloat; message 'width';
    procedure setMinWidth(minWidth_: CGFloat); message 'setMinWidth:';
    function minWidth: CGFloat; message 'minWidth';
    procedure setMaxWidth(maxWidth_: CGFloat); message 'setMaxWidth:';
    function maxWidth: CGFloat; message 'maxWidth';
    procedure setHeaderCell(cell: NSCell); message 'setHeaderCell:';
    function headerCell: id; message 'headerCell';
    procedure setDataCell(cell: NSCell); message 'setDataCell:';
    function dataCell: id; message 'dataCell';
    function dataCellForRow(row: NSInteger): id; message 'dataCellForRow:';
    procedure setEditable(flag: Boolean); message 'setEditable:';
    function isEditable: Boolean; message 'isEditable';
    procedure sizeToFit; message 'sizeToFit';
    procedure setSortDescriptorPrototype(sortDescriptor: NSSortDescriptor); message 'setSortDescriptorPrototype:';
    function sortDescriptorPrototype: NSSortDescriptor; message 'sortDescriptorPrototype';
    procedure setResizingMask(resizingMask_: NSUInteger); message 'setResizingMask:';
    function resizingMask: NSUInteger; message 'resizingMask';
    procedure setHeaderToolTip(string_: NSString); message 'setHeaderToolTip:';
    function headerToolTip: NSString; message 'headerToolTip';
    function isHidden: Boolean; message 'isHidden';
    procedure setHidden(hidden: Boolean); message 'setHidden:';
    procedure setResizable(flag: Boolean); message 'setResizable:';
    function isResizable: Boolean; message 'isResizable';
  end; external;

{$endif}
{$endif}
