{ Parsed from Appkit.framework NSTableHeaderView.h }

{$ifdef TYPES}
{$ifndef NSTABLEHEADERVIEW_PAS_T}
{$define NSTABLEHEADERVIEW_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSTABLEHEADERVIEW_PAS_R}
{$define NSTABLEHEADERVIEW_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSTABLEHEADERVIEW_PAS_F}
{$define NSTABLEHEADERVIEW_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSTABLEHEADERVIEW_PAS_S}
{$define NSTABLEHEADERVIEW_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSTableHeaderView = objcclass;
  NSTableHeaderViewPointer = ^NSTableHeaderView;
  NSTableHeaderViewPtr = NSTableHeaderViewPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSTABLEHEADERVIEW_PAS_C}
{$define NSTABLEHEADERVIEW_PAS_C}

{ NSTableHeaderView }
  NSTableHeaderView = objcclass external (NSView)
  private
    _tableView: NSTableView;
    _resizedColumn: NSInteger;
    _draggedColumn: NSInteger;
    _pressedColumn: NSInteger;
    _headerDragImage: NSImage;
    _draggedDistance: CGFloat;
    _isColumnResizing: Boolean;
    _showHandCursorFired: Boolean;
    _toolTipRectsDirty: Boolean;
    _alignTitleWithDataCell: Boolean;
    _skipDrawingSeparator: Boolean;
    _reserved: id;
    
  public
    procedure setTableView(tableView_: NSTableView); message 'setTableView:';
    function tableView: NSTableView; message 'tableView';
    function draggedColumn: NSInteger; message 'draggedColumn';
    function draggedDistance: CGFloat; message 'draggedDistance';
    function resizedColumn: NSInteger; message 'resizedColumn';
    function headerRectOfColumn(column: NSInteger): NSRect; message 'headerRectOfColumn:';
    function columnAtPoint(point: NSPoint): NSInteger; message 'columnAtPoint:';
  end;

{$endif}
{$endif}
