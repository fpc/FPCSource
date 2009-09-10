{ Parsed from Appkit.framework NSTableHeaderView.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:01 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSTABLEHEADERVIEW_PAS_H}
{$define NSTABLEHEADERVIEW_PAS_H}
type
  NSTableHeaderViewPointer = Pointer;

{$endif}
{$endif}

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
{$ifndef NSTABLEHEADERVIEW_PAS_T}
{$define NSTABLEHEADERVIEW_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSTableHeaderView = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSTABLEHEADERVIEW_PAS_C}
{$define NSTABLEHEADERVIEW_PAS_C}

{ NSTableHeaderView }
  NSTableHeaderView = objcclass(NSView)
  private
    __tableView: NSTableView;
    __resizedColumn: clong;
    __draggedColumn: clong;
    __pressedColumn: clong;
    __headerDragImage: NSImage;
    __draggedDistance: CGFloat;
    __isColumnResizing: Boolean;
    __showHandCursorFired: Boolean;
    __toolTipRectsDirty: Boolean;
    __reserved4: Boolean;
    __skipDrawingSeparator: Boolean;
    __reserved: id;
    
  public
    class function alloc: NSTableHeaderView; message 'alloc';

    procedure setTableView(tableView_: NSTableView); message 'setTableView:';
    function tableView: NSTableView; message 'tableView';
    function draggedColumn: clong; message 'draggedColumn';
    function draggedDistance: CGFloat; message 'draggedDistance';
    function resizedColumn: clong; message 'resizedColumn';
    function headerRectOfColumn(column: clong): NSRect; message 'headerRectOfColumn:';
    function columnAtPoint(point: NSPoint): clong; message 'columnAtPoint:';
  end; external;

{$endif}
{$endif}
