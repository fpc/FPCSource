{ Parsed from Appkit.framework NSTableHeaderCell.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:06 CET 2010 }


{$ifdef TYPES}
{$ifndef NSTABLEHEADERCELL_PAS_T}
{$define NSTABLEHEADERCELL_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSTABLEHEADERCELL_PAS_R}
{$define NSTABLEHEADERCELL_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSTABLEHEADERCELL_PAS_F}
{$define NSTABLEHEADERCELL_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSTABLEHEADERCELL_PAS_S}
{$define NSTABLEHEADERCELL_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSTableHeaderCell = objcclass;
  NSTableHeaderCellPointer = ^NSTableHeaderCell;
  NSTableHeaderCellPtr = NSTableHeaderCellPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSTABLEHEADERCELL_PAS_C}
{$define NSTABLEHEADERCELL_PAS_C}

{ NSTableHeaderCell }
  NSTableHeaderCell = objcclass(NSTextFieldCell)
    
  public
    class function alloc: NSTableHeaderCell; message 'alloc';

    procedure drawSortIndicatorWithFrame_inView_ascending_priority(cellFrame: NSRect; controlView_: NSView; ascending: Boolean; priority: NSInteger); message 'drawSortIndicatorWithFrame:inView:ascending:priority:';
    function sortIndicatorRectForBounds(theRect: NSRect): NSRect; message 'sortIndicatorRectForBounds:';
  end; external;

{$endif}
{$endif}
