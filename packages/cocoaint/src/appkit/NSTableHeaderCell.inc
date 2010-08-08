{ Parsed from Appkit.framework NSTableHeaderCell.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:07 ICT 2009 }


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
