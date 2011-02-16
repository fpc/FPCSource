{ Parsed from Appkit.framework NSBrowserCell.h }

{$ifdef TYPES}
{$ifndef NSBROWSERCELL_PAS_T}
{$define NSBROWSERCELL_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSBROWSERCELL_PAS_R}
{$define NSBROWSERCELL_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSBROWSERCELL_PAS_F}
{$define NSBROWSERCELL_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSBROWSERCELL_PAS_S}
{$define NSBROWSERCELL_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSBrowserCell = objcclass;
  NSBrowserCellPointer = ^NSBrowserCell;
  NSBrowserCellPtr = NSBrowserCellPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSBROWSERCELL_PAS_C}
{$define NSBROWSERCELL_PAS_C}

{ NSBrowserCell }
  NSBrowserCell = objcclass external (NSCell)
    
  public
    class function branchImage: NSImage; message 'branchImage';
    class function highlightedBranchImage: NSImage; message 'highlightedBranchImage';
    function highlightColorInView(controlView_: NSView): NSColor; message 'highlightColorInView:';
    function isLeaf: Boolean; message 'isLeaf';
    procedure setLeaf(flag: Boolean); message 'setLeaf:';
    function isLoaded: Boolean; message 'isLoaded';
    procedure setLoaded(flag: Boolean); message 'setLoaded:';
    procedure reset; message 'reset';
    procedure set_; message 'set';
    procedure setImage(image_: NSImage); message 'setImage:';
    function image: NSImage; message 'image';
    procedure setAlternateImage(newAltImage: NSImage); message 'setAlternateImage:';
    function alternateImage: NSImage; message 'alternateImage';
  end;

{$endif}
{$endif}
