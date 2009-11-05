{ Parsed from Appkit.framework NSMenuItemCell.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:07 ICT 2009 }


{$ifdef TYPES}
{$ifndef NSMENUITEMCELL_PAS_T}
{$define NSMENUITEMCELL_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSMENUITEMCELL_PAS_R}
{$define NSMENUITEMCELL_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSMENUITEMCELL_PAS_F}
{$define NSMENUITEMCELL_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSMENUITEMCELL_PAS_S}
{$define NSMENUITEMCELL_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSMenuItemCell = objcclass;
  NSMenuItemCellPointer = ^NSMenuItemCell;

{$endif}

{$ifdef CLASSES}
{$ifndef NSMENUITEMCELL_PAS_C}
{$define NSMENUITEMCELL_PAS_C}

{ NSMenuItemCell }
  NSMenuItemCell = objcclass(NSButtonCell)
  private
    __extraData: id;
    __stateImageSize: NSSize;
    __imageSize: NSSize;
    __titleSize: NSSize;
    __keyEquivalentSize: NSSize;
    __size: NSSize;
    __micFlags: bitpacked record
        needsSizing: 0..1;
        reserved: 0..1;
        needsDisplay: 0..1;
        keyEquivGlyphWidth: 0..((1 shl 16)-1);
        RESERVED_: 0..((1 shl 13)-1);
      end;
    
  public
    class function alloc: NSMenuItemCell; message 'alloc';

    procedure setMenuItem(item: NSMenuItem); message 'setMenuItem:';
    function menuItem: NSMenuItem; message 'menuItem';
    procedure setMenuView(menuView_: NSMenuView); message 'setMenuView:';
    function menuView: NSMenuView; message 'menuView';
    procedure setNeedsSizing(flag: Boolean); message 'setNeedsSizing:';
    function needsSizing: Boolean; message 'needsSizing';
    procedure calcSize; message 'calcSize';
    procedure setNeedsDisplay_(flag: Boolean); message 'setNeedsDisplay:';
    function needsDisplay: Boolean; message 'needsDisplay';
    function stateImageWidth: CGFloat; message 'stateImageWidth';
    function imageWidth: CGFloat; message 'imageWidth';
    function titleWidth: CGFloat; message 'titleWidth';
    function keyEquivalentWidth: CGFloat; message 'keyEquivalentWidth';
    function stateImageRectForBounds(cellFrame: NSRect): NSRect; message 'stateImageRectForBounds:';
    function titleRectForBounds(cellFrame: NSRect): NSRect; message 'titleRectForBounds:';
    function keyEquivalentRectForBounds(cellFrame: NSRect): NSRect; message 'keyEquivalentRectForBounds:';
    procedure drawSeparatorItemWithFrame_inView(cellFrame: NSRect; controlView_: NSView); message 'drawSeparatorItemWithFrame:inView:';
    procedure drawStateImageWithFrame_inView(cellFrame: NSRect; controlView_: NSView); message 'drawStateImageWithFrame:inView:';
    procedure drawImageWithFrame_inView(cellFrame: NSRect; controlView_: NSView); message 'drawImageWithFrame:inView:';
    procedure drawTitleWithFrame_inView(cellFrame: NSRect; controlView_: NSView); message 'drawTitleWithFrame:inView:';
    procedure drawKeyEquivalentWithFrame_inView(cellFrame: NSRect; controlView_: NSView); message 'drawKeyEquivalentWithFrame:inView:';
    procedure drawBorderAndBackgroundWithFrame_inView(cellFrame: NSRect; controlView_: NSView); message 'drawBorderAndBackgroundWithFrame:inView:';
    function tag: NSInteger; message 'tag';
  end; external;

{$endif}
{$endif}
