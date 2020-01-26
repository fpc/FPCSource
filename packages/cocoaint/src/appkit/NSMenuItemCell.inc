{ Parsed from AppKit.framework NSMenuItemCell.h }


{ Types from NSMenuItemCell }
{$ifdef TYPES}

{$endif}


{$ifdef TYPES}
type
  NSMenuItemCellPtr = ^NSMenuItemCell;
{$endif}

{$ifdef CLASSES}

type
  NSMenuItemCell = objcclass external (NSButtonCell)
  private
    _extraData: id;
    _stateImageSize: NSSize;
    _imageSize: NSSize;
    _titleSize: NSSize;
    {$if  not defined(__LP64__)}
    _keyEquivalentSize: NSSize;
    {$endif}
    _size: NSSize;
    __micFlags: bitpacked record
      case byte of
        0: (_anonBitField___micFlags0: cuint);
        1: (
          needsSizing: 0..1;
          reserved: 0..1;
          needsDisplay: 0..1;
          keyEquivGlyphWidth: 0..((1 shl 16)-1);
          uniqueAgainstMain: 0..1;
          RESERVED_: 0..((1 shl 12)-1);
        );
      end;
  public
    procedure setMenuItem(newValue: NSMenuItem); message 'setMenuItem:';
    function menuItem: NSMenuItem; message 'menuItem';
    {$if  not defined(__LP64__)}
    procedure setMenuView (menuView: NSMenuView); message 'setMenuView:';
    function menuView: NSMenuView; message 'menuView';
    {$endif}
    procedure setNeedsSizing(newValue: ObjCBOOL); message 'setNeedsSizing:';
    function needsSizing: ObjCBOOL; message 'needsSizing';
    procedure calcSize; message 'calcSize';
    procedure setNeedsDisplay(newValue: ObjCBOOL); message 'setNeedsDisplay:';
    function needsDisplay: ObjCBOOL; message 'needsDisplay';
    function stateImageWidth: CGFloat; message 'stateImageWidth';
    function imageWidth: CGFloat; message 'imageWidth';
    function titleWidth: CGFloat; message 'titleWidth';
    function keyEquivalentWidth: CGFloat; message 'keyEquivalentWidth';
    function stateImageRectForBounds (cellFrame: NSRect): NSRect; message 'stateImageRectForBounds:';
    function titleRectForBounds (cellFrame: NSRect): NSRect; message 'titleRectForBounds:';
    function keyEquivalentRectForBounds (cellFrame: NSRect): NSRect; message 'keyEquivalentRectForBounds:';
    procedure drawSeparatorItemWithFrame_inView (cellFrame: NSRect; controlView_: NSView); message 'drawSeparatorItemWithFrame:inView:';
    procedure drawStateImageWithFrame_inView (cellFrame: NSRect; controlView_: NSView); message 'drawStateImageWithFrame:inView:';
    procedure drawImageWithFrame_inView (cellFrame: NSRect; controlView_: NSView); message 'drawImageWithFrame:inView:';
    procedure drawTitleWithFrame_inView (cellFrame: NSRect; controlView_: NSView); message 'drawTitleWithFrame:inView:';
    procedure drawKeyEquivalentWithFrame_inView (cellFrame: NSRect; controlView_: NSView); message 'drawKeyEquivalentWithFrame:inView:';
    procedure drawBorderAndBackgroundWithFrame_inView (cellFrame: NSRect; controlView_: NSView); message 'drawBorderAndBackgroundWithFrame:inView:';
    function tag: NSInteger; message 'tag';
  end;
{$endif}

