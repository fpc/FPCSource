{ Parsed from AppKit.framework NSBrowserCell.h }


{$ifdef TYPES}
type
  NSBrowserCellPtr = ^NSBrowserCell;
{$endif}

{$ifdef CLASSES}

type
  NSBrowserCell = objcclass external (NSCell)
  public
    class function branchImage: NSImage; message 'branchImage';
    class function highlightedBranchImage: NSImage; message 'highlightedBranchImage';
    function highlightColorInView (controlView_: NSView): NSColor; message 'highlightColorInView:';
    procedure setLeaf(newValue: ObjCBOOL); message 'setLeaf:';
    function isLeaf: ObjCBOOL; message 'isLeaf';
    procedure setLoaded(newValue: ObjCBOOL); message 'setLoaded:';
    function isLoaded: ObjCBOOL; message 'isLoaded';
    procedure reset; message 'reset';
    procedure set_; message 'set';
    procedure setImage(newValue: NSImage); message 'setImage:';
    function image: NSImage; message 'image';
    procedure setAlternateImage(newValue: NSImage); message 'setAlternateImage:';
    function alternateImage: NSImage; message 'alternateImage';
  end;
{$endif}

