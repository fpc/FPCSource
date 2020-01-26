{ Parsed from AppKit.framework NSStatusBar.h }


{$ifdef TYPES}
type
  NSStatusBarPtr = ^NSStatusBar;
{$endif}

{$ifdef CLASSES}

type
  NSStatusBar = objcclass external (NSObject)
  private
    _items: id;
    _fReserved1: pointer;
    _fReserved2: pointer;
    _registeredForNote: NSInteger;
  public
    class function systemStatusBar: NSStatusBar; message 'systemStatusBar';
    function statusItemWithLength (length: CGFloat): NSStatusItem; message 'statusItemWithLength:';
    procedure removeStatusItem (item: NSStatusItem); message 'removeStatusItem:';
    function isVertical: ObjCBOOL; message 'isVertical';
    function thickness: CGFloat; message 'thickness';
  end;
{$endif}

