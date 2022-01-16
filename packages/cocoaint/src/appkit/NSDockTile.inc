{ Parsed from AppKit.framework NSDockTile.h }


{$ifdef TYPES}
type
  NSDockTilePtr = ^NSDockTile;
  NSDockTilePlugInProtocolPtr = ^NSDockTilePlugInProtocol;
{$endif}

{$ifdef TYPES}
const
  NSAppKitVersionNumberWithDockTilePlugInSupport = 1001.0;
{$endif}

{$ifdef CLASSES}

type
  NSDockTile = objcclass external (NSObject)
  private
    _owner: id;
    _dockContextRef: pointer;
    _contentView: NSView;
    _frameView: NSView;
    _backstopView: NSView;
    _badgeLabel: NSString;
    _dFlags: bitpacked record
      case byte of
        0: (_anonBitField__dFlags0: cuint);
        1: (
          showsAppBadge: 0..1;
          reserved: 0..((1 shl 31)-1);
        );
      end;
    _dockTileSize: NSSize;
    _miniViewController: id;
    reserved:array[0..3] of id;
  public
    function size: NSSize; message 'size';
    procedure setContentView(newValue: NSView); message 'setContentView:';
    function contentView: NSView; message 'contentView';
    procedure display; message 'display';
    procedure setShowsApplicationBadge(newValue: ObjCBOOL); message 'setShowsApplicationBadge:';
    function showsApplicationBadge: ObjCBOOL; message 'showsApplicationBadge';
    procedure setBadgeLabel(newValue: NSString); message 'setBadgeLabel:';
    function badgeLabel: NSString; message 'badgeLabel';
    function owner: id; message 'owner';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  NSDockTilePlugInProtocol = objcprotocol external name 'NSDockTilePlugIn' (NSObjectProtocol)
  required
    procedure setDockTile (dockTile: NSDockTile); message 'setDockTile:';
  optional
    function dockMenu: NSMenu; message 'dockMenu';
  end;
{$endif}

