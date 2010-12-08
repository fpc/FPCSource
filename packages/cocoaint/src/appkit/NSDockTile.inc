{ Parsed from Appkit.framework NSDockTile.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:06 CET 2010 }


{$ifdef TYPES}
{$ifndef NSDOCKTILE_PAS_T}
{$define NSDOCKTILE_PAS_T}

{ Defines }
const
  NSAppKitVersionNumberWithDockTilePlugInSupport = 1001.0;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSDOCKTILE_PAS_R}
{$define NSDOCKTILE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSDOCKTILE_PAS_F}
{$define NSDOCKTILE_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSDOCKTILE_PAS_S}
{$define NSDOCKTILE_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSDockTilePlugInProtocol = objcprotocol;
  NSDockTile = objcclass;
  NSDockTilePointer = ^NSDockTile;
  NSDockTilePtr = NSDockTilePointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSDOCKTILE_PAS_C}
{$define NSDOCKTILE_PAS_C}

{ NSDockTile }
  NSDockTile = objcclass(NSObject)
  private
    _owner: id;
    _dockContextRef: Pointer;
    _contentView: NSView;
    _frameView: NSView;
    _backstopView: NSView;
    _badgeLabel: NSString;
    __dFlags: bitpacked record
      case byte of
       0: (_anonbitfield_NSDockTile0: cuint);
       1: (
        showsAppBadge: 0..1;
        reserved: 0..((1 shl 31)-1);
      );
      end;
    _dockTileSize: NSSize;
        reserved: array[0..4] of id;
    
  public
    class function alloc: NSDockTile; message 'alloc';

    function size: NSSize; message 'size';
    procedure setContentView(view: NSView); message 'setContentView:';
    function contentView: NSView; message 'contentView';
    procedure display; message 'display';
    procedure setShowsApplicationBadge(flag: Boolean); message 'setShowsApplicationBadge:';
    function showsApplicationBadge: Boolean; message 'showsApplicationBadge';
    procedure setBadgeLabel(string_: NSString); message 'setBadgeLabel:';
    function badgeLabel: NSString; message 'badgeLabel';
    function owner: id; message 'owner';
  end; external;

{$endif}
{$endif}
{$ifdef PROTOCOLS}
{$ifndef NSDOCKTILE_PAS_P}
{$define NSDOCKTILE_PAS_P}
  
{ NSDockTilePlugIn Protocol }
  NSDockTilePlugInProtocol = objcprotocol
    procedure setDockTile(dockTile: NSDockTile); message 'setDockTile:';
    function dockMenu: NSMenu; message 'dockMenu';
  end; external name 'NSDockTilePlugIn';
{$endif}
{$endif}
