{ Parsed from Appkit.framework NSDockTile.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:00 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSDOCKTILE_PAS_H}
{$define NSDOCKTILE_PAS_H}
type
  NSDockTilePointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSDOCKTILE_PAS_T}
{$define NSDOCKTILE_PAS_T}

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
{$ifndef NSDOCKTILE_PAS_T}
{$define NSDOCKTILE_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSDockTile = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSDOCKTILE_PAS_C}
{$define NSDOCKTILE_PAS_C}

{ NSDockTile }
  NSDockTile = objcclass(NSObject)
  private
    __owner: id;
    __dockContextRef: Pointer;
    __contentView: NSView;
    __frameView: NSView;
    __backstopView: NSView;
    __badgeLabel: NSString;
    __dFlags: bitpacked record
        showsAppBadge: 0..1;
        reserved: 0..((1 shl 31)-1);
      end;
    __dockTileSize: NSSize;
    _reserved: id;
    
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
