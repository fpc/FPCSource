{ Parsed from Appkit.framework NSToolbarItemGroup.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:06 CET 2010 }


{$ifdef TYPES}
{$ifndef NSTOOLBARITEMGROUP_PAS_T}
{$define NSTOOLBARITEMGROUP_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSTOOLBARITEMGROUP_PAS_R}
{$define NSTOOLBARITEMGROUP_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSTOOLBARITEMGROUP_PAS_F}
{$define NSTOOLBARITEMGROUP_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSTOOLBARITEMGROUP_PAS_S}
{$define NSTOOLBARITEMGROUP_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSToolbarItemGroup = objcclass;
  NSToolbarItemGroupPointer = ^NSToolbarItemGroup;
  NSToolbarItemGroupPtr = NSToolbarItemGroupPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSTOOLBARITEMGROUP_PAS_C}
{$define NSTOOLBARITEMGROUP_PAS_C}

{ NSToolbarItemGroup }
  NSToolbarItemGroup = objcclass(NSToolbarItem)
  private
    _groupItems: id;
    __giFlags: bitpacked record
      case byte of
       0: (_anonbitfield_NSToolbarItemGroup0: cuint);
       1: (
        dirtiedLayout: 0..1;
        reserved: 0..((1 shl 31)-1);
      );
      end;
    _giReserved: id;
    
  public
    class function alloc: NSToolbarItemGroup; message 'alloc';

    procedure setSubitems(subitems_: NSArray); message 'setSubitems:';
    function subitems: NSArray; message 'subitems';
  end; external;

{$endif}
{$endif}
