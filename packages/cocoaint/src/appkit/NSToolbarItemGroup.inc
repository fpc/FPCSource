{ Parsed from Appkit.framework NSToolbarItemGroup.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:07 ICT 2009 }


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

{$endif}

{$ifdef CLASSES}
{$ifndef NSTOOLBARITEMGROUP_PAS_C}
{$define NSTOOLBARITEMGROUP_PAS_C}

{ NSToolbarItemGroup }
  NSToolbarItemGroup = objcclass(NSToolbarItem)
  private
    __groupItems: id;
    __giFlags: bitpacked record
        dirtiedLayout: 0..1;
        reserved: 0..((1 shl 31)-1);
      end;
    __giReserved: id;
    
  public
    class function alloc: NSToolbarItemGroup; message 'alloc';

    procedure setSubitems(subitems_: NSArray); message 'setSubitems:';
    function subitems: NSArray; message 'subitems';
  end; external;

{$endif}
{$endif}
