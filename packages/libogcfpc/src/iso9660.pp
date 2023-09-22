{$IFNDEF FPC_DOTTEDUNITS}
unit iso9660;
{$ENDIF FPC_DOTTEDUNITS}
{$mode objfpc} 
{$J+}
{$INLINE ON}
{$MACRO ON}
{$ASSERTIONS ON}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.CTypes, WiiApi.Gctypes, WiiApi.Gccore;
{$ELSE FPC_DOTTEDUNITS}
uses
  ctypes, gctypes, gccore;
{$ENDIF FPC_DOTTEDUNITS}

const
  ISO_MAXPATHLEN = 128;

function ISO9660_Mount(const name: pcchar; const disc_interface: PDISC_INTERFACE): cbool; cdecl; external;
function ISO9660_Unmount(const name: pcchar): cbool; cdecl; external;
function ISO9660_GetVolumeLabel(const name: pcchar): pcchar; cdecl; external;

implementation

initialization

end.
