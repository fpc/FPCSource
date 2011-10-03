unit iso9660;
{$mode objfpc} 
{$J+}
{$INLINE ON}
{$MACRO ON}
{$ASSERTIONS ON}

interface

uses
  ctypes, gctypes, gccore;

const
  ISO_MAXPATHLEN = 128;

function ISO9660_Mount(const name: pcchar; const disc_interface: PDISC_INTERFACE): cbool; cdecl; external;
function ISO9660_Unmount(const name: pcchar): cbool; cdecl; external;
function ISO9660_GetVolumeLabel(const name: pcchar): pcchar; cdecl; external;

implementation

initialization

end.
