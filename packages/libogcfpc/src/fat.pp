unit fat;
{$mode objfpc} 
{$J+}
{$INLINE ON}
{$MACRO ON}
{$ASSERTIONS ON}

interface

uses
  ctypes, gctypes, gccore;

function fatInit(cacheSize: cuint32; setAsDefaultDevice: cbool): cbool; cdecl; external;
function fatInitDefault: cbool; cdecl; external;

function fatMountSimple(name_: pcchar; interface_: PDISC_INTERFACE): cbool; cdecl; external;

function fatMount(name_: pcchar; interface_: PDISC_INTERFACE; startSector: sec_t; cacheSize, SectorsPerPage: cuint32): cbool; cdecl; external;
procedure fatUnmount(name_: pcchar); cdecl; external;
procedure fatGetVolumeLabel(name_, label_: pcchar); cdecl; external;

implementation

initialization
{$linklib fat}
end.
