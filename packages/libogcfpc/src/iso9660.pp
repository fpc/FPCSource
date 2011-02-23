unit iso9660;
{$mode objfpc} 
{$J+}
{$INLINE ON}
{$MACRO ON}
{$ASSERTIONS ON}

interface

uses
  ctypes, gctypes;

const
  ISO_MAXPATHLEN = 128;

function ISO9660_Mount: cbool; cdecl; external;
function ISO9660_Unmount: cbool; cdecl; external;
function ISO9660_LastAccess: cuint64; cdecl; external;

implementation

initialization

end.
