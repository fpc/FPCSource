{$IFNDEF FPC_DOTTEDUNITS}
unit filesystem;
{$ENDIF FPC_DOTTEDUNITS}
{$mode objfpc} 
{$apptype arm9}
{$define arm9}

{$J+}
{$INLINE ON}
{$MACRO ON}
{$PACKRECORDS C}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.CTypes, NdsApi.Nds9;
{$ELSE FPC_DOTTEDUNITS}
uses
  ctypes, nds9;
{$ENDIF FPC_DOTTEDUNITS}

{$linklib filesystem}
{$linklib fat}
{$linklib nds9}

{$linklib c}
{$linklib gcc}
{$linklib sysbase}

{$define NDS_INTERFACE}
{ $include fathelper.inc}
{$include ../nds/disc_io.inc}
{$include filesystem.inc}
{$undef NDS_INTERFACE}

implementation
{$define NDS_IMPLEMENTATION}
{ $include fathelper.inc}
{$include ../nds/disc_io.inc}
{$include filesystem.inc}
{$undef NDS_IMPLEMENTATION}

initialization

finalization

end.
