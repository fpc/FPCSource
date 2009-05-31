unit fat;
{$mode objfpc} 
{$apptype arm9}
{$define arm9}

{$J+}
{$INLINE ON}
{$MACRO ON}
{$PACKRECORDS C}

interface

uses
  ctypes, nds9;

{$linklib fat}
{$linklib nds9}

{$linklib c}
{$linklib gcc}
{$linklib sysbase}

{$define NDS_INTERFACE}
{$include fathelper.inc}
{$include ../nds/disc_io.inc}
{$include fat.inc}
{$undef NDS_INTERFACE}

implementation
{$define NDS_IMPLEMENTATION}
{$include fathelper.inc}
{$include ../nds/disc_io.inc}
{$include fat.inc}
{$undef NDS_IMPLEMENTATION}

initialization

finalization

end.
