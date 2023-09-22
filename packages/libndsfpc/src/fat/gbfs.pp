{$IFNDEF FPC_DOTTEDUNITS}
unit gbfs;
{$ENDIF FPC_DOTTEDUNITS}

{$error  gbfs is no longer supported on  nds}

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

{$linklib nds9}
{$linklib fat}

{$linklib c}
{$linklib gcc}
{$linklib sysbase}

{$define NDS_INTERFACE}
{$include gbfs.inc}
{$undef NDS_INTERFACE}

implementation
{$define NDS_IMPLEMENTATION}
{$include gbfs.inc}
{$undef NDS_IMPLEMENTATION}

end.
