{$IFNDEF FPC_DOTTEDUNITS}
unit maxmod9;
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

{$linklib nds9}
{$linklib mm9}

{$linklib c}
{$linklib gcc}
{$linklib sysbase}

{$define NDS_INTERFACE}
{$include inc/mm_types.inc}
{$include inc/maxmod9.inc}
{$undef NDS_INTERFACE}

implementation
{$define NDS_IMPLEMENTATION}
{$include inc/maxmod9.inc}
{$undef NDS_IMPLEMENTATION}

end.
