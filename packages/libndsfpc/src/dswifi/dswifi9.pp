{$IFNDEF FPC_DOTTEDUNITS}
unit dswifi9;
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
{$linklib dswifi9}

{$linklib c}
{$linklib gcc}
{$linklib sysbase}

{$define NDS_INTERFACE}
{$include inc/dswifi9.inc}
{$undef NDS_INTERFACE}

implementation
{$define NDS_IMPLEMENTATION}
{$include inc/dswifi9.inc}
{$undef NDS_IMPLEMENTATION}

end.
