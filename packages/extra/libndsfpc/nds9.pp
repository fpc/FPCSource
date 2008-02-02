unit nds9;
{$mode objfpc} 
{$apptype arm9}
{$define arm9}

{$J+}
{$INLINE ON}
{$MACRO ON}
{$PACKRECORDS C}

interface

uses
  ctypes;

{$linklib nds9}

{$linklib c}
{$linklib gcc}
{$linklib sysbase}

{$define NDS_INTERFACE}
{$include nds/ndsinclude.inc}
{$undef NDS_INTERFACE}

implementation

{$define NDS_IMPLEMENTATION}
{$include nds/ndsinclude.inc}
{$undef NDS_IMPLEMENTATION}

end.
