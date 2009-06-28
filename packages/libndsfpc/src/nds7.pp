unit nds7;
{$mode objfpc} 
{$apptype arm7}
{$define arm7}

{$J+}
{$INLINE ON}
{$MACRO ON}
{$PACKRECORDS C}

interface

uses
  ctypes;

{$linklib nds7}

{$linklib c}
{$linklib g}
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
