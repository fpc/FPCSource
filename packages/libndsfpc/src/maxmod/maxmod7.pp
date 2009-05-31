unit maxmod7;
{$mode objfpc} 
{$apptype arm7}
{$define arm7}

{$J+}
{$INLINE ON}
{$MACRO ON}
{$PACKRECORDS C}

interface

uses
  ctypes, nds7;

{$linklib nds7}
{$linklib mm7}

{$linklib c}
{$linklib gcc}
{$linklib sysbase}

{$define NDS_INTERFACE}
{$include inc/mm_types.inc}
{$include inc/maxmod7.inc}
{$undef NDS_INTERFACE}

implementation
{$define NDS_IMPLEMENTATION}
{$include inc/maxmod7.inc}
{$undef NDS_IMPLEMENTATION}

end.
