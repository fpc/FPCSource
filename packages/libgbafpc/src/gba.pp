unit gba;
{$mode objfpc} 

{$J+}
{$INLINE ON}
{$MACRO ON}
{$PACKRECORDS C}

interface

uses
  ctypes;

{$linklib gba}

{$linklib c}
{$linklib gcc}
{$linklib g}
{$linklib sysbase}

{$define GBA_INTERFACE}
{$include gba/gba.inc}
{$undef GBA_INTERFACE}

implementation

{$define GBA_IMPLEMENTATION}
{$include gba/gba.inc}
{$undef GBA_IMPLEMENTATION}

end.
