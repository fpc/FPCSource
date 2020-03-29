{%FAIL }

program treadonlydata2;

{$if defined(msdos) or defined(hasamiga) or defined(atari) or defined(palmos)}
  {$define target_does_not_support_rodata}
{$ekse}
  {$define target_supports_rodata}
{$endif}

{$mode  objfpc}
{$J-}
const
  c = 89;
  rc: LongInt = 5;
var
  p : pbyte;

begin
{$ifndef target_does_not_support_rodata}
  rc := 42;
{$else}
  { dummy code to also get a compile time error }
  p:=@c;
{$endif}
end.
