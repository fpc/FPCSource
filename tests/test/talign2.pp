{ %VERSION=1.1 }
{%OPT=-Og}
{ This verifies if the strings are
  correctly aligned, normally the generated assembler
  should be verified manually.

  I consider this test as flawed, or is there a reason, why a
  shortstring should be aligned to pointer boundaries? (FK)
}
program talign2;

{$ifdef fpc}
{$mode objfpc}
{$define haswidestring}
{$ifdef go32v2}
  {$define USE_INTERNAL_UNICODE}
{$endif}

{$ifdef USE_INTERNAL_UNICODE}
  {$define USE_FPWIDESTRING_UNIT}
  {$define USE_UNICODEDUCET_UNIT}
  {$define USE_CPALL_UNIT}
{$endif}
{$else}
  {$ifndef ver70}
    {$define haswidestring}
  {$endif}
{$endif}

{$ifdef fpc}
{$ifndef USE_INTERNAL_UNICODE}
{$ifdef unix}
uses
  {$ifdef darwin}iosxwstr{$else}cwstring{$endif};
{$endif unix}
{$else USE_INTERNAL_UNICODE}
uses
 {$ifdef USE_UNICODEDUCET_UNIT}
  unicodeducet,
 {$endif}
 {$ifdef USE_FPWIDESTRING_UNIT}
  fpwidestring,
 {$endif}
 {$ifdef USE_CPALL_UNIT}
  cpall;
 {$endif}
{$endif USE_INTERNAL_UNICODE}
{$endif}

{$ifdef CPUI8086}
  { The pointer size is either 2 or 4
    depending on memory model, but
    there is no point in having better
    alignment than 2 for a pointer,
    as 4 byte pointer are in fact
    2 byte offset and 2 byte segment/selector }
const
  pointer_alignment = 2;
{$else}
const
  pointer_alignment = sizeof(pointer);
{$endif}


procedure test(b : boolean);
begin
  if b then exit;
  WriteLn('Error in length/alignment!!');
  halt(1);
end;

var
 pt: pchar;
const
  b: byte = 0;  { lets just misalign the stuff }
  p : pchar = 'simple pchar stuff';
  ansistr : ansistring = 'simple ansistring';
{$ifdef haswidestring}
  widestr : widestring = 'simple widestring';
{$endif}
  shortstr :shortstring = 'simple shortstring';
begin
  test(length(ansistr)=17);
{$ifdef haswidestring}
  test(length(widestr)=17);
{$endif}
  test(length(shortstr)=18);
  { verify if the address are correctly aligned! }
  pt:=@shortstr;
  test((ptruint(pt) mod pointer_alignment)=0);
  pt:=p;
  test((ptruint(pt) mod pointer_alignment)=0);
  pt:=pchar(ansistr);
  test((ptruint(pt) mod pointer_alignment)=0);
{$ifdef haswidestring}
  pt:=pchar(widestr);
{$ifdef FPC_WINLIKEWIDESTRING}
  test((ptruint(pt) mod 4)=0);
{$else FPC_WINLIKEWIDESTRING}
  test((ptruint(pt) mod pointer_alignment)=0);
{$endif FPC_WINLIKEWIDESTRING}
{$endif}
end.
