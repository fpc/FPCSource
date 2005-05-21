{ %VERSION=1.1 }
{%OPT=-Og}
{ This verifies if the strings are
  correctly aligned, normally the generated assembler
  should be verified manually.
}
program talign2;

{$ifdef fpc}
{$mode objfpc}
  {$ifndef ver1_0}
    {$define haswidestring}
  {$endif}
{$else}
  {$ifndef ver70}
    {$define haswidestring}
  {$endif}
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
  test((ptruint(pt) mod sizeof(pointer))=0);
  pt:=p;
  test((ptruint(pt) mod sizeof(pointer))=0);
  pt:=pchar(ansistr);
  test((ptruint(pt) mod sizeof(pointer))=0);
{$ifdef haswidestring}
  pt:=pchar(widestr);
  test((ptruint(pt) mod sizeof(pointer))=0);
{$endif}
end.
