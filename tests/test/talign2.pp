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


{$mode objfpc}


procedure test(b : boolean);
begin
  if b then exit;
  WriteLn('Error in length!');
  halt(1);
end;

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
  test(length(widestr)=16);
  test(length(shortstr)=17);
end.  
{
   $Log$
   Revision 1.1  2002-11-09 13:18:25  carl
     * more alignment checking


}