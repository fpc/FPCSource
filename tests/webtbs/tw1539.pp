{ %interactive }
{ This test generates an error/retry box on windows }
{ Source provided for Free Pascal Bug Report 1539 }
{ Submitted by "Salvatore Licciardi" on  2001-06-30 }
{ e-mail: turylicciardi@tiscalinet.it }

{
If the floppy is protect to write, at the end
write(ItsPossible)=='TRUE' : ERROR
go32v2 on pure Dos specific
}
program MyProg;

uses
  dos,crt;

var
  copyfx:text;
  st:string;
  ItsPossible:boolean;

begin
  Writeln(' To test the bug, first insert a write protected floppy in a:');
  Write('Press Return when ready.');
  Readln(st);
  Assign(copyfx,'a:\w');
  ItsPossible:=true;

{$i-}
  rewrite(copyfx);
{$i+}
  if ioresult<>0 then
    ItsPossible:=false
  else
    begin
{$i-}
      close(copyfx);
{$i+}
      if IOResult<>0 then
        ItsPossible:=false;
    end;
  write(ItsPossible);
  if ItsPossible then
    begin
{$i-}
      writeln(copyfx,'Simple write test');
{$i+}
      Writeln('InOutRes after write atempt = ',IOResult);
      Close(copyfx);
    end;
end.
