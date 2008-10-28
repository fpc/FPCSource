{ %cpu=i386 }
{ %target=win32 }

{compilation: fpc test.pp}
{$IFDEF FPC}
{$MODE DELPHI}
{$ASMMODE Intel}
{$ELSE}
{$APPTYPE CONSOLE}
{$ENDIF}
type
  TBig=record
    data:array[1..1000] of integer;
  end;
  TBig2=array[1..1000] of integer;
var
  s,s1:integer;
  x:TBig;
  x2:TBig2;
  err : boolean;
procedure temp(x:TBig);stdcall;
begin
  asm
    mov s,ebp
  end;
end;
procedure temp2(x:TBig2);stdcall;
begin
  asm
    mov s,ebp
  end;
end;
begin
  asm
    mov s1,esp
  end;
  writeln(s1);
  temp(x);
  writeln(s);
  if (s1-s)<1000 then
    begin
      writeln('incompatible with Delphi: records');
      err:=true;
    end;

  asm
    mov s1,esp
  end;
  writeln(s1);
  temp2(x2);
  writeln(s);
  if (s1-s)>1000 then
    begin
      writeln('incompatible with Delphi: arrays');
      err:=true;
    end;
  if err then
    halt(1);
end.
