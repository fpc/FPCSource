{$mode objfpc}
{$h+}
Program testp;

uses sysutils,strutils;

Const
  Count = 10000000;
  Delims = [' ','.'];
Var
  S,T : String;
  I,N : Integer;

begin
  S:='THE CAT WAS NEVER SMART ENOUGH TO CATCH THIS FAST LITTLE MOUSE.';
  For I:=1 to 8 do
    Write('1234567890');
  Writeln;
  Writeln(S);
  For I:=1 to WordCount(S,Delims) do
    begin
    T:=ExtractWordPos(I,S,Delims,N);
    Writeln('Word ',I:2,' starts at ',N:2,'(',WordPosition(I,S,Delims):2,') : ',T);
    end;
end.                                                