{ Old file: tbs0007.pp }
{  tests the infinity loop when using byte counter     OK 0.9.2 }

uses
  erroru;

var
   count : byte;
   test : longint;
begin
   test:=0;
   for count:=1 to 127 do
     begin
        inc(test);
        writeln(count,'. loop');
        if test>127 then
          Error;
     end;
end.
