{ Old file: tbs0005.pp }
{  tests the if 1=1 then ... bugs                       OK 0.9.2 }

uses
  erroru;

begin
  if 1=1 then
    begin
      Writeln('OK');
    end;
  if 1<>1 then
    begin
      Error;
    end;
end.
