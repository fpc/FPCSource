{ %target=darwin }

uses
  sysutils;

var
  res: longint;
begin
  res:=executeprocess('./tweaklib2','1');
  if (res<>0) then
    begin
      writeln('error: ',res);
      halt(1);
    end;
end.

