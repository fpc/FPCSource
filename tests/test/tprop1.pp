{$mode fpc}
var
  FErrno : longint;
function GetROVar:longint;
begin
  GetROVar:=3;
end;
function GetErrno:longint;
begin
  GetErrno:=FErrno;
end;
procedure SetErrno(e:longint);
begin
  FErrno:=e;
end;

property
  Errno:longint read GetErrno write SetErrno;
  ROVar:longint read GetROVar;

begin
  FErrno:=1;
  if Errno<>1 then
    begin
      writeln('Error 1');
      halt(1);
    end;
  Errno:=2;
  if Errno<>2 then
    begin
      writeln('Error 2');
      halt(1);
    end;
  if ROVar<>3 then
    begin
      writeln('Error 3');
      halt(1);
    end;
end.
