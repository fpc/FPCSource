{ Old file: tbs0190.pp }
{ can't have typecast for var params ??                 OK 0.99.11 (PM) }

procedure a(var b: boolean);
begin
  b:=true;
end;

var C: byte;

begin
  a(boolean(c));
end.
