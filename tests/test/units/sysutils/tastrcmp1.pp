program comp;
uses
   SysUtils;

var
  error : boolean;

procedure check(ok : boolean; func : string; value : longint);
begin
  if not ok then
  begin
    error:=true;
    writeln(func,' failed, result = ',value);
  end;
end;

var
  a, b: array[0..1] of char;
  tmp : longint;
begin
  error:=false;
  a[0] := #0; a[1] := #1;      //Empty string
  b[0] := #0; b[1] := #0;      //Empty string with different char after end
  tmp:=AnsiStrComp(a, b);      //should be zero because a=b
  check(tmp=0,'AnsiStrComp',tmp);
  tmp:=AnsiStrIComp(a, b);     //should be zero because a=b
  check(tmp=0,'AnsiStrIComp',tmp);
  if error then
    halt(1);
end.