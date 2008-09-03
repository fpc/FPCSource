unit uw11762;
interface
procedure p; inline;
var
  i : longint;
implementation
procedure p; inline;
label x;
begin
    goto x;
    i:=i+10;
    x:
    i:=i+1;
end;
begin
end.

