type
  rec=record
    a : longint;
  end;

var
  r1 : rec absolute $40:$49;
begin
  with r1 do
   a:=1;
end.
