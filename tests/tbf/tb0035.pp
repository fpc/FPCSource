{ %TARGET=go32v2 }
{ %FAIL }
{ Old file: tbf0172.pp }
{ with with absolute seg:ofs should not be possible OK 0.99.9 (PM) }

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
