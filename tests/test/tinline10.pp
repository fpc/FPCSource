{$inline on}

type
  tr = record
    l: longint;
  end;
  pr = ^tr;

procedure test(r: pr); inline;
begin
  with r^ do
    begin
      l:=5;
      exit;
    end;
end;

function f: longint;
var
  r: tr;
begin
  f:=1;
  test(@r);
  f:=2;
end;

begin
  if (f <> 2) then
    halt(1);
end.  
