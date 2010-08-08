{$ifdef fpc}
{$mode delphi}
{$endif}

type
  tc = class
    a : longint;
    class procedure classmethod;
    procedure method;
  end;

  ttc = class of tc;

var
  l : longint;

class procedure tc.classmethod;
begin
  if l <> 1 then
    halt(1);
  l := 2;
end;

procedure tc.method;
begin
end;

var
  c: ttc;
begin
  c := tc;
  l := 1;
  with c do
    classmethod;
  if l <> 2 then
    halt(2);
end.
