{$mode delphi}
type
  tc1 = class
    procedure p;virtual;abstract;
  end;

  tc2 = class(tc1)
    procedure p;override;
  end;

procedure tc2.p;
  begin
    inherited;
  end;

var
  c2 : tc2;
begin
  c2:=tc2.create;
  c2.p;
  c2.free;
end.
