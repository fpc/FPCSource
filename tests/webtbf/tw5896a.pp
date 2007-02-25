{ %fail }
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

begin
end.
