unit svrclass;

interface

type

  TServerClass = class
  public
    procedure WriteString(const s: String);
    function Add(Arg1, Arg2: Integer): Integer;
  end;


implementation

procedure TServerClass.WriteString(const s: String);
begin
  WriteLn('String: "', s, '"');
end;

function TServerClass.Add(Arg1, Arg2: Integer): Integer;
begin
  WriteLn('Adding ', Arg1, ' and ', Arg2);
  Result := Arg1 + Arg2;
end;

end.
