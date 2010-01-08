{ %fail }

program statictest;

{$mode delphi}

type
  TMyClass = class
  public
    class procedure StaticCall; static;
  end;

class procedure TMyClass.StaticCall;
begin
  WriteLn('Static method was called!');
  writeln(ptruint(self));
end;

begin
  TMyClass.StaticCall;
end.
