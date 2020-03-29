{ %FAIL }

{ the type parameters of the implementation need to match those in the interface }
unit tgeneric105;

{$mode delphi}

interface

type
  TTest<T> = class
    procedure Test;
  end;

implementation

procedure TTest<S>.Test;
begin
end;

end.

