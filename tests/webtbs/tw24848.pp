{ %NORUN }

program project18;

{$mode delphi}

type
  TFoo<T> = class
    class constructor Create;
  end;

class constructor TFoo<T>.Create;
begin
end;

begin
end. // Error: Undefined symbol: P$PROJECT18$_$TFOO$1_$__$$_create 
