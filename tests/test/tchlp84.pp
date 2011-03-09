{ %NORUN }

{ test visibility of symbols in the extended type - strict protected }
program tchlp84;

{$ifdef fpc}
  {$mode delphi}
{$endif}
{$apptype console}

uses
  uchlp82;

type
  TFooHelper = class helper for TFoo
    function AccessField: Integer;
  end;

function TFooHelper.AccessField: Integer;
begin
  Result := Test3;
end;

begin

end.
