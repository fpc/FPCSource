{ %norun }

{$mode objfpc}
{$h+}

type
  TMyClass = class
    function ToString: String; override;
  end;

function TMyClass.ToString: String;
begin
  Result:=inherited;
end;

begin
end.
