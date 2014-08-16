{ %NORUN }

program tw21015;

{$mode delphi}

uses
  uw21015;

var
  x: IIntTest;
begin
  x := TGenImpl<Integer>.Create;
end.