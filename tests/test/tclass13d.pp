{ %norun }
{$mode delphi}

type
  TObj = class
  const
    Val = 1;
    V1: Integer = Val;
    V2: Integer = TObj.Val;
  end;

begin
end.