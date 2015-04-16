{ %norun }

program project1;

{$mode delphi}

type
  TSomeRecord = record
    Value: Integer;
  const
    Invalid: TSomeRecord = (Value: 25);
  end;

begin
end.

