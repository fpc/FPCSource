program tgeneric35;

{$mode delphi}

type
  TSomeRecord = record
    F: Integer;
  end;

  TSomeRecord<TData> = record
    data: TData;
  end;
var
  R1: TSomeRecord;
  R2: TSomeRecord<Integer>;
begin
  R1.F := 2;
  R2.data := 1;
end.
