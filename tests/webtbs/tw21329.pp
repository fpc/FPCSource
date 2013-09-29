{$MODE DELPHI}
{$DEFINE CAUSE_ERROR}

type
  TArray<T> = array of T;

  TRecord = record end;

  TWrapper<T> = class
  strict private
  {$IFDEF CAUSE_ERROR}
    FRecords: TArray<TRecord>;
  {$ELSE}
    FRecords: array of TRecord;
  {$ENDIF}
  public
    constructor Create;
  end;

constructor TWrapper<T>.Create;
begin
  SetLength(FRecords, 1);
  with FRecords[0] do;
  // FRecords[0].x:=1;
end;

begin
  TWrapper<TRecord>.Create.Free;
end.
