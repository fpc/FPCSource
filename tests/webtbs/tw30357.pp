program tw30357;

{$mode delphi}

type
  TMyRecord = record
  private
    class function GetEmpty: TMyRecord; static;
  public
    class property Empty: TMyRecord read GetEmpty;
  private
    FData: IInterface;
  end;

class function TMyRecord.GetEmpty: TMyRecord; static;
begin
end;

procedure Main2(Sender: TObject);
var
  v1: PtrUInt;
begin
  v1 := 42;
end;

procedure Main(Sender: TObject);
var
  v1: TMyRecord;
begin
  if v1.FData <> nil then
    Halt(1);
end;

begin
  { with Main2 we ensure that the stack area is not 0 }
  Main2(nil);
  Main(nil);
end.
