program proptest;

type
  TMyRec = record
    Int: Integer;
    Str: String;
  end;

  TMyClass = class
  private
    FMyRec: TMyRec;
  public
    property AnInt: Integer read FMyRec.Int;
    property AStr: String read FMyRec.Str;
  end;

begin
end.