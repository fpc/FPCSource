{ Old file: tbs0212.pp }
{ problem with properties                              OK 0.99.11 (PFV) }

program proptest;

{$mode objfpc}

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
