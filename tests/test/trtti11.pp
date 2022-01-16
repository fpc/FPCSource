program trtti11;

{$MODE DELPHI}

uses
  SysUtils;

type
  PFoo = ^TFoo;
  TFoo = packed record
  public
    F: Integer;
    S: string;
  end;

var
  PF: PFoo;
begin
  try
    GetMem(PF, SizeOf(TFoo));
    InitializeArray(PF, TypeInfo(TFoo), 1);
    PF.S := 'foo';
    FinalizeArray(PF, TypeInfo(TFoo), 1);
    FreeMem(PF);
  except
    Halt(1);
  end;
end.
