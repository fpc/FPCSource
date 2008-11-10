{ %opt=-Seh -vh }

{$mode delphi}
program TestMismatch;

type
  TTestClass = class(TObject)
  public
    procedure AProc(AValue : Single);
  end;

procedure TTestClass.AProc(AValue : Single);
begin
  WriteLn(AValue);
end;

var
  AnObj : TTestClass;
  ASingle : Single;
  
begin
  AnObj := TTestClass.Create;
  ASingle := 1;
  AnObj.AProc(ASingle * 2); //Does not generate warning
  AnObj.AProc(ASingle * 2.0); //Generates warning with -Cr or -vh
end.

