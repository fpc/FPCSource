{$ifdef fpc}{$Mode delphi}{$endif}
uses
  uw4056;

type
  TestC = class(TestB)
  public
    procedure TestProc; override;
    procedure TestProc(const C: Integer); override;
  end;

var
  X : TestC;

procedure TestC.TestProc;
begin
  inherited TestProc;
end;

procedure TestC.TestProc(const C: Integer);
begin
  inherited TestProc(C);
end;

begin
  X := TestC.Create;
  X.TestProc(10);
  X.Free;
end.
