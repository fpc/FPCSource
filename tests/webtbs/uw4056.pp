{$ifdef fpc}{$Mode delphi}{$endif}
unit uw4056;

interface

type
  TestA = class
  protected
    procedure TestProc; overload; virtual;
    procedure TestProc(const C: Integer); overload; virtual;
  end;
  TestB = class(TestA)
  public
    procedure TestProc; override;
  end;

implementation

procedure TestA.TestProc;
begin
end;

procedure TestA.TestProc(const C: Integer);
begin
  writeln(C);
end;

procedure TestB.TestProc;
begin
end;

end.
