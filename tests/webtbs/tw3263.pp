{ Source provided for Free Pascal Bug Report 3263 }
{ Submitted by "Frank Kintrup" on  2004-08-20 }
{ e-mail: frank.kintrup@gmx.de }
{$MODE Delphi}
type
  TAncestor = class (TObject)
    constructor Create; virtual; overload;
  end;

type
  TDerived = class (TAncestor)
    constructor Create; override; overload;
    constructor Create(aParam : Integer); overload;
  end;

var
  err : boolean;

constructor TAncestor.Create;
begin
  writeln('TAnscestor.Create');
  err:=false;
end;

constructor TDerived.Create;
begin
  writeln('TDerived.Create');
  inherited Create;  // Calls TAncestor.Create
end;

constructor TDerived.Create(aParam : Integer);
begin
  // Should call virtual TDerived.Create
  // Compiler stops here "Illegal expression"
  writeln('TDerived.Create(aParam)');
  Create;
end;

var D : TDerived;
begin
  err:=true;
  D := TDerived.Create(0);
  if err then
    halt(1);
end.
