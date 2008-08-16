{$ifdef fpc}
{$mode delphi}
{$endif}

type

  { TMyObj }

  TMyObj = class
    procedure Proc(A1 : TObject; A2: Integer);
  end;

type
   TProc = procedure(AObject : TObject; A2: Integer) of object;

  
var X: TMyObj;
    P1: TProc;

procedure foo(const AMethod1);
begin
  if pointer(AMethod1) <> pointer(@P1) then
    halt(1);
end;

   
{ TMyObj }

procedure TMyObj.Proc(A1 : TObject; A2: Integer);
begin
end;


begin
  X := TMyObj.Create;
  P1 := X.Proc;
  foo(P1);
end.

