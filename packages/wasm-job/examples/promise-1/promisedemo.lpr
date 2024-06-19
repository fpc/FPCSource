library promisedemo;
{$mode objfpc}
{$h+}
uses nothreads, sysutils, job.js, variants;


Type
  TApp = Class(TObject)
    function DoResolve(const aValue: Variant): Variant;
    procedure DoPromiseExecutor(const OnResolve, OnReject:TJSPromiseResolver);
    function ResolveTest: TJSPromise;
    procedure Run;
  end;

function TApp.DoResolve(const aValue: Variant): Variant;
begin
  Writeln('Wasm: in DoResolve: success. Argument vartype: ', vartype(aValue));
  if vartype(aValue)=varOleStr then
    Writeln('Wasm: DoResolve received value: ', VarToStr(aValue));
  result:=unassigned;
end;

procedure TApp.DoPromiseExecutor(const OnResolve, OnReject: TJSPromiseResolver);
begin
  Writeln('Wasm: in promise executor');
  if not Assigned(OnResolve) then
    Writeln('Wasm ERROR: no resolve callback');
  if not Assigned(OnReject) then
    Writeln('Wasm ERROR: no reject callback');
 if Assigned(OnResolve) then
    OnResolve('This is a success value');
end;

function TApp.ResolveTest: TJSPromise;
begin
  Result:=TJSPromise.Create(@DoPromiseExecutor);
end;

procedure TApp.Run;

Var
  P : TJSPromise;

begin
  try
    P:=ResolveTest;
    P._then(@DoResolve);
  except
    on E: Exception do
      Writeln(e.Message);
  end;
end;

var
  App : TApp;
begin
  App:=TApp.Create;
  App.Run;
end.

