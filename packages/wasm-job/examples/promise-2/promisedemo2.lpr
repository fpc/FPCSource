library promisedemo;
{$mode objfpc}
{$h+}
uses nothreads, sysutils, job.js, variants;

Type
  TFunction = function (const arguments: Variant): Variant of object;

  IJSWindow = interface(IJSObject)
    function setTimeout(const aHandler: TFunction; aTimeout: LongInt; const aArguments: Variant): LongInt{; ToDo:varargs};
    function setTimeout(const aHandler: TFunction): LongInt{; ToDo:varargs};
    function setTimeout(const aHandler: UnicodeString; aTimeout: LongInt; const aUnused: Variant): LongInt{; ToDo:varargs};
    function setTimeout(const aHandler: UnicodeString): LongInt{; ToDo:varargs};
  end;

  TJSWindow = class(TJSObject,IJSWindow)
    function setTimeout(const aHandler: TFunction; aTimeout: LongInt; const aArguments: Variant): LongInt{; ToDo:varargs};
    function setTimeout(const aHandler: TFunction): LongInt{; ToDo:varargs};
    function setTimeout(const aHandler: UnicodeString; aTimeout: LongInt; const aUnused: Variant): LongInt{; ToDo:varargs};
    function setTimeout(const aHandler: UnicodeString): LongInt{; ToDo:varargs};
  end;

  TApp = Class(TObject)
  private
    FOnResolve: TJSPromiseResolver;
    FOnReject: TJSPromiseResolver;
  public
    function DoTimeout(const arguments: Variant): Variant;
    function DoResolve(const aValue: Variant): Variant;
    procedure DoPromiseExecutor(const OnResolve, OnReject:TJSPromiseResolver);
    function ResolveTest: TJSPromise;
    procedure Run;
  end;

var
  JSWindow : IJSWindow;

function JOBCallFunction_(const aMethod: TMethod; var H: TJOBCallbackHelper): PByte;
var
  arguments: Variant;
begin
  arguments:=H.GetVariant;
  Result:=H.AllocVariant(TFunction(aMethod)(arguments));
end;


function TJSWindow.setTimeout(const aHandler: TFunction; aTimeout: LongInt; const aArguments: Variant): LongInt{; ToDo:varargs};
var
  m: TJOB_Method;
begin
  m:=TJOB_Method.Create(TMethod(aHandler),@JOBCallFunction_);
  try
    Result:=InvokeJSLongIntResult('setTimeout',[m,aTimeout,aArguments]);
  finally
    m.free;
  end;
end;
function TJSWindow.setTimeout(const aHandler: TFunction): LongInt{; ToDo:varargs};
var
  m: TJOB_Method;
begin
  m:=TJOB_Method.Create(TMethod(aHandler),@JOBCallFunction_);
  try
    Result:=InvokeJSLongIntResult('setTimeout',[m]);
  finally
    m.free;
  end;
end;
function TJSWindow.setTimeout(const aHandler: UnicodeString; aTimeout: LongInt; const aUnused: Variant): LongInt{; ToDo:varargs};
begin
  Result:=InvokeJSLongIntResult('setTimeout',[aHandler,aTimeout,aUnused]);
end;
function TJSWindow.setTimeout(const aHandler: UnicodeString): LongInt{; ToDo:varargs};
begin
  Result:=InvokeJSLongIntResult('setTimeout',[aHandler]);
end;


{ TApp }

function TApp.DoTimeout(const arguments: Variant): Variant;
begin
  if not Assigned(FOnResolve) then
    Writeln('Wasm ERROR: no resolve callback');
  if not Assigned(FOnReject) then
    Writeln('Wasm ERROR: no reject callback');

  if Assigned(FOnResolve) then
    FOnResolve('resolved');
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
  FOnResolve := OnResolve;
  FOnReject := OnReject;

  JSWindow.setTimeout(@DoTimeout, 1000, nil);
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
  JSWindow:=TJSWindow.JOBCreateGlobal('window');
  App:=TApp.Create;
  App.Run;
end.
