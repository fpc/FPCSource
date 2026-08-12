program vmidemo;

(*
  Demonstrates TVirtualMethodInterceptor: it routes the virtual method calls of
  an instance through a runtime-generated proxy class, so they can be logged,
  modified, suppressed or have their exceptions handled without changing the
  original class.

  Note: only virtual methods that have RTTI are intercepted. 
  But unlike Delphi, FPC does not emit method RTTI by default, hence the 
    {$RTTI EXPLICIT METHODS[...]}
  directive on TCalculator below. 
  Without it the program still runs, but the events do not fire. 
*)  

{$mode objfpc}{$h+}

uses
{$ifdef unix}
  ffi.manager,
{$endif}  
  SysUtils, Rtti;

type
  ECalculator = class(Exception);

  {$RTTI EXPLICIT METHODS[vcPrivate,vcProtected,vcPublic,vcPublished]}

  { TCalculator }

  TCalculator = class
  public
    // Adds two numbers.
    function Add(aA, aB: Integer): Integer; virtual;
    // Divides aA by aB; raises ECalculator on division by zero.
    function Divide(aA, aB: Integer): Integer; virtual;
  end;

  { TInterceptLogger: supplies the interception event handlers. }

  TInterceptLogger = class
  public
    // Logs and optionally short-circuits the call before it is invoked.
    procedure BeforeCall(aInstance: TObject; aMethod: TRttiMethod; const aArgs: TValueArray; out aDoInvoke: Boolean; out aResult: TValue);
    // Logs and may modify the result after the call returned.
    procedure AfterCall(aInstance: TObject; aMethod: TRttiMethod; const aArgs: TValueArray; var aResult: TValue);
    // Logs and may swallow an exception raised by the call.
    procedure ExceptionCall(aInstance: TObject; aMethod: TRttiMethod; const aArgs: TValueArray; out aRaiseException: Boolean; aTheException: Exception; out aResult: TValue);
  end;

{ TCalculator }

function TCalculator.Add(aA, aB: Integer): Integer;

begin
  Result:=aA + aB;
end;


function TCalculator.Divide(aA, aB: Integer): Integer;

begin
  if aB = 0 then
    raise ECalculator.Create('division by zero');
  Result:=aA div aB;
end;


{ TInterceptLogger }

procedure TInterceptLogger.BeforeCall(aInstance: TObject; aMethod: TRttiMethod; const aArgs: TValueArray; out aDoInvoke: Boolean; out aResult: TValue);

var
  lArg: TValue;
  lLine: String;

begin
  lLine:='  before ' + aMethod.Name + '(';
  for lArg in aArgs do
    lLine:=lLine + ' ' + lArg.ToString;
  Writeln(lLine + ' )');
  aDoInvoke:=True;
  aResult:=TValue.Empty;
end;


procedure TInterceptLogger.AfterCall(aInstance: TObject; aMethod: TRttiMethod; const aArgs: TValueArray; var aResult: TValue);

begin
  Writeln('  after  ', aMethod.Name, ' -> ', aResult.ToString);
  { Double whatever the original method returned. }
  if not aResult.IsEmpty then
    aResult:=aResult.AsInteger * 2;
end;


procedure TInterceptLogger.ExceptionCall(aInstance: TObject; aMethod: TRttiMethod; const aArgs: TValueArray; out aRaiseException: Boolean; aTheException: Exception; out aResult: TValue);

begin
  Writeln('  caught exception in ', aMethod.Name, ': ', aTheException.Message);
  { Swallow the exception and return 0 instead. }
  aRaiseException:=False;
  aResult:=0;
end;


var
  Calc: TCalculator;
  Logger: TInterceptLogger;
  Interceptor: TVirtualMethodInterceptor;
  Res: Integer;

begin
  Logger:=TInterceptLogger.Create;
  Calc:=TCalculator.Create;
  try
    try
      Interceptor:=TVirtualMethodInterceptor.Create(TCalculator);
    except
      { Method implementations need a function call manager, which is not
        available on every platform. }
      on E: ENotImplemented do
        begin
        Writeln('Interception not supported on this platform: ', E.Message);
        Halt(0);
        end;
    end;
    try
      Interceptor.OnBefore:=@Logger.BeforeCall;
      Interceptor.OnAfter:=@Logger.AfterCall;
      Interceptor.OnException:=@Logger.ExceptionCall;

      Interceptor.Proxify(Calc);

      Writeln('Add(2, 3):');
      Res:=Calc.Add(2, 3);
      Writeln('result = ', Res, ' (original 5, doubled by OnAfter)');

      Writeln('Divide(10, 0):');
      Res:=Calc.Divide(10, 0);
      Writeln('result = ', Res, ' (exception swallowed by OnException, doubled by OnAfter)');

      Interceptor.Unproxify(Calc);
      Writeln('After Unproxify, Add(2, 3) = ', Calc.Add(2, 3), ' (no interception)');
    finally
      Interceptor.Free;
    end;
  finally
    Calc.Free;
    Logger.Free;
  end;
end.
