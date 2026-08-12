unit tests.rtti.intercept;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC}
  fpcunit, testregistry,
{$ELSE FPC}
  TestFramework,
{$ENDIF FPC}
  SysUtils, Classes, typinfo, Rtti;

type
  EInterceptTest = class(Exception);

{$RTTI EXPLICIT METHODS[vcPrivate,vcProtected,vcPublic,vcPublished]}

  { TInterceptSample }

  TInterceptSample = class
  public
    function Add(aA, aB: Integer): Integer; virtual;
    procedure Accumulate(var aValue: Integer; aDelta: Integer); virtual;
    function Describe: String; virtual;
    procedure Boom; virtual;
    function NonVirtualAdd(aA, aB: Integer): Integer;
  end;

  { TInterceptChild }

  TInterceptChild = class(TInterceptSample)
  public
    function Add(aA, aB: Integer): Integer; override;
  end;

  {$RTTI EXPLICIT METHODS[]}
  { TNoRttiSample: 
    its virtual methods have no RTTI and sit beyond the standard TObject slots. 
    Reproduces issue #41799, where the proxy VMT was sized from RTTI-visible 
    methods only and was therefore too small. 
    }
  TNoRttiSample = class
  public
    function Value: Integer; virtual;
  end;
  {$RTTI EXPLICIT METHODS[vcPrivate,vcProtected,vcPublic,vcPublished]}

  { TTestVirtualMethodInterceptor }

  TTestVirtualMethodInterceptor = class(TTestCase)
  private
    FBeforeCount: Integer;
    FAfterCount: Integer;
    FExceptCount: Integer;
    FLastMethodName: String;
    FLastInstance: TObject;
    FLastArgCount: SizeInt;
    FArg0, FArg1: Integer;
    FLastExceptionMsg: String;
    { configuration read by the handlers }
    FSuppress: Boolean;
    FSuppressResult: Integer;
    FResultFactor: Integer;
    FReraise: Boolean;
    FSwallowResult: Integer;
    procedure DoBefore(aInstance: TObject; aMethod: TRttiMethod; const aArgs: TValueArray; out aDoInvoke: Boolean; out aResult: TValue);
    procedure DoAfter(aInstance: TObject; aMethod: TRttiMethod; const aArgs: TValueArray; var aResult: TValue);
    procedure DoException(aInstance: TObject; aMethod: TRttiMethod; const aArgs: TValueArray; out aRaiseException: Boolean; aTheException: Exception; out aResult: TValue);
    function CreateInterceptor(aClass: TClass): TVirtualMethodInterceptor;
  protected
    procedure SetUp; override;
  published
    procedure TestBeforeAfterFire;
    procedure TestUnproxifyRestores;
    procedure TestSuppressInvoke;
    procedure TestModifyResult;
    procedure TestExceptionReraise;
    procedure TestExceptionSwallow;
    procedure TestNonVirtualNotIntercepted;
    procedure TestIsOperatorAndParent;
    procedure TestVarParameter;
    procedure TestChildOverride;
    procedure TestNoRttiVirtualMethodNotCrash;
  end;

implementation

{ TInterceptSample }

function TInterceptSample.Add(aA, aB: Integer): Integer;

begin
  Result:=aA + aB;
end;


procedure TInterceptSample.Accumulate(var aValue: Integer; aDelta: Integer);

begin
  aValue:=aValue + aDelta;
end;


function TInterceptSample.Describe: String;

begin
  Result:='original';
end;


procedure TInterceptSample.Boom;

begin
  raise EInterceptTest.Create('boom');
end;


function TInterceptSample.NonVirtualAdd(aA, aB: Integer): Integer;

begin
  Result:=aA + aB;
end;


{ TInterceptChild }

function TInterceptChild.Add(aA, aB: Integer): Integer;

begin
  Result:=inherited Add(aA, aB) + 1000;
end;


{ TNoRttiSample }

function TNoRttiSample.Value: Integer;

begin
  Result:=4711;
end;


{ TTestVirtualMethodInterceptor }

procedure TTestVirtualMethodInterceptor.SetUp;

begin
  inherited SetUp;
  FBeforeCount:=0;
  FAfterCount:=0;
  FExceptCount:=0;
  FLastMethodName:='';
  FLastInstance:=nil;
  FLastArgCount:=-1;
  FArg0:=0;
  FArg1:=0;
  FLastExceptionMsg:='';
  FSuppress:=False;
  FSuppressResult:=0;
  FResultFactor:=0;
  FReraise:=True;
  FSwallowResult:=0;
end;


procedure TTestVirtualMethodInterceptor.DoBefore(aInstance: TObject; aMethod: TRttiMethod; const aArgs: TValueArray; out aDoInvoke: Boolean; out aResult: TValue);

begin
  Inc(FBeforeCount);
  FLastMethodName:=aMethod.Name;
  FLastInstance:=aInstance;
  FLastArgCount:=Length(aArgs);
  if Length(aArgs) >= 1 then
    FArg0:=aArgs[0].AsInteger;
  if Length(aArgs) >= 2 then
    FArg1:=aArgs[1].AsInteger;
  aDoInvoke:=not FSuppress;
  if FSuppress then
    aResult:=FSuppressResult;
end;


procedure TTestVirtualMethodInterceptor.DoAfter(aInstance: TObject; aMethod: TRttiMethod; const aArgs: TValueArray; var aResult: TValue);

begin
  Inc(FAfterCount);
  if FResultFactor <> 0 then
    aResult:=aResult.AsInteger * FResultFactor;
end;


procedure TTestVirtualMethodInterceptor.DoException(aInstance: TObject; aMethod: TRttiMethod; const aArgs: TValueArray; out aRaiseException: Boolean; aTheException: Exception; out aResult: TValue);

begin
  Inc(FExceptCount);
  FLastExceptionMsg:=aTheException.Message;
  aRaiseException:=FReraise;
  if not FReraise then
    aResult:=FSwallowResult;
end;


function TTestVirtualMethodInterceptor.CreateInterceptor(aClass: TClass): TVirtualMethodInterceptor;

begin
  { degrade gracefully on platforms without a function call manager }
  try
    Result:=TVirtualMethodInterceptor.Create(aClass);
  except
    on e: ENotImplemented do
      begin
      Ignore('Method implementations not supported on this platform: ' + e.Message);
      Result:=nil;
      end;
    on e: EInsufficientRtti do
      begin
      Ignore('Insufficient RTTI: ' + e.Message);
      Result:=nil;
      end;
  end;
end;


procedure TTestVirtualMethodInterceptor.TestBeforeAfterFire;

var
  itc: TVirtualMethodInterceptor;
  obj: TInterceptSample;
  res: Integer;

begin
  itc:=CreateInterceptor(TInterceptSample);
  obj:=TInterceptSample.Create;
  try
    itc.OnBefore:=@DoBefore;
    itc.OnAfter:=@DoAfter;
    itc.Proxify(obj);
    res:=obj.Add(2, 3);
    CheckEquals(5, res, 'Original method result');
    CheckEquals(1, FBeforeCount, 'OnBefore fired once');
    CheckEquals(1, FAfterCount, 'OnAfter fired once');
    CheckEquals('Add', FLastMethodName, 'Intercepted method name');
    CheckSame(obj, FLastInstance, 'Instance passed to handler');
    CheckEquals(2, FLastArgCount, 'Argument count (Self excluded)');
    CheckEquals(2, FArg0, 'First argument');
    CheckEquals(3, FArg1, 'Second argument');
    itc.Unproxify(obj);
  finally
    obj.Free;
    itc.Free;
  end;
end;


procedure TTestVirtualMethodInterceptor.TestUnproxifyRestores;

var
  itc: TVirtualMethodInterceptor;
  obj: TInterceptSample;

begin
  itc:=CreateInterceptor(TInterceptSample);
  obj:=TInterceptSample.Create;
  try
    itc.OnBefore:=@DoBefore;
    itc.Proxify(obj);
    obj.Add(1, 1);
    CheckEquals(1, FBeforeCount, 'Fired while proxified');
    itc.Unproxify(obj);
    CheckTrue(obj.ClassType = TInterceptSample, 'Class restored after Unproxify');
    obj.Add(1, 1);
    CheckEquals(1, FBeforeCount, 'Not fired after Unproxify');
  finally
    obj.Free;
    itc.Free;
  end;
end;


procedure TTestVirtualMethodInterceptor.TestSuppressInvoke;

var
  itc: TVirtualMethodInterceptor;
  obj: TInterceptSample;
  res: Integer;

begin
  itc:=CreateInterceptor(TInterceptSample);
  obj:=TInterceptSample.Create;
  try
    FSuppress:=True;
    FSuppressResult:=42;
    itc.OnBefore:=@DoBefore;
    itc.Proxify(obj);
    res:=obj.Add(2, 3);
    CheckEquals(42, res, 'Suppressed call returns the supplied result');
  finally
    obj.Free;
    itc.Free;
  end;
end;


procedure TTestVirtualMethodInterceptor.TestModifyResult;

var
  itc: TVirtualMethodInterceptor;
  obj: TInterceptSample;
  res: Integer;

begin
  itc:=CreateInterceptor(TInterceptSample);
  obj:=TInterceptSample.Create;
  try
    FResultFactor:=2;
    itc.OnAfter:=@DoAfter;
    itc.Proxify(obj);
    res:=obj.Add(2, 3);
    CheckEquals(10, res, 'OnAfter doubled the result');
  finally
    obj.Free;
    itc.Free;
  end;
end;


procedure TTestVirtualMethodInterceptor.TestExceptionReraise;

var
  itc: TVirtualMethodInterceptor;
  obj: TInterceptSample;
  raised: Boolean;

begin
  itc:=CreateInterceptor(TInterceptSample);
  obj:=TInterceptSample.Create;
  raised:=False;
  try
    FReraise:=True;
    itc.OnException:=@DoException;
    itc.Proxify(obj);
    try
      obj.Boom;
    except
      on e: EInterceptTest do
        raised:=True;
    end;
    CheckEquals(1, FExceptCount, 'OnException fired');
    CheckEquals('boom', FLastExceptionMsg, 'Exception message');
    CheckTrue(raised, 'Exception was re-raised');
  finally
    obj.Free;
    itc.Free;
  end;
end;


procedure TTestVirtualMethodInterceptor.TestExceptionSwallow;

var
  itc: TVirtualMethodInterceptor;
  obj: TInterceptSample;

begin
  itc:=CreateInterceptor(TInterceptSample);
  obj:=TInterceptSample.Create;
  try
    FReraise:=False;
    itc.OnException:=@DoException;
    itc.Proxify(obj);
    try
      obj.Boom;
    except
      Fail('Exception should have been swallowed');
    end;
    CheckEquals(1, FExceptCount, 'OnException fired');
  finally
    obj.Free;
    itc.Free;
  end;
end;


procedure TTestVirtualMethodInterceptor.TestNonVirtualNotIntercepted;

var
  itc: TVirtualMethodInterceptor;
  obj: TInterceptSample;
  res: Integer;

begin
  itc:=CreateInterceptor(TInterceptSample);
  obj:=TInterceptSample.Create;
  try
    itc.OnBefore:=@DoBefore;
    itc.Proxify(obj);
    res:=obj.NonVirtualAdd(2, 3);
    CheckEquals(5, res, 'Non-virtual method result');
    CheckEquals(0, FBeforeCount, 'Non-virtual methods are not intercepted');
  finally
    obj.Free;
    itc.Free;
  end;
end;


procedure TTestVirtualMethodInterceptor.TestIsOperatorAndParent;

var
  itc: TVirtualMethodInterceptor;
  obj: TInterceptSample;

begin
  itc:=CreateInterceptor(TInterceptSample);
  obj:=TInterceptSample.Create;
  try
    itc.Proxify(obj);
    CheckTrue(obj is TInterceptSample, '"is" still holds after Proxify');
    CheckTrue(obj.InheritsFrom(TInterceptSample), 'InheritsFrom still holds');
    CheckTrue(itc.ProxyClass.ClassParent = TInterceptSample, 'Proxy descends from original');
    itc.Unproxify(obj);
  finally
    obj.Free;
    itc.Free;
  end;
end;


procedure TTestVirtualMethodInterceptor.TestVarParameter;

var
  itc: TVirtualMethodInterceptor;
  obj: TInterceptSample;
  value: Integer;

begin
  itc:=CreateInterceptor(TInterceptSample);
  obj:=TInterceptSample.Create;
  try
    itc.OnBefore:=@DoBefore;
    itc.OnAfter:=@DoAfter;
    itc.Proxify(obj);
    value:=10;
    obj.Accumulate(value, 5);
    CheckEquals(15, value, 'var parameter written back through interception');
    CheckEquals(1, FBeforeCount, 'OnBefore fired');
    CheckEquals(1, FAfterCount, 'OnAfter fired');
  finally
    obj.Free;
    itc.Free;
  end;
end;


procedure TTestVirtualMethodInterceptor.TestChildOverride;

var
  itc: TVirtualMethodInterceptor;
  obj: TInterceptChild;
  res: Integer;

begin
  itc:=CreateInterceptor(TInterceptChild);
  obj:=TInterceptChild.Create;
  try
    itc.OnBefore:=@DoBefore;
    itc.Proxify(obj);
    res:=obj.Add(2, 3);
    CheckEquals(1005, res, 'Childmost override is invoked');
    CheckEquals(1, FBeforeCount, 'OnBefore fired once for the override');
  finally
    obj.Free;
    itc.Free;
  end;
end;


procedure TTestVirtualMethodInterceptor.TestNoRttiVirtualMethodNotCrash;

var
  itc: TVirtualMethodInterceptor;
  obj: TNoRttiSample;

begin
  itc:=CreateInterceptor(TNoRttiSample);
  obj:=TNoRttiSample.Create;
  try
    itc.Proxify(obj);
    { Issue #41799: 
      a proxified virtual method without RTTI must still be callable 
      (falling through to the original implementation) and must not
      cause an access violation because of a too-small proxy VMT. }
    CheckEquals(4711, obj.Value, 'Non-RTTI virtual method callable when proxified');
    itc.Unproxify(obj);
  finally
    obj.Free;
    itc.Free;
  end;
end;


initialization
  RegisterTest(TTestVirtualMethodInterceptor);
end.
