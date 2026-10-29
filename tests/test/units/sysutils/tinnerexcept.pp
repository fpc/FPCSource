{ Exception.InnerException, RaiseOuterException and the stack information hooks, issue #36865 }
program tinnerexcept;

{$mode objfpc}{$h+}

uses
  SysUtils;

type
  ECounted = class(Exception)
  public
    destructor Destroy; override;
  end;

var
  gDestroyed, gStackInfoCalls, gCleanUps: LongInt;
  gStackInfoObject: TObject;
  gStackInfoAddr: CodePointer;


destructor ECounted.Destroy;

begin
  Inc(gDestroyed);
  inherited Destroy;
end;


// Halts with aCode when aCondition does not hold
procedure Check(aCode: Integer; aCondition: Boolean; const aMsg: string);

begin
  if not aCondition then
    begin
    writeln('Failed: ',aMsg);
    halt(aCode);
    end;
end;


// Returns a new string as stack information for the exception in aInfo
function TestStackInfo(aInfo: PExceptObject): Pointer;

var
  lInfo: PString;

begin
  Inc(gStackInfoCalls);
  gStackInfoObject:=aInfo^.FObject;
  gStackInfoAddr:=aInfo^.Addr;
  New(lInfo);
  lInfo^:='trace of '+aInfo^.FObject.ClassName;
  Result:=lInfo;
end;


// Returns the string stored by TestStackInfo
function TestStackInfoString(aStackInfo: Pointer): string;

begin
  if aStackInfo=nil then
    Result:=''
  else
    Result:=PString(aStackInfo)^;
end;


// Frees the string stored by TestStackInfo
procedure TestCleanUp(aStackInfo: Pointer);

begin
  Inc(gCleanUps);
  Dispose(PString(aStackInfo));
end;


// Raises ECounted('inner') and wraps it in ECounted('outer') with RaiseOuterException
procedure RaiseChained;

begin
  try
    raise ECounted.Create('inner');
  except
    Exception.RaiseOuterException(ECounted.Create('outer'));
  end;
end;


// Wraps RaiseChained in a third exception with ThrowOuterException
procedure RaiseChainedTwice;

begin
  try
    RaiseChained;
  except
    Exception.ThrowOuterException(ECounted.Create('top'));
  end;
end;


// Divides by a zero that the compiler cannot see
function Divide(aValue, aBy: Integer): Integer;

begin
  Result:=aValue div aBy;
end;


var
  lCaught: Boolean;
  lZero: Integer;

begin
  { inner exception set and owned }
  gDestroyed:=0;
  lCaught:=False;
  try
    RaiseChained;
  except
    on E: ECounted do
      begin
      lCaught:=True;
      Check(1,E.Message='outer','outer exception is caught');
      Check(2,Assigned(E.InnerException),'InnerException is set');
      Check(3,E.InnerException.Message='inner','InnerException is the handled exception');
      Check(4,E.BaseException=E.InnerException,'BaseException is the inner exception');
      Check(5,E.InnerException.InnerException=nil,'inner exception has no InnerException');
      Check(6,E.ToString='ECounted: outer'+sLineBreak+'ECounted: inner','ToString lists the chain');
      Check(7,gDestroyed=0,'inner exception is not freed while the outer one lives');
      end;
  end;
  Check(8,lCaught,'RaiseOuterException raises');
  Check(9,gDestroyed=2,'outer and inner exception are freed once');

  { chain of three }
  gDestroyed:=0;
  try
    RaiseChainedTwice;
  except
    on E: ECounted do
      begin
      Check(20,E.Message='top','top exception is caught');
      Check(21,E.InnerException.Message='outer','second level');
      Check(22,E.BaseException.Message='inner','BaseException walks the chain');
      end;
  end;
  Check(23,gDestroyed=3,'all three exceptions are freed once');

  { no exception being handled }
  gDestroyed:=0;
  try
    Exception.RaiseOuterException(ECounted.Create('alone'));
  except
    on E: ECounted do
      begin
      Check(30,E.InnerException=nil,'no InnerException outside an except block');
      Check(31,E.BaseException=E,'BaseException is the exception itself');
      Check(32,E.ToString='ECounted: alone','ToString without inner exception');
      end;
  end;
  Check(33,gDestroyed=1,'exception without inner is freed once');

  { a plain raise does not set an inner exception }
  try
    try
      raise ECounted.Create('first');
    except
      raise ECounted.Create('second');
    end;
  except
    on E: ECounted do
      Check(40,E.InnerException=nil,'plain raise in an except block sets no InnerException');
  end;

  { stack information hooks }
  Exception.GetExceptionStackInfoProc:=@TestStackInfo;
  Exception.GetStackInfoStringProc:=@TestStackInfoString;
  Exception.CleanUpStackInfoProc:=@TestCleanUp;
  gStackInfoCalls:=0;
  gCleanUps:=0;
  try
    raise ECounted.Create('hooked');
  except
    on E: ECounted do
      begin
      Check(50,gStackInfoCalls=1,'GetExceptionStackInfoProc called once on raise');
      Check(51,gStackInfoObject=E,'GetExceptionStackInfoProc receives the raised object');
      Check(52,gStackInfoAddr<>nil,'GetExceptionStackInfoProc receives the raise address');
      Check(53,E.StackInfo<>nil,'StackInfo is set');
      Check(54,E.StackTrace='trace of ECounted','StackTrace uses GetStackInfoStringProc');
      end;
  end;
  Check(55,gCleanUps=1,'CleanUpStackInfoProc called once on destruction');

  gStackInfoCalls:=0;
  gCleanUps:=0;
  try
    RaiseChained;
  except
    on E: ECounted do
      begin
      Check(60,gStackInfoCalls=2,'both exceptions of the chain get stack information');
      Check(61,E.InnerException.StackTrace='trace of ECounted','inner exception keeps its stack information');
      end;
  end;
  Check(62,gCleanUps=2,'stack information of the chain is cleaned up');

  gStackInfoCalls:=0;
  gCleanUps:=0;
  lZero:=0;
  try
    Divide(1,lZero);
  except
    on E: EDivByZero do
      begin
      Check(70,gStackInfoCalls=1,'runtime error exception gets stack information');
      Check(71,gStackInfoObject=E,'stack information is for the runtime error exception');
      end;
  end;
  Check(72,gCleanUps=1,'stack information of the runtime error exception is cleaned up');

  Exception.GetExceptionStackInfoProc:=nil;
  Exception.GetStackInfoStringProc:=nil;
  Exception.CleanUpStackInfoProc:=nil;
  try
    raise ECounted.Create('unhooked');
  except
    on E: ECounted do
      begin
      Check(80,E.StackInfo=nil,'no StackInfo without hook');
      Check(81,E.StackTrace='','empty StackTrace without hook');
      end;
  end;
  writeln('ok');
end.
