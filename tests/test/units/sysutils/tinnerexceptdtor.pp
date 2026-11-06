{ Exception chains and destructors that raise while the exception being handled, or dispatched,
  must stay alive: (1) a chain destroyed at the end of an except block whose inner exception's
  destructor raises, (2) RaiseOuterException in the destructor of an exception whose unwind another
  abandoned, (3) the same for an exception whose except block another one leaves.
  Each case also runs alone: tinnerexceptdtor 1|2|3. }
program tinnerexceptdtor;

{$mode objfpc}{$h+}

uses
  SysUtils;

type
  // Logs its letter when created and when destroyed
  ELogged = class(Exception)
  public
    class function Letter: Char; virtual;
    procedure AfterConstruction; override;
    destructor Destroy; override;
  end;

  EOuter = class(ELogged)
    class function Letter: Char; override;
  end;

  EDispatched = class(ELogged)
    class function Letter: Char; override;
  end;

  EFromDestructor = class(ELogged)
    class function Letter: Char; override;
  end;

  EHead = class(ELogged)
    class function Letter: Char; override;
  end;

  EOther = class(ELogged)
    class function Letter: Char; override;
  end;

  // Raises EOther when destroyed
  EInnerRaising = class(ELogged)
    class function Letter: Char; override;
    destructor Destroy; override;
  end;

  // Raises EFromDestructor with RaiseOuterException when destroyed
  EOuterRaising = class(ELogged)
    class function Letter: Char; override;
    destructor Destroy; override;
  end;

var
  gCreated, gDestroyed: string;


class function ELogged.Letter: Char;

begin
  Result:='?';
end;


procedure ELogged.AfterConstruction;

begin
  inherited AfterConstruction;
  gCreated:=gCreated+Letter;
end;


destructor ELogged.Destroy;

begin
  gDestroyed:=gDestroyed+Letter;
  inherited Destroy;
end;


class function EOuter.Letter: Char;

begin
  Result:='O';
end;


class function EDispatched.Letter: Char;

begin
  Result:='D';
end;


class function EFromDestructor.Letter: Char;

begin
  Result:='F';
end;


class function EHead.Letter: Char;

begin
  Result:='H';
end;


class function EOther.Letter: Char;

begin
  Result:='X';
end;


class function EInnerRaising.Letter: Char;

begin
  Result:='I';
end;


destructor EInnerRaising.Destroy;

begin
  inherited Destroy;
  raise EOther.Create('raised by the destructor of the inner exception');
end;


class function EOuterRaising.Letter: Char;

begin
  Result:='R';
end;


destructor EOuterRaising.Destroy;

begin
  inherited Destroy;
  Exception.RaiseOuterException(EFromDestructor.Create('raised by a destructor'));
end;


// Halts with aCode when aCondition does not hold
procedure Check(aCode: Integer; aCondition: Boolean; const aMsg: string);

begin
  if not aCondition then
    begin
    writeln('Failed: ',aMsg);
    writeln('  created: ',gCreated,', destroyed: ',gDestroyed);
    halt(aCode);
    end;
end;


// Number of times aLetter occurs in aLog
function CountOf(aLetter: Char; const aLog: string): Integer;

var
  lChar: Char;

begin
  Result:=0;
  for lChar in aLog do
    if lChar=aLetter then
      Inc(Result);
end;


// Checks that every object created was destroyed exactly once
procedure CheckAllDestroyedOnce(aCode: Integer);

var
  lChar: Char;

begin
  for lChar in gCreated do
    Check(aCode,CountOf(lChar,gDestroyed)=CountOf(lChar,gCreated),
          'every '+lChar+' destroyed exactly once');
  Check(aCode,Length(gDestroyed)=Length(gCreated),'nothing destroyed that was not created');
end;


// Checks that the exception aHandled, received by a handler, is still alive
procedure CheckAlive(aCode: Integer; aHandled: ELogged; const aWhere: string);

begin
  Check(aCode,CountOf(aHandled.Letter,gDestroyed)<CountOf(aHandled.Letter,gCreated),
        aHandled.ClassName+' still alive in '+aWhere);
end;


// Runs aInner in the handler of an EOuter; checks EOuter stays the one handled and dies at the end
procedure InOuterHandler(aCode: Integer; aInner: TProcedure);

var
  lOuter: TObject;

begin
  try
    raise EOuter.Create('outer');
  except
    on O: EOuter do
      begin
      lOuter:=O;
      aInner();
      Check(aCode,ExceptObject=lOuter,'EOuter is still the exception handled');
      Check(aCode,CountOf('O',gDestroyed)=0,'EOuter alive until the end of its handler');
      end;
  end;
  Check(aCode,CountOf('O',gDestroyed)=1,'EOuter destroyed at the end of its handler');
end;


procedure ChainDestroyedAtEndOfExcept;

var
  lGot: string;

begin
  lGot:='';
  try
    try
      try
        raise EInnerRaising.Create('inner');
      except
        Exception.RaiseOuterException(EHead.Create('head'));
      end;
    except
      on H: EHead do
        Check(11,Assigned(H.InnerException) and (H.InnerException is EInnerRaising),'EHead has EInnerRaising as inner exception');
    end;
  except
    on E: ELogged do
      begin
      CheckAlive(12,E,'the handler around the chain');
      lGot:=E.ClassName;
      end;
  end;
  Check(13,lGot='EOther','the exception of the inner destructor reaches the handler around the chain, got "'+lGot+'"');
end;


procedure TestChain;

begin
  InOuterHandler(10,@ChainDestroyedAtEndOfExcept);
  CheckAllDestroyedOnce(14);
end;


procedure AbandonedUnwind;

var
  lGot: string;

begin
  lGot:='';
  try
    try
      raise EOuterRaising.Create('abandoned');
    finally
      raise EDispatched.Create('dispatched');
    end;
  except
    on E: ELogged do
      begin
      CheckAlive(21,E,'the handler of the abandoning exception');
      lGot:=E.ClassName;
      end;
  end;
  writeln('  abandoned unwind: handler got ',lGot);
end;


procedure TestAbandoned;

begin
  InOuterHandler(20,@AbandonedUnwind);
  CheckAllDestroyedOnce(22);
end;


procedure ExceptBlockLeft;

var
  lGot: string;

begin
  lGot:='';
  try
    try
      raise EOuterRaising.Create('left');
    except
      raise EDispatched.Create('dispatched');
    end;
  except
    on E: ELogged do
      begin
      CheckAlive(31,E,'the handler of the leaving exception');
      lGot:=E.ClassName;
      end;
  end;
  writeln('  except block left: handler got ',lGot);
end;


procedure TestLeft;

begin
  InOuterHandler(30,@ExceptBlockLeft);
  CheckAllDestroyedOnce(32);
end;


// Runs case aCase with fresh logs
procedure RunCase(aCase: Integer);

begin
  gCreated:='';
  gDestroyed:='';
  writeln('case ',aCase);
  case aCase of
    1: TestChain;
    2: TestAbandoned;
    3: TestLeft;
  end;
end;


var
  lCase: Integer;

begin
  if ParamCount>0 then
    RunCase(StrToInt(ParamStr(1)))
  else
    for lCase:=1 to 3 do
      RunCase(lCase);
  writeln('ok');
end.
