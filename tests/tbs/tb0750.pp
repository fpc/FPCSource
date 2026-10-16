{ %target=win32,win64 }

{ an exception whose destructor raises is destroyed inside the handler of an
  outer exception: by the end of an except block (1), by leaving an except
  block through another exception (2), or as an exception whose unwind another
  one abandoned (3). The outer exception must stay the one its handler
  handles, and be destroyed once, at the end of that handler; the exception
  expected for the case must reach the handler around the inner code, and
  every object must be destroyed once. Where the run time library destroys
  the object while an exception is dispatched -- 3, and 2 on win64 -- a
  destructor raising an exception whose destructor raises in turn is
  contained, and the dispatched exception goes on; at the end of an except
  block, and on win32 where another exception leaves it, the destructor's
  exception goes on instead }
program tb0750;

{$mode objfpc}

type
  TCounted = class
    destructor Destroy; override;
  end;
  EOuter = class(TCounted)
  end;
  EOther = class(TCounted)
  end;
  EThird = class(TCounted)
  end;
  ELast = class(TCounted)
  end;
  { raises EOther when destroyed }
  EInner = class(TCounted)
    destructor Destroy; override;
  end;
  { raises ELast when destroyed }
  EMiddle = class(TCounted)
    destructor Destroy; override;
  end;
  { raises EMiddle when destroyed }
  EChain = class(TCounted)
    destructor Destroy; override;
  end;

var
  { the objects destroyed, in order: a letter for each class }
  Destroyed: shortstring;

function Letter(o: TObject): char;
begin
  if o is EOuter then
    Letter:='O'
  else if o is EOther then
    Letter:='o'
  else if o is EThird then
    Letter:='t'
  else if o is ELast then
    Letter:='l'
  else
    Letter:='?';
end;

destructor TCounted.Destroy;
begin
  Destroyed:=Destroyed+Letter(Self);
  inherited Destroy;
end;

destructor EInner.Destroy;
begin
  Destroyed:=Destroyed+'i';
  raise EOther.Create;
end;

destructor EMiddle.Destroy;
begin
  Destroyed:=Destroyed+'m';
  raise ELast.Create;
end;

destructor EChain.Destroy;
begin
  Destroyed:=Destroyed+'c';
  raise EMiddle.Create;
end;

procedure AtEnd;
begin
  try
    raise EInner.Create;
  except
  end;
end;

procedure LeftByAnother;
begin
  try
{$ifdef WIN64}
    raise EChain.Create;
{$else}
    raise EInner.Create;
{$endif}
  except
    raise EThird.Create;
  end;
end;

procedure Abandoned;
begin
  try
    raise EChain.Create;
  finally
    raise EThird.Create;
  end;
end;

type
  TInner = procedure;

{ runs inner inside the handler of an outer exception; returns a bit per failed
  check: 1 not the expected exception arrived, 2 the outer exception was
  destroyed inside its handler, 4 it was no longer the one handled, 8 the
  objects destroyed were not those expected, the outer one last }
function Check(inner: TInner; expected: TClass; const destructions: shortstring): longint;
var
  caught: TClass;
  afterinner: shortstring;
  stillthere: boolean;
begin
  Check:=0;
  Destroyed:='';
  caught:=nil;
  afterinner:='?';
  stillthere:=false;
  try
    raise EOuter.Create;
  except
    on E: EOuter do
      begin
        try
          inner();
        except
          on X: TObject do
            caught:=X.ClassType;
        end;
        afterinner:=Destroyed;
        stillthere:=assigned(RaiseList) and (RaiseList^.FObject=E);
      end;
  end;
  if caught<>expected then
    Check:=Check or 1;
  if pos('O',afterinner)<>0 then
    Check:=Check or 2;
  if not stillthere then
    Check:=Check or 4;
  if Destroyed<>destructions+'O' then
    Check:=Check or 8;
  if Check<>0 then
    begin
      if assigned(caught) then
        write('caught ',caught.ClassName)
      else
        write('caught nothing');
      writeln(', destroyed ',Destroyed);
    end;
end;

var
  code: longint;
begin
  { a parameter 1, 2 or 3 runs only that case }
  code:=0;
  if (paramstr(1)='') or (paramstr(1)='1') then
    code:=code or Check(@AtEnd,EOther,'io');
  if (paramstr(1)='') or (paramstr(1)='2') then
{$ifdef WIN64}
    code:=code or (Check(@LeftByAnother,EThird,'cmlt') shl 4);
{$else}
    code:=code or (Check(@LeftByAnother,EOther,'ito') shl 4);
{$endif}
  if (paramstr(1)='') or (paramstr(1)='3') then
    code:=code or (Check(@Abandoned,EThird,'cmlt') shl 8);
  if code<>0 then
    begin
      writeln('failed: ',hexstr(code,3));
      halt(1);
    end;
  writeln('ok');
end.
