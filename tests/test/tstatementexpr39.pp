{ anonymous functions in case-, try- and nested statement expressions }
{$Mode Delphi}

type
  TProcRef = reference to procedure;
  TIntFuncRef = reference to function(a: Integer): Integer;

var
  Called: Integer;

procedure Run(const p: TProcRef);
begin
  p();
end;

function Raiser(doRaise: Boolean): Integer;
begin
  Result := 1;
  if doRaise then
    raise TObject.Create;
end;

procedure Test(i: Integer; Expected: Integer; Code: Integer);
var
  SomeProc: TProcRef;
  f: TIntFuncRef;
  Local: Integer;
begin
  Local:=100;
  Called:=0;
  { case }
  SomeProc:=case i of
      1: procedure begin Called:=Local+1; end;
      2: procedure begin Called:=Local+2; end;
      else procedure begin Called:=Local+3; end
    end;
  SomeProc();
  WriteLn(Called);
  if Called<>Local+Expected then
    Halt(Code);

  { try, note: anonymous functions are not allowed in except blocks }
  Called:=0;
  SomeProc:=try
      if Raiser(i=2)=1 then procedure begin Called:=Local+1; end else procedure begin Called:=Local+4; end
    except
      on o: TObject do nil;
      else nil
    end;
  if i=2 then
    begin
      if Assigned(SomeProc) then
        Halt(Code+10);
    end
  else
    begin
      SomeProc();
      WriteLn(Called);
      if Called<>Local+1 then
        Halt(Code+20);
    end;

  { nested if-expression }
  Called:=0;
  SomeProc:=if i=1 then
      (if Local=100 then procedure begin Called:=Local+1; end else procedure begin Called:=0; end)
    else
      procedure begin Called:=Local+Expected; end;
  SomeProc();
  WriteLn(Called);
  if Called<>Local+Expected then
    Halt(Code+30);

  { constant condition }
  f:=if true then function(a: Integer): Integer begin Result:=a+Local; end
    else function(a: Integer): Integer begin Result:=a-Local; end;
  if f(1)<>101 then
    Halt(Code+40);

  { as argument }
  Called:=0;
  Run(if i=1 then procedure begin Called:=7; end else procedure begin Called:=8; end);
  WriteLn(Called);
  if (i=1) and (Called<>7) then
    Halt(Code+50);
  if (i<>1) and (Called<>8) then
    Halt(Code+60);
end;

begin
  Test(1,1,1);
  Test(2,2,2);
  Test(3,3,3);
end.
