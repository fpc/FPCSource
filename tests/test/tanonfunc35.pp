program tanonfunc35;

{$mode objfpc}
{$modeswitch anonymousfunctions}
{$modeswitch functionreferences}

{ test anonymous methods with calling conventions }

type
  TRec = record
    l: longint;
    c: ansichar;
    b: boolean;
    p: ^TRec;
  end;

  TCdeclFunc = reference to function(l: longint; r: TRec): longint cdecl;
  TRegisterFunc = reference to function(l: longint; r: TRec): longint register;

procedure Foo(aRec: TRec);
var
  cdeclFunc: TCdeclFunc;
  registerFunc: TRegisterFunc;
begin
  cdeclFunc := function(l: longint; r: TRec): longint cdecl
  begin
    Result := l + r.l;
    if r.c <> 'a' then
      halt(1);
  end;
  if cdeclFunc(123, aRec) <> 246 then
    halt(2);

  registerFunc := function(l: longint; r: TRec): longint register
  begin
    Result := l + r.l;
    if r.c <> 'a' then
      halt(3);
  end;
  if registerFunc(321, aRec) <> 444 then
    halt(4);
end;

var
  r: TRec;
begin
  r.l := 123;
  r.c := 'a';
  r.b := False;
  r.p := @r;
  Foo(r);
end.


