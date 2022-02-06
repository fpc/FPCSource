{ %NORUN }

{ normal procedure variable directives can be used on function references }
program tfuncref5;

{$mode objfpc}
{$modeswitch functionreferences}

type
  TProc1 = reference to procedure cdecl;
  TProc2 = reference to procedure; cdecl;
  TProc3 = reference to procedure; [cdecl];

  TFunc1 = reference to function: LongInt cdecl;
  TFunc2 = reference to function: LongInt; cdecl;
  TFunc3 = reference to function: LongInt; [cdecl];

var
  Proc1: reference to procedure cdecl;
  Proc2: reference to procedure; cdecl;
  //Proc3: reference to procedure; [cdecl];

  Func1: reference to function: LongInt cdecl;
  Func2: reference to function: LongInt; cdecl;
  //Func3: reference to function: LongInt; [cdecl];

const
  CProc1: reference to procedure cdecl = Nil;
  CProc2: reference to procedure; cdecl = Nil;
  //CProc3: reference to procedure; [cdecl] = Nil;

  CFunc1: reference to function: LongInt cdecl = Nil;
  CFunc2: reference to function: LongInt; cdecl = Nil;
  //CFunc3: reference to function: LongInt; [cdecl] = Nil;

begin

end.
