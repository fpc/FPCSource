{ %OPT = -gt }

program tw39857;

{$mode objfpc}{$H+}
{$ModeSwitch anonymousfunctions}
{$ModeSwitch functionreferences}

type
  TProc = reference to procedure;

procedure problem(aParam1: integer; aParam2: integer; aParam3: TProc);
begin
  Writeln(aParam1, aParam2);
end;

procedure noproblem(aParam1: integer; aParam2: integer; aParam3: IUnknown);
begin
  Writeln(aParam1, aParam2);
end;

procedure test;
begin
  noproblem(1, 2, TInterfacedObject.Create);            // ok
  problem(3,4, nil);                                    // ok
  problem(5,6, procedure begin Writeln('x'); end);      // aParam3 is trashed
end;

begin
  test;
end.
