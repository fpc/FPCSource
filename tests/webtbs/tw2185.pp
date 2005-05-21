{ Source provided for Free Pascal Bug Report 2185 }
{ Submitted by "Andrew Johnson" on  2002-10-16 }
{ e-mail: AJ_Genius@Hotmail.com }
Program BugTest;

{$Mode ObjFPC}{$H+}
{off $Define InvertOrder}

Uses Classes;

Type
  TMyClass = Class(TPersistent)
  public
    dummy : Longint;
    {$IfNDef InvertOrder}
      procedure Assign(Source : TPersistent); override;
      procedure Assign(const value: integer);
    {$Else InvertOrder}
      procedure Assign(const value: integer);
      procedure Assign(Source : TPersistent); override;
    {$EndIf InvertOrder}

    Constructor Create;
  end;

procedure TMyClass.Assign(Source : TPersistent);
begin
  If Source is TMyClass then
    Dummy := TMyClass(Source).dummy;
end;

procedure TMyClass.Assign(const value: integer);
begin
  Dummy := value;
end;

Constructor TMyClass.Create;
begin
  Inherited Create;
end;

Const
  ConstDummy : Integer = 3;

var
  Test1, Test2 : TMyClass;
begin
  Test1 := TMyClass.Create;
  Test2 := TMyClass.Create;
  Test1.Dummy := 2;
  Test2.Dummy := 1;
  Writeln(Test2.Dummy);
  Test2.Assign(Test1);
  Writeln(Test2.Dummy);
  Test2.Assign(ConstDummy);
  Writeln(Test2.Dummy);
  Test1.Destroy;
  Test2.Destroy;
end.
