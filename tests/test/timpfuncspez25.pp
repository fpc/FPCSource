{%NORUN}
{$mode objfpc}
{$modeswitch implicitfunctionspecialization}
{
  Test generic methods within generic method bodies
}

program timpfuncspez25;
  
type
  TMyClass = class
    generic procedure DoThis<T>(msg: T);
    generic procedure DoThat<T>(msg: T);
  end;
  TMyChild = class(TMyClass)
    generic procedure DoSomething<T>(msg: T);
  end;

generic procedure TMyClass.DoThis<T>(msg:T);
begin
  DoThat(msg);
end;

generic procedure TMyClass.DoThat<T>(msg: T);
begin
end;

generic procedure TMyChild.DoSomething<T>(msg: T);
begin
  DoThis(msg);
end;

var
  a: TMyClass;
  b: TMyChild;
begin
  a := TMyClass.Create;
  a.DoThis(1);
  b := TMyChild.create;
  b.DoSomething(1);
end.