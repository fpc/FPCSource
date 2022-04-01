{%FAIL}
{$mode objfpc}
{$modeswitch implicitfunctionspecialization}
{
  Test class constraints
}

program timpfuncspez22;

type
  TMyClass = class
  end;
  TOtherClass = class
  end;

generic procedure DoThis<T: TMyClass>(obj: T);
begin
end;

begin
  DoThis(TOtherClass.Create);
end.