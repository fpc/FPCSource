{%NORUN}
{$mode objfpc}
{$modeswitch implicitfunctionspecialization}
{
  Test class constraints
}

program timpfuncspez21;

type
  TMyClass = class
  end;
  TMyChild = class(TMyClass)
  end;

generic procedure DoThis<T: TMyClass>(obj: T);
begin
end;

begin
  DoThis(TMyClass.Create);
  DoThis(TMyChild.Create);
end.