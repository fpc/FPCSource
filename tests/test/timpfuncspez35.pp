{%NORUN}
{$mode delphi}
{$modeswitch implicitfunctionspecialization}
{
  Grab bag to test Delphi mode
}

program timpfuncspez35;

{ Classes }

type
  TMyClass = class
    class procedure Call<T>(msg: T);
    procedure DoThis<T>(msg: T);
  end;

class procedure TMyClass.Call<T>(msg:T);
begin
end;

procedure TMyClass.DoThis<T>(msg:T);
begin
end;

{ Methods }

procedure DoThis<A>(param1: A);
begin
end;
 
procedure DoThis<A, B>(param1: A; param2: B);
begin
end;

procedure DoThis<A, B, C>(param1: A; param2: B; param3: C);
begin
end;

var
  obj: TMyClass;
begin
  TMyClass.Call('Hello World');
  obj := TMyClass.create;
  obj.DoThis(1);

  DoThis(1);
  DoThis(1,2);
  DoThis(1,2,3);

  DoThis(1);
  DoThis(1,'aaa');
  DoThis(1,'aaa',nil);
end.