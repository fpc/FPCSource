{%NORUN}
{$mode objfpc}
{$modeswitch implicitfunctionspecialization}
{
  Test various overloads
}

program timpfuncspez34;

generic procedure DoThis<A>(param1: A);
begin
end;
 
generic procedure DoThis<A, B>(param1: A; param2: B);
begin
end;

generic procedure DoThis<A, B, C>(param1: A; param2: B; param3: C);
begin
end;

begin
  DoThis(1);
  DoThis(1,2);
  DoThis(1,2,3);

  DoThis(1);
  DoThis(1,'aaa');
  DoThis(1,'aaa',nil);
end.