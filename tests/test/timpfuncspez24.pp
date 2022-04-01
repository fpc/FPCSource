{%NORUN}
{$mode objfpc}
{$modeswitch implicitfunctionspecialization}
{
  Test multi-dimensional array specializations
}

program timpfuncspez24;

type
  generic TArray<T> = array of T;

generic procedure DoThis<T>(param: specialize TArray<specialize TArray<T>>);
begin
end;

begin
  DoThis([[1,2,3],[2,3,4],[3,4,5]]);
end.