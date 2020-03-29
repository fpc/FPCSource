{ %RECOMPILE }
{ %NORUN }

{ ensure that nested routines inside generics are handled correctly }

program tgeneric104;

uses
  ugeneric104;

type
  TTest = specialize TGeneric<LongInt>;

begin
  specialize TestProc<LongInt>;
end.
