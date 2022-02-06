{ %RECOMPILE }
{ %NORUN }

program tanonfunc55;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

uses
  uanonfunc55;

var
  f: specialize TFunc<LongInt>;
begin
  f := specialize Foo<LongInt>;
end.

