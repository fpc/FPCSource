{ %RECOMPILE }
{ %NORUN }

program tanonfunc68;

{$mode delphi}{$H+}
{$modeswitch advancedrecords}

{same as tanonfunc55 but mode delphi}

uses
  uanonfunc55;

var
  f: TFunc<LongInt>;
begin
  f := Foo<LongInt>;
end.

