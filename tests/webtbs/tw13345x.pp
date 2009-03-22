{ %opt=-gh }
{ %recompile }

{$mode delphi}

uses
  uw13345y;

type
  tc = class(tinterfacedobject,ta)
  end;

begin
  HaltOnNotReleased:=true;
  { should be automatically freed by the finalization code of tw13345y }
  c:=tc.create;
end.
