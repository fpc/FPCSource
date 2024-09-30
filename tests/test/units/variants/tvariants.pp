program tvariants;

{$mode objfpc}

uses
  Variants, types;

var
  v: Variant;
begin
  v:=nullptr;
  if not VarIsNull(v) then
    ExitCode:=1;
end.

