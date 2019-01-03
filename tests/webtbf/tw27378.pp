{ %FAIL }
{ %OPT=-B -Sen }

{ we want the "Local variable "Var2" not used" hint as an error; if we don't
  get the error then resetting the verbosity when switching the unit failed }

program tw27378;

uses
  uw27378a, uw27378b;

begin

end.
