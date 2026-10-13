{ %opt=-Seh -vh }

{ The address of a member named through a type -- @TClass.Method and
  @TObject.Method -- uses the unit that declares the member, also when the type
  that names it is declared in another unit: ub0748a, which declares both
  members, must not be reported as not used }
program tb0748;

{$mode objfpc}

uses
  ub0748a, ub0748b;

var
  p, q: pointer;
begin
  p:=@TDerived0748.M;
  q:=@TDerivedObj0748.P;
  if (p=nil) or (q=nil) then
    halt(1);
end.
