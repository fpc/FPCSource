{ Source provided for Free Pascal Bug Report 3681 }
{ Submitted by "Marco" on  2005-02-22 }
{ e-mail:  }
{$Mode Delphi}
Uses Variants;

var cvariant : variant;
begin
  try
    cvariant := VarArrayCreate( [0, 5], varVariant );
    cvariant[ 0 ] := 'TEST1';
    cvariant[ 1 ] := 'TEST2';
    cvariant[ 2 ] := 'TEST3';
    cvariant[ 3 ] := 'TEST4';
    cvariant[ 4 ] := 'TEST5';
  finally
    VarArrayRedim( cvariant, 0 );
    if (not varIsEmpty( cvariant )) then
      VarClear( cvariant );
  end;
end.
