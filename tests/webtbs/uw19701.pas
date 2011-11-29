unit uw19701;
{$ifdef FPC}{$mode objfpc}{$h+}{$endif}
interface
var
 testvar: array of integer;
implementation

// an empty finalization section should not prevent
// generating the implicit finalization code
initialization
 setlength(testvar,100);
finalization
end.
