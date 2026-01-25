{$mode delphi}
type
  TSqlDestroyPtr = procedure(p: pointer); cdecl;

const
  SQLITE_TRANSIENT = pointer(1);
  SQLITE_STATIC = pointer(0);
var
  TRANSIENT_STATIC: array[boolean] of TSqlDestroyPtr = (
    SQLITE_TRANSIENT,
    SQLITE_STATIC);

begin
end.
