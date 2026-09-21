{ unit of tb0771.pp: its finalization raises }
unit ub0771;

{$mode objfpc}

interface

implementation

initialization
finalization
  raise TObject.Create;
end.
