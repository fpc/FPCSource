unit uprocext1;
interface
var
  err : boolean;
procedure proc1;
implementation
uses uprocext2;
procedure proc1;external name 'ExternalProc3';
end.
