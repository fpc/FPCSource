uses
{$ifdef unix}
  cthreads,
{$endif}
  sysutils;

var
  F: TMultiReadExclusiveWriteSynchronizer;
begin
  F:=TMultiReadExclusiveWriteSynchronizer.Create;
  F.Beginwrite; 
  IsMultiThread:=true; //Culprit.
  F.Endwrite;
  F.Beginwrite;
  F.Endwrite;
  F.Free;
end.
