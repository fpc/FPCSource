program project1;
const
  S: string = '123';
var
  I: Integer;
  P: PChar;
begin
  {$RANGECHECKS ON}
  P := PChar(@S[2]);
  I := -1;
  if (P[-1]<>'1') or
     (P[I]<>'1') then
   halt(1);
end.
