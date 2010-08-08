{$ifdef FPC}
{$mode macpas}
{$endif}
{$align power}
program StrSizeTest;
type
    StrRec =
      record
        s1: string[ 10];
        s2: string[ 10];
      end;
begin
  writeln( 'SizeOf( StrRec) = ', SizeOf( StrRec));
  if (sizeof(strrec)<>24) then
    halt(1);
end.
