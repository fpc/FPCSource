unit system;
interface

{$Y-}

type
  integer=-32768..32767;
  byte=0..255;
  word=0..65535;
  longint=$80000000..$7fffffff;
  pchar=^char;

implementation

procedure do_exit;[public,alias:'FPC_DO_EXIT'];
begin
end;

begin
end.
