uses sysutils;

var
  s: ansistring;
begin
  DecimalSeparator:='.';
  s := format('%e',[1.72]);
{$ifdef FPC_HAS_TYPE_EXTENDED}
  if s <> '1.7200000000000000E+000' then
{$else}
  if s <> '1.72000000000000E+000' then
{$endif}
    halt(1);
end.
