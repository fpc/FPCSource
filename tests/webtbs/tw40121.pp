uses
  dateutils;
var
  dt : TDateTime;
begin
  if not(TryISOStrToDateTime('2015-06-30T23:59:60Z',dt)) then
    halt(1);
  if not(TryISOStrToDateTime('2016-12-31T23:59:60Z',dt)) then
    halt(1);
end.
