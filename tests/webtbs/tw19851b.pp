uses
  sysutils;

begin
{$ifdef WINDOWS}
  executeprocess(GetEnvironmentVariable('COMSPEC'),'/c echo abcdefghijklmnopqrstuvwxyz1 | tw19851a');
{$endif WINDOWS}
end.
