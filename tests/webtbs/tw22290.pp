uses
  SysUtils;

begin
  if formatfloat('0e',-1)<>'-1e' then
    halt(1);
end.
