
{$mode delphiunicode}

const r: rawbytestring = 'abc';
begin
  if stringcodepage(r) = CP_NONE then
    halt(1);
end.
