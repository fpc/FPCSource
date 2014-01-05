
{mode delphiunicode}

const r: rawbytestring = 'abc';
begin
  if (stringcodepage(r) <> CP_ACP) and
     (stringcodepage(r) <> DefaultSystemCodePage) then
    halt(1);
end.
