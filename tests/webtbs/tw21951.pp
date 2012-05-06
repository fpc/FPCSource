{ %opt=-Mobjfpc -Sh }

{$modeswitch exceptions}

var
  S: string;
begin
  SetLength(S, 1000);
  if (length(s)<>1000) then
    halt(1);
end.
