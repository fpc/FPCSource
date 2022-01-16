{ %opt=-Fccp1258 }

{ should not have any effect, since the setting was not enabled }

{$modeswitch systemcodepage-}

program tw28718c;

var
  a: ansistring;
begin
  a:='abc';
  if stringcodepage(a)<>1258 then
    halt(1);
end.
