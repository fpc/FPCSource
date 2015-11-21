{ %opt=-Fccp1258 -Msystemcodepage }

{ disabling the systemcodepage after enabling it goes back to the default
  setting, there is no stack of previously set code pages }

{$modeswitch systemcodepage-}

program tw28718c;

var
  a: ansistring;
begin
  a:='abc';
  if stringcodepage(a)<>CP_ACP then
    halt(1);
end.
