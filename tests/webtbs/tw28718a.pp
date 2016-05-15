{$codepage cp1258}

{ should cancel/override the codepage setting above }
{$modeswitch systemcodepage}

program tw28718a;

{ should restore the compiler's default code page (-> CP_ACP) }
{$modeswitch systemcodepage-}

var
  a: ansistring;
begin
  a:='abc';
  if stringcodepage(a)<>CP_ACP then
    halt(1);
end.
