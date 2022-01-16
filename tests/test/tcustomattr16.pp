{ %FAIL }

{ prefixed attributes modeswitch disables procedure directives inside [...] }

program tcustomattr16;

{$mode objfpc}
{$modeswitch prefixedattributes}

procedure Test; [cdecl];
begin
end;

begin
end.
