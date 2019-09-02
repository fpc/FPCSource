{ %FAIL }

{ prefixed attributes modeswitch disables procedure directives inside [...] }

program tcustomattr15;

{$mode delphi}

procedure Test; [cdecl];
begin
end;

begin
end.
