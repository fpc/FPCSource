{$if not(fpc_fullversion>20202)}
  {$error Problem with fpc_fullversion}
{$endif}

{ force compiler error if it's defined wrong }
{$if fpc_fullversion>20202)}
begin
end.
{$endif}
