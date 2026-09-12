{ No OS services on PS1 bare metal. }
{$ifndef FPC_HAS_FEATURE_FILEIO}
function do_isdevice(handle: THandle): Boolean; inline;
begin
  Result := True;
end;
{$endif}