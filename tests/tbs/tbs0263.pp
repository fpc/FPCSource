{$if linux or win32}
library tbs0263;

{
  The export directive is not necessary anymore in delphi, it's a leftover
  from the 16bit model, just like near and far.
}

procedure testp;
begin
end;

exports
  testp name 'testp';

end.
{$else}
begin
end.
{$endif}
