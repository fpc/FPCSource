{ %NORUN }
{ Old file: tbs0263.pp }
{ export directive is not necessary in delphi anymore  OK 0.99.13 (PFV) }

{$ifdef Unix}
  {$define doit}
{$endif}
{$ifdef win32}
  {$define doit}
{$endif}
{$ifdef doit}
library tb0225;

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
