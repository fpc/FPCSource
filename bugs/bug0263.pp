library bug0263;

{
  The export directive is not necessary anymore in delphi, it's a leftover
  from the 16bit model, just like near and far.
}

procedure p1;export;
begin
end;

exports
  p1 name 'p1';

begin
end.
