{ $OPT=-Twin32 }
library tbs0263;

{
  The export directive is not necessary anymore in delphi, it's a leftover
  from the 16bit model, just like near and far.
}

procedure p;
begin
end;

exports
  p name 'p';
end.
