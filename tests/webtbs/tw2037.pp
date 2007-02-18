{ Source provided for Free Pascal Bug Report 2037 }
{ Submitted by "David Hagler" on  2002-07-11 }
{ e-mail: david@avimark.net }
program tw2037;

{$R-}

const
  FILE_FLAG_WRITE_THROUGH = -2147483648;
  FILE_ATTRIBUTE_NORMAL = 128;

var
  anattr : cardinal;

begin
  anattr := FILE_FLAG_WRITE_THROUGH or FILE_ATTRIBUTE_NORMAL;
end.
