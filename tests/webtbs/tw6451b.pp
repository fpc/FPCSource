{ %fail }
{$codepage utf8}

var
  a: char;
begin
  a:='a';
  { can't convert widechar to char, because don't know what }
  { encoding to use at compile-time                         }
  if a in ['a', 'é'] then;
end.
