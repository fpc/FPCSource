{ from GPC test suite }

Program fjf7;

Var
  S: String [ 80 ];
  astr: ansistring;

begin
  WriteStr ( astr, '' : 5, 'OKabcdf' : 7 );
  if (length ( astr ) <> 5 + 7) or
     (copy(astr,6,2) <> 'OK') then
    halt(1);

  WriteStr ( S, '' : 5, 'OKabcdf' : 7 );
  if length ( S ) = 5 + 7 then
    halt(ord(copy(S,6,2) <> 'OK'))
  else
    halt(1);
end.

