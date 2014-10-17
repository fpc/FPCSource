var
  c:boolean;
Begin
    c:=18446744073709551615>=9223372036854775807;
    if not(c) then
      halt(1);
End.
