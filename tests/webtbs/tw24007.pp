var
    str: bitpacked array [1..6] of 'a'..'z';
    i: integer;
    ch: char;
    error: boolean;
begin
    error := false;
    for i := 1 to 6 do str[i] := chr(ord('a')+i-1);
    
    for i := 1 to 6 do begin
        write('str[i] = ''', str[i], '''; ord(str[2]) = ',ord(str[i]));
	ch:=str[i]; {if we had used directly str[i] in the expression below, the correct value would have been read}
	if ch <> chr(ord(str[i])) then
          begin
           write(' ==> Bug: chr(',ord(ch),') read, excpected chr(',ord('a')+i-1,')');
           error:=true;
          end;
	writeln;
    end;
  halt(ord(error));
end.
