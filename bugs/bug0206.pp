PROGRAM SetRange_Bug;
CONST a:char='A';z:char='Z';
VAR s:set of char;c:char;
BEGIN
 s:=[a..z];
 for c:=#0 to #255 do
  if c in s then
   write(c);
 writeln;
END.