{$H+}
const trap : pchar = 'boese Falle';
var  s : ansistring;
begin
   s:='Dies ist eine ';
   s:=s+'boese Falle';  { see .Ll4: }
   s:=s+'.';            { see .Ll5: }
   writeln(s);
end.
