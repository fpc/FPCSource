var
 A: byte;
 w : word;
 B,B2: cardinal;
 s  : string;
 p : pointer;
 err : boolean;
begin
 B := $ffffffed;
 B2 := $fffffffd;
  p:=POinter(B-B2);
 Str(B-B2,s);
 writeln(s);
 if s<>'-16' then
   err:=true;

 W:=65535;
 A:=20;
 Str(a * w - 256000000,s);
 p:=POinter(a * w - 256000000);
 writeln(s);
{$ifdef cpu64}
 if s<>'18446744073454862316' then
{$else cpu64}
 if s<>'-254689300' then
{$endif cpu64}
  err:=true;


  if err then
    halt(1);
    
end.

