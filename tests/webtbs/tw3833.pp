{ Source provided for Free Pascal Bug Report 3833 }
{ Submitted by "Joost vd Sluis" on  2005-03-26 }
{ e-mail:  }
uses
  variants,sysutils;
var v1,v2     : variant;

begin
  v1 := 255;
  v2 := 255 + v1;
  if v2<>2*255 then
    begin
      writeln(v2);
      halt(1);
    end;
  v1 := '256';
  v2 := 'Hello:' + v1;
  if v2<>'Hello:256' then
    begin
      writeln(v2);
      halt(1);
    end;
  v1 := 255.0;
  v2 := 255.0 + v1;
  if v2<>2*255 then
    begin
      writeln(v2);
      halt(1);
    end;
  writeln('ok');
end.
