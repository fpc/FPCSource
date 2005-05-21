{ Source provided for Free Pascal Bug Report 3005 }
{ Submitted by "Michalis Kamburelis" on  2004-03-04 }
{ e-mail: michalis@camelot.homedns.org }
uses Math;
begin
 { This already works (result is integer) }
 Writeln((-2) ** 2);
 { Two things below do not work currently }
 Writeln(Power(-2.0, 2.0));
 if Power(-2.0, 2.0)<>4 then
   halt(1);
 Writeln((-2.0) ** 2.0);
 if (-2.0) ** 2.0<>4 then
   halt(1);
 writeln('ok');
end.
