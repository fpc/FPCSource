{ Source provided for Free Pascal Bug Report 2643 }
{ Submitted by "Wayne Sullivan" on  2003-08-19 }
{ e-mail: Wayne.Sullivan@cnri.dit.ie }
program pbug;
var d:double;
    s:string;
begin
   d:=5168568.5;
   str(d:10,s);
   if s<>' 5.17E+006' then
     begin
       writeln(s);
       halt(1);
     end;
   str(d:11,s);
   if s<>' 5.170E+006' then
     begin
       writeln(s);
       halt(1);
     end;
   str(d,s);
   if s<>' 5.168568500000000E+006' then
     begin
       writeln(s);
       halt(1);
     end;
end.
