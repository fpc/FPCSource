{ Source provided for Free Pascal Bug Report 2643 }
{ Submitted by "Wayne Sullivan" on  2003-08-19 }
{ e-mail: Wayne.Sullivan@cnri.dit.ie }
program pbug;
var d:double;
    s:string;
    s1:string;
begin
   d:=5168568.5;
   str(d:10,s);
   if s<>' 5.17E+006' then
     begin
       writeln(s);
       halt(1);
     end;
   str(d:11,s);
   if s<>' 5.169E+006' then
     begin
       writeln(s);
       halt(1);
     end;
   str(d:22,s);
   if sizeof(extended) > 8 then
     s1 := ' 5.16856850000000E+006'
   else
     s1 := ' 5.16856850000000E+006';
   if s<>s1 then
     begin
       writeln(s);
       halt(1);
     end;
end.
