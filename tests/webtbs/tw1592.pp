{ Source provided for Free Pascal Bug Report 1592 }
{ Submitted by "Guenther Palfinger" on  2001-08-23 }
{ e-mail: guenther.palfinger@gmx.net }
Program ShowBug;                                          (* 2001-08-23 *)

var L,R,A,B,Z1,tmp : real;

function arccos(x: real): real;
var y : real;
begin
   (*  gdb gives the following message for next line:
    *  "Program received signal SIGFPE, Arithmetic exception." *)
   writeln(x);
   if abs(x) > 1.0 then writeln(' error arccos(x), x = ',x:7:3);
   if abs(x) > 0.0 then y := arctan(sqrt(1.0-x*x)/abs(x))
   else y := pi/2.0;
   if x < 0.0 then y := pi - y;
   arccos := y;
end;

function arcsin(x: real): real;
begin
   arcsin := pi/2.0 - arccos(x);
end;

begin
   L := 5.2631578947368425;
   R := 3.6315789473684212;
   A := 39.88919667590028;
   B := 15.512465373961222;
   (* Behaves OK *)
   tmp :=  1/(pi*R)*(ArcCos(B/A) - 1/(2*L)*(sqrt((A+2)*(A+2)-2*R*R)*ArcCos(B/(R*A)) + B*ArcSin(1/R) ));
   writeln ('tmp = ', tmp);
   (* OK *)
   writeln('1/R-tmp = ', 1/R-tmp);
   (* Next line causes FPE at run time, althogh it is the same as previous line *)
   Z1 := 1/R-  1/(pi*R)*(ArcCos(B/A) - 1/(2*L)*(sqrt((A+2)*(A+2)-2*R*R)*ArcCos(B/(R*A)) + B*ArcSin(1/R) ));
end.
