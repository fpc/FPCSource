{ %FAIL }
{ Old file: tbf0049.pp }
{  shows an error while defining subrange types         OK 0.99.7 (PFV) }

type
   days = (Mon,Tue,Wed,Thu,Fri,Sat,Sun);
   weekend = Sat..Sun;

var
   w : weekend;

begin
   w:=5;
   {$message the line before should produce an error }
end.
