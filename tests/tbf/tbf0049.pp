type
   days = (Mon,Tue,Wed,Thu,Fri,Sat,Sun);
   weekend = Sat..Sun;

var
   w : weekend;

begin
   w:=5;
   {$message the line before should produce an error }
end.
