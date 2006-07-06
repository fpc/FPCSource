program test_0_to_power_6;

uses
  math;
var
 result,number,exponent : integer;
begin
 number := 0;
 exponent := 6;
 result := number ** exponent;
 write (result);
 if result<>0 then
   begin
     Writeln(' 0 ** 6 should be equal to 0');
     Halt(1);
   end;
end.
