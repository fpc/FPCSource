{$ifdef CPUAVR}
{ avr uses instruction based init/finalization of units: this does not work if an error happens during finalization,
  it results in an endless loop, so skip this test for avr }
begin
end.
{$else CPUAVR}
uses
   erroru,uunit1;

begin
   if testvar<>1234567 then
     do_error(1000);
end.
{$endif CPUAVR}