{ Stupid compare test
  but it once failed for m68k code PM }

var
  st : string;

begin

  st:='t';

  if st > 'ta' then
   begin
     writeln('Error "t">"ta" is wrong !');
     runerror(1);
   end
  else
   writeln('String compare works OK !');
end.
