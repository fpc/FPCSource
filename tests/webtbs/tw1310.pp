type
  Days     = (Mon, Tue, Wed, Thu, Fri, Sat, Sun);
  Weekdays = Mon..Fri;
  Weekend  = Sat..Sun;
begin
  WriteLn('Days     ', Ord(Low(Days))    , Ord(High(Days))    :2);
  WriteLn('Weekdays ', Ord(Low(Weekdays)), Ord(High(Weekdays)):2, ' should be 4!');
  WriteLn('Weekend  ', Ord(Low(Weekend)) , Ord(High(Weekend)) :2);
  if ord(high(weekdays))<>4 then
   begin
     writeln('ERROR!');
     Halt(1);
   end;
end.
