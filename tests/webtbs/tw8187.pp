program testtime_console;
  
  uses SysUtils;
  
Procedure Check(No : Integer; A,B : String);

begin
  if A<>B then
    begin
    Writeln('Test ',No,' failed: ',A,'<>',B);
    Halt(No);
    end;
end;  
  
Procedure CheckTime;
  
  
begin
  Check(1,TimeToStr(0),'00:00:00');
  Check(2,FormatDateTime('hh:nn:ss',StrToTime('12:00:00 AM')),'00:00:00');
  Check(3,TimeToStr(StrToTime('12:00:00 AM')),'00:00:00');
  Check(4,TimeToStr(StrToTime('12:35:00 PM')),'12:35:00');
end;
  
begin
  LongTimeFormat:='hh:nn:ss';
  CheckTime;
end.