type
  pstring=^string;
const
  drivestr:string='c:';
  pdrivestr:pstring=pstring(@drivestr);
begin
  if pdrivestr^<>'c:' then
    begin
       Writeln('Error in typecast of const');
       Halt(1);
    end;
end.
