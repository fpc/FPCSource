{ Old file: tbs0171.pp }
{ missing typecasting in constant expression solved for pointers                                   OK 0.99.11 (PM) }

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
