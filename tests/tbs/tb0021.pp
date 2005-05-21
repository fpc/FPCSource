{ Old file: tbs0025.pp }
{  tests for a wrong uninit. var. warning              OK 0.9.3 }

procedure p1;
type
  datetime=record
    junk : string;
end;
var
  dt : datetime;
begin
  fillchar(dt,sizeof(dt),0);
end;

begin
  P1;
end.
