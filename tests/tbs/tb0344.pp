{ %version=1.1 }

{$R+}
var
  s : string;
  error : boolean;
begin
  error:=false;
  str(high(int64),s);
  if s<>'9223372036854775807' then
   begin
     writeln('high(int64) error!: "',s,'"');
     error:=true;
   end;
  str(low(int64),s);
  if s<>'-9223372036854775808' then
   begin
     writeln('low(int64) error!: "',s,'"');
     error:=true;
   end;
{$ifdef fpc}
  str(high(qword),s);
  if s<>'18446744073709551615' then
   begin
     writeln('high(qword) error!: "',s,'"');
     error:=true;
   end;
  str(low(qword),s);
  if s<>'0' then
   begin
     writeln('low(qword) error!: "',s,'"');
     error:=true;
   end;
{$endif}
  if error then
   halt(1);
end.
