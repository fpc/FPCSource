type
  tref=record
    ofs : longint;
  end;

procedure p(const ref:tref);
begin
  with ref do
   ofs:=ofs+1;   { This should issue an error, because ref is const ! }
end;

begin
end.