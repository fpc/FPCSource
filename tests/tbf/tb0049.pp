{ %FAIL }
{ Old file: tbf0246.pp }
{ const para can be changed without error              OK 0.99.13 (PFV) }

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
