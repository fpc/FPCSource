{ Old file: tbs0067.pp }
{  Shows incorrect symbol resolution when using uses in implementation More info can be found in file tbs0067b.pp. }

unit ub0060;

interface

type
  tlong=record
    a : longint;
  end;

procedure p(var t:tlong);

implementation

procedure p(var t:tlong);
begin
end;

end.
