{ %fail }

{ Source provided for Free Pascal Bug Report 3337 }
{ Submitted by "Vincent Snijders" on  2004-10-01 }
{ e-mail: vslist@zonnet.nl }

{$mode objfpc}

type
  TB=class
    function AddNewFile: integer; override;
  end;

function TB.AddNewFile: integer;
begin
  Result:= 5;
end;

begin
end.
