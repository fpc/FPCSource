{$mode delphi}

procedure error1(a, b: array of string);
begin
end;

procedure error2(a, b: array of byte);
begin
end;

begin
 error1(['abc'], ['xyz']);
 error2([1,2,3,4], [2,1,0]);
end.
