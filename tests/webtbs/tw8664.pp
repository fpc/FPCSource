{ %OPT=-gh }
program project1;

{$mode objfpc}{$H+}

procedure TLResourceListAdd(Values: array of string);
begin
end;


begin
  HaltOnNotReleased := true;
  TLResourceListAdd(['Value1']);
end.
