{$mode objfpc}
type
   tc1 = class
   end;

   tc2 = class(tc1)
   end;

   tcoc1 = class of tc1;
   tcoc2 = class of tc2;

procedure p(const a : array of tcoc1);

  begin
  end;

begin
   p([tc2]);
end.
