{$mode delphi}
type
   tc1 = class
      l : longint;
      property p : longint read l;
   end;

   tc2 = class(tc1)
      { in Delphi mode }
      { parameters can have the same name as properties }
      procedure p1(p : longint);
   end;

procedure tc2.p1(p : longint);

  begin
  end;

begin
end.
