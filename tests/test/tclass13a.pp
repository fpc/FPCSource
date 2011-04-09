{ %norun }
{$mode delphi}

type
  tc = class
   type
    tnest = class
    end;
  end;
  td = class(tc)
   type
     tnest = class(tc.tnest)
     end;
  end;

begin
end.
