{ %FAIL }
{$mode objfpc}
type
   to1 = class
      procedure p;virtual;
   end;

   to2 = class(to1)
     function p : longint;override;
   end;

  procedure to1.p;

    begin
    end;

  function to2.p : longint;

    begin
    end;

begin
end.
