{ %NORUN }
{$ifdef win32}
library test;

  procedure exporttest;export;

    begin
    end;

  exports exporttest;
{$endif}

begin
end.
