{ %fail }

{$mode delphi}

var
  i: Int64;

begin
{$ifdef cpu64}
  The code below only has to fail for 32 bit cpus, so
  make sure the program fails to compile for another
  reason on 64 bit cpus
{$else cpu64}
  for i := 1 to 10 do begin
    write ( '*' )
  end { for i }

; writeln
{$endif cpu64}
end.
