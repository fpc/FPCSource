{ %skipcpu=x86_64,powerpc64 }

{ %fail }

{$mode delphi}

var
  i: Int64;

begin

  for i := 1 to 10 do begin
    write ( '*' )
  end { for i }

; writeln
end.
