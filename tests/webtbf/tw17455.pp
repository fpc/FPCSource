{ %fail }

{$mode macpas}
{$extendedsyntax off}

program crash;
var
  thePStr: string[ 255];
begin
  System.Move
    ( thePStr, MenuPtr^, Length( thePStr) + 1)
end.
