{ %norun }
{ %opt=-Sx }

{ this is file a.pas }

begin   try write('') finally write('') end   end .

{
$
$
$ fpc -Mfpc -Sx a
a.pas(3,13) Error: Identifier not found "try"
. . . .
$
$
$ fpc -Mfpc -Mexceptions a
$
$
}
