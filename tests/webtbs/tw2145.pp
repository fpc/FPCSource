{ %interactive }
{
1) move cursor in ' writeln(2); ' and press ctrl+F8: red line
2) press ctrl+F9
now the cursor is under 'w' of winreln(2);
3) press Enter here
4) press ctrl+F9
5) move cursor under ';' of 'writeln(1);' and press Canc
now red line is absent
6) press ctrl+F9
there is a breackpoint in 'writeln(2)' , but line isn't red :-(
---
then, close the IDE and re-run it, the red line is now present
Category: IDE(Text)
Type: Error
Date entered: 2002-09-25
Submitter:Salvatore Licciardi
}
 begin // ide DOS/Win 0.9.2 at 2002/08/25 writeln(1);
writeln(2);
writeln(3);
writeln(4);
writeln(5);
writeln(6);
writeln(7);
end.
