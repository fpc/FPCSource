 {
 ide 0.9.2 dos/win32 2002/05/31
press F7 or F8 more times:
the green row is in first position when work with the macro
This problem is more complex with more macros
Category: IDE(Text)
Type: Error
Date entered: 2002-06-12
Submitter:salvatore licciardi
Date Fixed: 2002-09-05
Fix version: 1.0.7

Fixer: Pierre Comment:
Was a general problem with the file position
within a macro. Merged to 1.1 branch. }

 {$macro on}
//you need 30..40 enter











{$include uw2004.inc}














begin
{$define x:= write('ooooo');
write('ooooo');}
x;
write('ppppp');
y;
x;
writeln;
end.
