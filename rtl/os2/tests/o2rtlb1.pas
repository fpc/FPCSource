program testread;
{uses crt;}
var
  cadena,cadena2 : string;
  number : real;
begin
  {clrscr;}
  cadena2 := 'Previous string';
  write ('Enter the string ');
  readln (cadena);
  writeln ('You entered ',cadena);
  writeln ('Previous string was ',cadena2);
  write ('Enter a number ');
  readln (number);
  writeln ('Number entered was ',number);
  readln;
end.
{(I have retyped now because my computer is not connected to the net, but I
think that there are no errors).
Now you can do some tests:
1- Compile and run the program as is (that is, using crt). You will find that
      a) the program does not erase the screen (that is normal because we have
commented clrscr), but the cursor goes to the first line, thus overwriting the
screen.
   b) While the program is expecting the string to be entered, some of the keys
do not work correctly: Backspace advances some spaces (just like tab), tab key
does not work and the cursor keys write garbage. (however this is only in the
screen, because if you have erased a part of the string it will be actually
erased).
   c) Once you have press return, the message 'You entered...' appears in the
same line as the text entered.

2- Uncomment the clrscr call, cokpile and execute. Point a of test 1 will be
solved (the screen is erased, so nothing is overwritten), but points b and c
persist.

3- Comment 'uses crt' and 'clrscr'. Now you will not be using crt. Now:
   a) Point a of test 1 does not appear: the program begins to write in the
next line, it does not overwrite anything.
   b) Now all the keys (tab, backspace..) work as expected.
   c) Now the message 'You entered...' appears in the following line, so point
c of test 1 is also solved.
   d) BUT it writes only 'You entered', WITHOUT writing the string cadena (!).
It writes also 'Previous string was previous string', so the problem is in
readln and not in writeln.

4- To see if the problem is only in the string vars, uncomment the definition
of number, and also the three lines at the end that deal with number. Now ld
gives the following error message:

testread.pp:0 (testread.o): undefined symbol READ_TEXT_INTEGER referenced from
text segment.

This error happens with 'uses crt' and also without it.

5- Define number as word. Regardless of crt we get the following error from ld:

testread.pp:0 (testread.o): undefined symbol READ_TEXT_WORD referenced from
text segment.

6- Uncomment 'uses crt' if it was commented, and change the definition of
number as real. The program will compile, and it will print the number,
although in the same line as the input.

7- Finally, comment 'uses crt' again. This time it will also compile and link,
but it gives a runtime error!

Laufzeitfehler 106 bei 66422

This error is shown before printing the number.

I expect that these bug report will be useful to debug the RTL. Tonight I will
try to work in the blockwrite problem.

Best regards

Ramon

--
}
