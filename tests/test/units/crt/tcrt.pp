{ %INTERACTIVE }
{

  Program to test CRT unit by Mark May.
  Only standard TP functions are tested (except WhereX, WhereY).
}
program tesicrt;

uses crt;
var
  i,j : longint;
  fil : text;
  c   : char;
begin
{Window/AssignCrt/GotoXY}
  clrscr;
  writeln ('This should be on a clear screen...');
  gotoxy (10,10);
  writeln ('(10,10) is the coordinate of this sentence');
  window  (10,11,70,22);
  writeln ('Window (10,11,70,22) executed.');
  writeln ('Sending some output to a file, assigned to crt.');
  assigncrt ( fil);
  rewrite (fil);
  writeln (fil,'This was written to the file, assigned to the crt.');
  writeln (fil,'01234567890123456789012345678901234567890123456789012345678901234567890');
  close (fil);
  writeln ('The above too, but this not any more');
  write ('Press any key to continue');
  c:=readkey;
  clrscr;
  writeln ('the small window should have been cleared.');
  write ('Press any key to continue');
  c:=readkey;

{Colors/KeyPressed}
  window (1,1,80,25);
  clrscr;
  writeln ('Color testing :');
  writeln;
  highvideo;
  write ('highlighted text');
  normvideo;
  write (' normal text ');
  lowvideo;
  writeln ('And low text.');
  writeln;
  writeln ('Color chart :');
  for i:=black to lightgray do
   begin
     textbackground (i);
     textcolor (0);
     write ('backgr. : ',i:2,' ');
     for j:= black to white do
      begin
        textcolor (j);
        write (' ',j:2,' ');
      end;
     writeln;
   end;
  normvideo;
  writeln ('The same, with blinking foreground.');
  for i:=black to lightgray do
   begin
     textbackground (i);
     textcolor (0);
     write ('backgr. : ',i:2,' ');
     for j:= black to white do
      begin
        textcolor (j+128);
        write (' ',j:2,' ');
      end;
     writeln;
   end;
  textcolor (white);
  textbackground (black);
  writeln;
  writeln ('press any key to continue');
  repeat until keypressed;
  c:=readkey;

{ClrEol/DelLine/InsLine}
  clrscr;
  writeln ('Testing some line functions :');
  writeln ;
  writeln ('This line should become blank after you press enter');
  writeln;
  writeln ('The following line should then become blank from column 10');
  writeln ('12345678901234567890');
  writeln;
  writeln ('This line should disappear.');
  writeln;
  writeln ('Between this line and the next, an empty line should appear.');
  writeln ('This is the next line, above which the empty one should appear');
  writeln;
  write ('Press any key to observe the predicted effects.');
  readkey;
  gotoxy(1,3);clreol;
  gotoxy (10,6);clreol;
  gotoxy (1,8);delline;
  gotoxy (1,10); insline;
  gotoxy (18,13); clreol;
  writeln ('end.');
  readkey;
end.
