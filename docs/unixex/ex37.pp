Program Example37;

{ Program to demonstrate the Popen function. }

uses BaseUnix,Unix;

var f : text;
    i : longint;

begin
  writeln ('Creating a shell script to which echoes its arguments');
  writeln ('and input back to stdout');
  assign (f,'test21a');
  rewrite (f);
  writeln (f,'#!/bin/sh');
  writeln (f,'echo this is the child speaking.... ');
  writeln (f,'echo got arguments \*"$*"\*');
  writeln (f,'cat');
  writeln (f,'exit 2');
  writeln (f);
  close (f);
  fpchmod ('test21a',&755);
  popen (f,'./test21a arg1 arg2','W');
  if fpgeterrno<>0 then
     writeln ('error from POpen : errno : ', fpgeterrno);
  for i:=1 to 10 do
    writeln (f,'This is written to the pipe, and should appear on stdout.');
  Flush(f);
  Writeln ('The script exited with status : ',PClose (f));
  writeln;
  writeln ('Press <return> to remove shell script.');
  readln;
  assign (f,'test21a');
  erase (f)
end.