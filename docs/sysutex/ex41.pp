Program Example41;

{ Program to demonstrate the FileSearch function. }

Uses Sysutils;

Const
{$ifdef unix}
  FN = 'find';
  P = '.:/bin:/usr/bin';
{$else}
  FN = 'find.exe';
  P = 'c:\dos;c:\windows;c:\windows\system;c:\windows\system32';
{$endif}

begin
  Writeln ('find is in : ',FileSearch (FN,P));
end.
