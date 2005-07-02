{ %target=win32}
{ Source provided for Free Pascal Bug Report 3820 }
{ Submitted by "Matthias Hryniszak" on  2005-03-24 }
{ e-mail: matthias@hryniszak.de }
program Test;

uses
  ActiveX;
  
begin
  // the following imports are missing:
  OleInitialize(nil);
  OleUninitialize;
end.