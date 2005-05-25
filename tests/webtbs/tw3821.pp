{ %OPT=-Sew }

{ Source provided for Free Pascal Bug Report 3821 }
{ Submitted by "Matthias Hryniszak" on  2005-03-24 }
{ e-mail: matthias@hryniszak.de }
program Test;

uses
  SyncObjs;

var
  Event: TSimpleEvent;

begin
  // The following line produces a compile-time
  // warning
  Event := TSimpleEvent.Create;
end.
