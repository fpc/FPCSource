{ Source provided for Free Pascal Bug Report 1850 }
{ Submitted by "Sebastian Günther" on  2002-03-06 }
{ e-mail: sg@freepascal.org }
type
  TMyRecord = record
    a, b: Integer;
  end;
var
  r: TMyRecord;
begin
  with r do;
end.
