{ Source provided for Free Pascal Bug Report 2328 }
{ Submitted by "Pavel V. Ozerski" on  2003-01-20 }
{ e-mail: ozerski@list.ru }
{$ifdef fpc}
{$mode objfpc}
{$endif}
type
 tClassA=class
  procedure DefaultHandler(var Message);override;
 end;
procedure tClassA.DefaultHandler(var Message);
 begin
  inherited //;
 end;
begin
end.
