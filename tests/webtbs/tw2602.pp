{ %version=1.1 }

{ Source provided for Free Pascal Bug Report 2602 }
{ Submitted by "Pavel V. Ozerski" on  2003-07-25 }
{ e-mail: ozerski@list.ru }
{$ifdef FPC}
{$mode Delphi}
{$endif}
begin
 with TObject.Create do
  Destroy;
end.
