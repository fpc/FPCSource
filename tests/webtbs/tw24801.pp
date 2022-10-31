program ErrorSample;
{$mode objfpc}{$H+}
uses
    {$IFDEF UNIX}
    cThreads,
    {$ENDIF}
  Classes,
  uw24801;
begin
  if f<>'asdfasdf' then
    halt(1);
end.         
