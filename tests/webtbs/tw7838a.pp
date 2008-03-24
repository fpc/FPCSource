{ %norun }
{ %target=win32,win64,wince,linux}

library tw7838a;

{$mode objfpc} {$h+}

uses uw7838a;

{$ifdef win32}
const
 progname = '.\prog.exe';
{$endif}




function dllf: longint; 
begin
  result:=exetest;
  if (result<>aa) then
    halt(1);
end;

exports dllf;

begin

end.
