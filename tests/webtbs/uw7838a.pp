unit uw7838a;
{$mode objfpc} {$h+}

interface

var
  aa: longint;

{$ifdef windows}
function exetest: longint; external 'tw7838b.exe'; 
{$endif}

{$ifdef unix}
function exetest: longint; external name 'exetest'; 
{$endif}


implementation

initialization
  writeln('libunit1 initialization');
  aa:=5;

finalization
  
  writeln('libunit1 finalization');
  aa:=-5;
end.
