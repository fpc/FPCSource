{ %TARGET=win64,wince,win32,linux }

{ Source provided for Free Pascal Bug Report 2364 }
{ Submitted by "Maarten Bekers" on  2003-02-08 }
{ e-mail: fpc-bugs@elebbs.com }
unit tw2364;

interface

type blah = function: integer;
function iee: integer;


var blah2: blah;

implementation

function iee: integer;
begin
end;

exports
  iee;

end.
