{ Source provided for Free Pascal Bug Report 2363 }
{ Submitted by "Maarten Bekers" on  2003-02-07 }
{ e-mail: bugs-fpc@elebbs.com }
unit tw2363;

{$mode delphi}

interface

type blah = function: integer;
function iee: integer; export;


var blah2: blah;

implementation

function iee: integer; export;
begin
end;

begin
  blah2 := iee;
end.
