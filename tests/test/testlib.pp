{$ifdef win32}
 {$define supported}
{$endif win32}
{$ifdef linux}
 {$define supported}
{$endif linux}

{$ifdef supported}

library bug;

const
   publicname='TestName';
   publicindex = 1234;

procedure Test;export;

 begin
 end;

exports
  Test name publicname;
exports
  Test index publicindex;

begin
end.
{$else supported}
begin
  Writeln('No library for that target');
end.
{$endif supported}
