{ %NORUN }

{$ifdef win32}
 {$define supported}
 {$define supportidx}
{$endif win32}
{$ifdef Unix}
 {$define supported}
{$endif Unix}

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
{$ifdef supportidx}
exports
  Test index publicindex;
{$endif}

begin
end.
{$else supported}
begin
  Writeln('No library for that target');
end.
{$endif supported}
