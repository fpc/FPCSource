{$ifdef go32v2}
uses
   dpmiexcp;
{$endif}

var
   e : extended;

begin
 e:=-1.0;
 writeln(ln(e));
// writeln(ln(0));
 writeln(power(0,1.0));
end .
