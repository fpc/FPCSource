{$ifdef go32v2}
uses
   dpmiexcp;
{$endif}

var
   e : extended;

begin
 writeln('ln(0) = ',ln(0));
 writeln(' zero ^ one = ',power(0,1.0));
 e:=563545;
 writeln('exp(',e,') = ',exp(e));
end .
