{ %result=201 }

{ from GPC test suite }

program mir034e;
{$r+}

type range = 10..13;
var k : range;

begin
   ReadStr ('14', k); { over ubound }
end.
