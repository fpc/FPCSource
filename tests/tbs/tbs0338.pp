{$define skip}

program test;
begin
 writeln('Hello world!');
{$ifndef skip}
 write('}');
{$endif skip}
end.
