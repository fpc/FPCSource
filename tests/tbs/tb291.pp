{ Old file: tbs0338.pp }
{  }

{$mode delphi}

{$define skip}

begin
 writeln('Hello world!');
{$ifndef skip}
 write('}');
{$endif skip}
end.
