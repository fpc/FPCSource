{%TARGET=go32v2 }
{ %NOTE=This test requires a go32v2 NASM assembler }
{ %OPT=-Anasmcoff }

{ Check mantis bug report 2710 }

program worldcrash;
var
  test : string;
begin
writeln('Hello, world!');
test:='Hello, world!';
  if (test[1]<>'H') or (test[length(test)]<>'!') then
    begin
      writeln('Error within tw7210.pp test');
      halt(1);
    end;
end.
