{ %opt=-Xe }

{$packrecords 16}
{$codealign varmin=16}
{$codealign localmin=16}

type
  rec=record
    v:array[0..511] of byte;
  end;

var
  a,b:longword;
  x:rec;
  y: longword;

begin
  a:=b;
  y:=1;
  writeln(hexstr(@a));
  if ptruint(@a) and $f<>0 then
    begin
      writeln('ERROR in alignment of a');
      halt(1);
    end;
  writeln(hexstr(@b));
  if ptruint(@b) and $f<>0 then
    begin
      writeln('ERROR in alignment of b');
      halt(1);
    end;
  writeln(hexstr(@x));
  if ptruint(@x) and $f<>0 then
    begin
      writeln('ERROR in alignment of x');
      halt(1);
    end;
  writeln(hexstr(@y));
  if ptruint(@y) and $f<>0 then
    begin
      writeln('ERROR in alignment of y');
      halt(1);
    end;
end.
