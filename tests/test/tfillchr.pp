const
  err: boolean = false;

var
  a: array[0..512] of byte;

procedure test_fillchar;
var
  i, j: longint;
  k: byte;
  l: longint;
begin
  for i := 0 to 512 do
    begin
      a[i] := 255;
    end;
  { lengths }
  for i := 0 to 256 do
    { alignments }
    for j := 0 to 31 do
      { values }
      for k := 0 to 1 do
        begin
          fillchar(a[j+4],i,k);
          { check whether we didn't write a byte too early }
          for l := 0 to j+3 do
            begin
              if a[l] <> 255 then
                begin
                  writeln('Fillchar test error 1');
                  err := true;
                  halt(1);
                end;
              a[l] := 255;
            end;
          { check whether the actual values were written correctly }
          for l := j+4 to j+i+3 do
            begin
              if a[l] <> k then
                begin
                  writeln('Fillchar test error 2');
                  err := true;
                  halt(1);
                end;
              a[l] := 255;
            end;
          { check whether we didn't write past the end }
          for l := j+i+4 to 512 do
            begin
              if a[l] <> 255 then
                begin
                  writeln('Fillchar test error 3');
                  err := true;
                  halt(1);
                end;
              a[l] := 255;
            end;
        end;
end;

begin
  test_fillchar;
  if err then
    halt(1);
end.
