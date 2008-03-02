{ %TIMEOUT=20 }

const
  err: boolean = false;

var
  a, b: array[0..512] of byte;

procedure test_forward_move;
var
  i, j, k: longint;
  l: longint;
begin
  for i := 0 to 512 do
    begin
      a[i] := byte(i);
      b[i] := 0;
    end;
  for i := 0 to 256 do
    for j := 0 to 31 do
      for k := 0 to 31 do
        begin
          move(a[j],b[k+4],i);
          { check whether we didn't write a byte too early }
          for l := 0 to k+3 do
            begin
              if b[l] <> 0 then
                begin
                  writeln('Forward test error 1');
                  err := true;
                  halt(1);
                end;
              b[l] := 0;
            end;
          { check whether the actual values were copied correctly }
          for l := k+4 to k+i+3 do
            begin
              if b[l] <> a[j+l-(k+4)] then
                begin
                  writeln('Forward test error 2');
                  err := true;
                  halt(1);
                end;
              b[l] := 0;
            end;
          { check whether we didn't write past the end }
          for l := k+i+4 to 512 do
            begin
              if b[l] <> 0 then
                begin
                  writeln('Forward test error 3');
                  err := true;
                  halt(1);
                end;
              b[l] := 0;
            end;
        end;
end;


procedure test_backward_move;
var
  i, j, k: longint;
  l: longint;
begin
  for i := 0 to 128 do
    begin
      for j := 0 to 31 do
        for k := 0 to 31 do
          begin
            for l := 0 to 255 do
              begin
                a[l] := l;
              end;

            move(a[127-j],a[127-j-k],i);
            { check whether we didn't write a byte too early }
            for l := 0 to 127-j-k-1 do
              begin
                if a[l] <> l then
                  begin
                    writeln('Backward test error 1');
                    err := true;
                    halt(1);
                  end;
              end;
            { check whether the actual values were copied correctly }
            for l := 127-j-k to 127-j-k+i-1 do
              begin
                if a[l] <> l+k then
                  begin
                    writeln('Backward test error 2');
                    err := true;
                    halt(1);
                 end;
              end;
            { check whether we didn't write past the end }
            for l := 127-j-k+i to 255 do
              begin
                if a[l] <> l then
                  begin
                    writeln('Backward test error 3');
                    err := true;
                    halt(1);
                  end;
              end;
          end;
    end;
end;


begin
  test_forward_move;
  test_backward_move;
  if err then
    halt(1);
end.
