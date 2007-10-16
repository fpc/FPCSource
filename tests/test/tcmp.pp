const
  err: boolean = false;

var
  a, b: array[0..512] of byte;

procedure test_compare;
type
  pdword = ^cardinal;
var
  i, j, k: longint;
  l: longint;
begin
  for i := 0 to 512 do
    a[i] := byte(i);
  for i := 0 to 256 do
    for j := 0 to 31 do
      for k := 0 to 31 do
        begin
          fillchar(b,sizeof(b),0);
          move(a[j],b[k+4],i);
          if comparebyte(a[j],b[k+4],i)<>0 then
            begin
              writeln('cmpbyte error 1 for (',i,',',j,',',k,')');
              halt(1);
            end;
          if comparebyte(a[j],b[k+4],i+1)<0 then
            begin
              writeln(a[j+i],' ',b[k+4+i]);
              writeln('cmpbyte error 2 for (',i,',',j,',',k,')');
              halt(2);
            end;

          if (i and 1 = 0) then
            begin
              if compareword(a[j],b[k+4],i shr 1)<>0 then
                begin
                  writeln('cmpword error 3 for (',i,',',j,',',k,')');
                  halt(3);
                end;
              if compareword(a[j],b[k+4],i shr 1 + 1)<0 then
                begin
                  writeln('cmpword error 4 for (',i,',',j,',',k,')');
                  halt(4);
                end;
            end
          else
            if compareword(a[j],b[k+4],(i+1) shr 1)<0 then
              begin
                writeln('cmpword error 5 for (',i,',',j,',',k,')');
                halt(5);
              end;

          if (i and 3 = 0) then
            begin
              if comparedword(a[j],b[k+4],i shr 2)<>0 then
                begin
                  writeln('cmpdword error 6 for (',i,',',j,',',k,')');
                  halt(6);
                end;
              if comparedword(a[j],b[k+4],i shr 2 + 1)<=0 then
                begin
                  writeln(comparedword(a[j],b[k+4],i shr 2+1));
                  writeln(pdword(@a[j])^,' ',pdword(@b[k+4])^);
                  writeln(pdword(@a[j+i])^,' ',pdword(@b[k+4+i])^);
                  writeln(pdword(@a[j+i+4])^,' ',pdword(@b[k+4+i+4])^);
                  writeln('cmpdword error 7 for (',i,',',j,',',k,')');
                  halt(7);
                end;
            end
          else
            if comparedword(a[j],b[k+4],(i+3) shr 2)<0 then
              begin
                writeln(comparedword(a[j],b[k+4],(i+3) shr 2));
                writeln(pdword(@a[j])^,' ',pdword(@b[k+4])^);
                writeln(pdword(@a[j+(i+3) shr 2-1])^,' ',pdword(@b[k+4+(i+3) shr 2-1])^);
                writeln('cmpdword error 8 for (',i,',',j,',',k,')');
                halt(8);
              end;
        end;
end;


begin
  test_compare;
end.
