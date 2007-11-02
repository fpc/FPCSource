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
          if comparebyte(b[k+4],a[j],i+1)>0 then
            begin
              writeln(b[k+4+i],' ',a[j+i]);
              writeln('cmpbyte error 3 for (',i,',',j,',',k,')');
              halt(3);
            end;

          if (i and 1 = 0) then
            begin
              if compareword(a[j],b[k+4],i shr 1)<>0 then
                begin
                  writeln('cmpword error 4 for (',i,',',j,',',k,')');
                  halt(4);
                end;
              if compareword(a[j],b[k+4],i shr 1 + 1)<0 then
                begin
                  writeln('cmpword error 5 for (',i,',',j,',',k,')');
                  halt(5);
                end;
              if compareword(b[k+4],a[j],i shr 1 + 1)>0 then
                begin
                  writeln('cmpword error 6 for (',i,',',j,',',k,')');
                  halt(6);
                end;
            end
          else
            begin
              if compareword(a[j],b[k+4],(i+1) shr 1)<0 then
                begin
                  writeln('cmpword error 7 for (',i,',',j,',',k,')');
                  halt(7);
                end;
              if compareword(b[k+4],a[j],(i+1) shr 1)>0 then
                begin
                  writeln('cmpword error 8 for (',i,',',j,',',k,')');
                  halt(8);
                end;
            end;

          if (i and 3 = 0) then
            begin
              if comparedword(a[j],b[k+4],i shr 2)<>0 then
                begin
                  writeln('cmpdword error 9 for (',i,',',j,',',k,')');
                  halt(9);
                end;
              if comparedword(a[j],b[k+4],i shr 2 + 1)<=0 then
                begin
                  writeln(comparedword(a[j],b[k+4],i shr 2+1));
                  writeln(unaligned(pdword(@a[j])^),' ',unaligned(pdword(@b[k+4])^));
                  writeln(unaligned(pdword(@a[j+i])^),' ',unaligned(pdword(@b[k+4+i])^));
                  writeln(unaligned(pdword(@a[j+i+4])^),' ',unaligned(pdword(@b[k+4+i+4])^));
                  writeln('cmpdword error 10 for (',i,',',j,',',k,')');
                  halt(10);
                end;
              if comparedword(b[k+4],a[j],i shr 2 + 1)>=0 then
                begin
                  writeln(comparedword(b[k+4],a[j],i shr 2+1));
                  writeln(unaligned(pdword(@b[k+4])^),' ',unaligned(pdword(@a[j])^));
                  writeln(unaligned(pdword(@b[k+4+i])^),' ',unaligned(pdword(@a[j+i])^));
                  writeln(unaligned(pdword(@b[k+4+i+4])^),' ',unaligned(pdword(@a[j+i+4])^));
                  writeln('cmpdword error 11 for (',i,',',j,',',k,')');
                  halt(11);
                end;
            end
          else
            begin
              if comparedword(a[j],b[k+4],(i+3) shr 2)<0 then
                begin
                  writeln(comparedword(a[j],b[k+4],(i+3) shr 2));
                  writeln(unaligned(pdword(@a[j])^),' ',unaligned(pdword(@b[k+4])^));
                  writeln(unaligned(pdword(@a[j+(i+3) shr 2-1])^),' ',unaligned(pdword(@b[k+4+(i+3) shr 2-1])^));
                  writeln('cmpdword error 12 for (',i,',',j,',',k,')');
                  halt(12);
                end;
              if comparedword(b[k+4],a[j],(i+3) shr 2)>0 then
                begin
                  writeln(comparedword(b[k+4],a[j],(i+3) shr 2));
                  writeln(unaligned(pdword(@b[k+4])^),' ',unaligned(pdword(@a[j])^));
                  writeln(unaligned(pdword(@b[k+4+(i+3) shr 2-1])^),' ',unaligned(pdword(@a[j+(i+3) shr 2-1])^));
                  writeln('cmpdword error 13 for (',i,',',j,',',k,')');
                  halt(13);
                end;
            end;
        end;
end;


begin
  test_compare;
end.
