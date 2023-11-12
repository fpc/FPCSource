
var
  a, b: array[0..515] of byte;

procedure test_index;
var
  i, j, k: longint;
  index: longint;
  l: qword;

  procedure verify(got, expected: sizeint; const funcname: string; exitcode: TExitCode);
  begin
    if got<>expected then
      begin
        writeln(funcname,' error for (',i,',',j,',',k,'): got ',got,', expected ',expected);
        halt(exitcode);
      end;
  end;

begin
  for i := 0 to 512 do
    a[i] := byte(i);
  for i := 0 to 256 do
    for j := 0 to 31 do
      for k := 0 to 31 do
        begin
          fillchar(b,sizeof(b),0);
          move(a[j],b[k+8],i);
          { for i = 256, every element appears in the array }
          if (byte(i)>0) then
            index:=i-1
          else
            index:=0;
          verify(indexbyte(b[k+8],index,a[j+i]),-1,'indexbyte 1',1);
          if b[k+8]=0 then
            index:=0
          else if (b[k+8]+i>=256) then
            index:=256-b[k+8]
          else
            index:=i;
          verify(indexbyte(b[k+8],i+1,0),index,'indexbyte 2',2);
          {same for length=-1}
          verify(indexbyte(b[k+8],-1,0),index,'indexbyte 2a',22);


          verify(indexbyte(b[k+8],i,b[k+8+i-1]),i-1,'indexbyte 3',3);
          {same for length=-1}
          if i<>0 then   // previous test will be no-op when i=0
            verify(indexbyte(b[k+8],-1,b[k+8+i-1]),i-1,'indexbyte 3a',23);


          if (i<1) then
            index:=-1
          else
            index:=i shr 1;
          verify(indexbyte(b[k+8],i,b[k+8+i shr 1]),index,'indexbyte 4',4);
          {same for length=-1}
          if i<>0 then  // previous test will be no-op when i=0
            verify(indexbyte(b[k+8],-1,b[k+8+i shr 1]),index,'indexbyte 4a',24);


          if (i=0) then
            index:=-1
          else
            index:=0;
          verify(indexbyte(b[k+8],i,b[k+8]),index,'indexbyte 5',5);
          {same for length=-1}
          if i<>0 then
            verify(indexbyte(b[k+8],-1,b[k+8]),index,'indexbyte 5a',25);


          verify(indexword(b[k+8],i shr 1,0),-1,'indexword 6',6);

          if (unaligned(pword(@b[k+8])^)=0) then
            index:=0
          else if (i=0) then
            index:=-1
          else if (b[k+8+i-1] = 0) and
                  odd(i) then
            index:=((i+1) shr 1)-1
          else
            index:=((i+1) shr 1);
          verify(indexword(b[k+8],(i+1) shr 1 + 1,0),index,'indexword 7',7);
          {same for length=-1}
          verify(indexword(b[k+8],-1,0),index,'indexword 7a',27);

          index:=i shr 1;
          l:=unaligned(pword(@(b[k+8+(i and not 1)]))^);
          verify(indexword(b[k+8],i shr 1+1,l),index,'indexword 8',8);
          {same for length=-1}
          verify(indexword(b[k+8],-1,l),index,'indexword 8a',28);

          l:=unaligned(pword(@(b[k+8+((i shr 2) and not 1)-2]))^);
          index:=((i shr 2) and not 1) shr 1 - 1;
          verify(indexword(b[k+8],i shr 1,l),index,'indexword 9',9);
          if (i>1) and (index<>-1) then
            verify(indexword(b[k+8],-1,l),index,'indexword 9a',29);

          l:=unaligned(pword(@(b[k+8]))^);
          if (i<2) then
            index:=-1
          else
            index:=0;
          verify(indexword(b[k+8],i shr 1,l),index,'indexword 10',10);
          if i>1 then
            verify(indexword(b[k+8],-1,l),index,'indexword 10a',30);


          if (unaligned(pdword(@b[k+8])^)=0) then
            index:=0
          else if (i=0) then
            index:=-1
          else if (b[k+8+i-1] = 0) and
                  ((i mod 4) = 1) then
            index:=((i+3) shr 2)-1
          else
            index:=((i+3) shr 2);
          verify(indexdword(b[k+8],(i+3) shr 2 + 1,0),index,'indexdword 11',11);

          index:=i shr 2;
          l:=unaligned(pdword(@(b[k+8+(i and not 3)]))^);
          verify(indexdword(b[k+8],i shr 2+1,l),index,'indexdword 12',12);

          l:=unaligned(pdword(@(b[k+8+((i shr 3) and not 3)-4]))^);
          index:=((i shr 3) and not 3) shr 2 - 1;
          verify(indexdword(b[k+8],i shr 2,l),index,'indexdword 13',13);
          l:=unaligned(pdword(@(b[k+8]))^);
          if (i<4) then
            index:=-1
          else
            index:=0;
          verify(indexdword(b[k+8],i shr 2,l),index,'indexdword 14',14);


          if (unaligned(pqword(@b[k+8])^)=0) then
             index:=0
           else if (i=0) then
             index:=-1
           else if (b[k+8+i-1] = 0) and
                   ((i mod 8) = 1) then
             index:=((i+7) shr 3)-1
           else
             index:=((i+7) shr 3);
          verify(indexqword(b[k+8],(i+7) shr 3 + 1,0),index,'indexqword 15',15);

          index:=i shr 3;
          l:=unaligned(pqword(@(b[k+8+(i and not 7)]))^);
          verify(indexqword(b[k+8],i shr 3+1,l),index,'indexqword 16',16);

          l:=unaligned(pqword(@(b[k+8+((i shr 4) and not 7)-8]))^);
          index:=((i shr 4) and not 7) shr 3 - 1;
          verify(indexqword(b[k+8],i shr 3,l),index,'indexqword 17',17);
          l:=unaligned(pqword(@(b[k+8]))^);
          if (i<4) then
            index:=-1
          else
            index:=0;
          verify(indexqword(b[k+8],i shr 2,l),index,'indexqword 18',18);
        end;
end;


begin
  test_index;
end.
