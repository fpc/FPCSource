program tb318;

Type
  TRec = record
    X,Y : longint;
    end;

  TRecFile = File of TRec;

var TF : TRecFile;
    LF : File of longint;
    i,j,k,l : longint;
    t : Trec;

begin
  Write ('Writing files...');
  assign (LF,'longint.dat');
  rewrite (LF);
  for i:=1 to 10 do
   write (LF,i);
  close (LF);
  Assign (TF,'TRec.dat');
  rewrite (TF);
  for i:=1 to 10 do
    for j:=1 to 10 do
      begin
      t.x:=i;
      t.y:=j;
      write (TF,T);
      end;
  close (TF);
  writeln ('Done');
  reset (LF);
  reset (TF);
  Write ('Sequential read test...');
  for i:=1 to 10 do
    begin
    read (LF,J);
    if j<>i then writeln ('Read of longint failed at :',i);
    end;
  for i:=1 to 10 do
    for j:=1 to 10 do
      begin
      read (tf,t);
      if (t.x<>i) or (t.y<>j) then
        writeln ('Read of record failed at :',i,',',j);
      end;
  writeln ('Done.');
  Write ('Random access read test...');
  For i:=1 to 10 do
    begin
    k:=random(10);
    seek (lf,k);
    read (lf,j);
    if j<>k+1 then
     Writeln ('Failed random read of longint at pos ',k,' : ',j);
    end;
  For i:=1 to 10 do
    for j:=1 to 10 do
      begin
      k:=random(10);
      l:=random(10);
      seek (tf,k*10+l);
      read (tf,t);
      if (t.x<>k+1) or (t.y<>l+1) then
        Writeln ('Failed random read of longint at pos ',k,',',l,' : ',t.x,',',t.y);
      end;
  Writeln ('Done.');
  close (lf);
  close (TF);
  erase (lf);
  erase (tf);

end.
