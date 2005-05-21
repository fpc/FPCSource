procedure ttt(i1:integer;s1:string[255];b1:byte;i2:integer;
  i3:integer;i4:integer;c1:char;s2,s3,s4,s5:string[255]);

begin
     if (i1 <> 1) or
        (s1 <> 'test1') or
        (b1 <> 1) or
        (i2 <> 2) or
        (i3 <> 3) or
        (i4 <> 4) or
        (c1 <> 'A') or
        (s2 <> 'test2') or
        (s3 <> 'test3') or
        (s4 <> 'test4') or
        (s5 <> 'test5') then
      halt(1);
end;

begin
ttt(1,'test1',1,2,3,4,'A','test2','test3','test4','test5');
end.
