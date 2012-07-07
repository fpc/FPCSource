Var a,b:qword;
      c:boolean;
      aa,bb:longword;      
Begin
    a:=qword($FFFFFFFFFFFFFFFF);
    b:=9223372036854775807;
    c:=a>b;
    if not c then
      halt(1);
    if not(qword($FFFFFFFFFFFFFFFF)>9223372036854775807) then
      halt(2);
    c:=qword($FFFFFFFFFFFFFFFF)>b;
    if not c then
      halt(3);
    c:=18446744073709551615>=9223372036854775807;  
    if not c then
      halt(4);
    
    
    aa:=$FFFFFFFF;
    bb:=2147483647;
    c:=aa>bb;
    if not c then
      halt(5);
    if not ($FFFFFFFF>2147483647) then
      halt(6);
    c:=$FFFFFFFF>bb;
    if not c then
      halt(7);
    c:=4294967295>=2147483647;
    if not c then
      halt(8);
End.
