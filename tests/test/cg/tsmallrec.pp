

program test_small_records;

type
  rec1 = record
    c1 : char;
  end;

  rec2 = record
    c1,c2 : char;
  end;

  rec3 = record
    c1,c2,c3 : char;
  end;

  rec4 = record
    c1,c2,c3,c4 : char;
  end;

  rec5 = record
    c1,c2,c3,c4,c5 : char;
  end;

  rec6 = record
    c1,c2,c3,c4,c5,c6 : char;
  end;

  rec7 = record
    c1,c2,c3,c4,c5,c6,c7 : char;
  end;

  rec8 = record
    c1,c2,c3,c4,c5,c6,c7,c8 : char;
  end;

const
  r1 : rec1 = ( c1 : 'a' );
  r2 : rec2 = ( c1 : 'a'; c2 : 'b' ); 
  r3 : rec3 = ( c1 : 'a'; c2 : 'b'; c3 : 'c' );
  r4 : rec4 = ( c1 : 'a'; c2 : 'b'; c3 : 'c'; c4 : 'd' );
  r5 : rec5 = ( c1 : 'a'; c2 : 'b'; c3 : 'c'; c4 : 'd'; c5 : 'e' );
  r6 : rec6 = ( c1 : 'a'; c2 : 'b'; c3 : 'c'; c4 : 'd'; c5 : 'e'; c6 : 'f' );
  r7 : rec7 = ( c1 : 'a'; c2 : 'b'; c3 : 'c'; c4 : 'd'; c5 : 'e'; c6 : 'f'; c7 : 'g' );
  r8 : rec8 = ( c1 : 'a'; c2 : 'b'; c3 : 'c'; c4 : 'd'; c5 : 'e'; c6 : 'f'; c7 : 'g'; c8 : 'h' );
  haserrors : boolean = false;
procedure seterror(size:byte);
begin
  writeln('Error for size ',size);
  haserrors:=true;
end;

procedure test_r1(r : rec1);
begin
  if (r.c1<>r1.c1) then
    seterror(sizeof(rec1));
end;

procedure test_r2(r : rec2);
begin
  if (r.c1<>r2.c1) or (r.c2<>r2.c2) then
    seterror(sizeof(rec2));
end;

procedure test_r3(r : rec3);
begin
  if (r.c1<>r3.c1) or (r.c2<>r3.c2) or (r.c3<>r3.c3) then
    seterror(sizeof(rec3));
end;

procedure test_r4(r : rec4);
begin
  if (r.c1<>r4.c1) or (r.c2<>r4.c2) or (r.c3<>r4.c3) or (r.c4<>r4.c4) then
    seterror(sizeof(rec4));
end;

procedure test_r5(r : rec5);
begin
  if (r.c1<>r5.c1) or (r.c2<>r5.c2) or (r.c3<>r5.c3) or (r.c4<>r5.c4) or (r.c5<>r5.c5) then
    seterror(sizeof(rec5));
end;

procedure test_r6(r : rec6);
begin
  if (r.c1<>r6.c1) or (r.c2<>r6.c2) or (r.c3<>r6.c3) or (r.c4<>r6.c4) or (r.c5<>r6.c5) or (r.c6<>r6.c6) then
    seterror(sizeof(rec6));
end;

procedure test_r7(r : rec7);
begin
  if (r.c1<>r7.c1) or (r.c2<>r7.c2) or (r.c3<>r7.c3) or (r.c4<>r7.c4) or (r.c5<>r7.c5) or (r.c6<>r7.c6) or (r.c7<>r7.c7) then
    seterror(sizeof(rec7));
end;

procedure test_r8(r : rec8);
begin
  if (r.c1<>r8.c1) or (r.c2<>r8.c2) or (r.c3<>r8.c3) or (r.c4<>r8.c4) or (r.c5<>r8.c5) or (r.c6<>r8.c6) or (r.c7<>r8.c7) or (r.c8<>r8.c8) then
    seterror(sizeof(rec8));
end;

procedure test_long_r1(r_1,r_2,r_3,r_4,r_5,r_6,r_7,r_8,r9,r10,r11,r12,r13,r14,r15,r16,r17,r18,r19,r20,r : rec1);
begin
  if (r.c1<>r1.c1) then
    seterror(sizeof(rec1));
end;

procedure test_long_r2(r_1,r_2,r_3,r_4,r_5,r_6,r_7,r_8,r9,r10,r11,r12,r13,r14,r15,r16,r17,r18,r19,r20,r : rec2);
begin
  if (r.c1<>r2.c1) or (r.c2<>r2.c2) then
    seterror(sizeof(rec2));
end;

procedure test_long_r3(r_1,r_2,r_3,r_4,r_5,r_6,r_7,r_8,r9,r10,r11,r12,r13,r14,r15,r16,r17,r18,r19,r20,r : rec3);
begin
  if (r.c1<>r3.c1) or (r.c2<>r3.c2) or (r.c3<>r3.c3) then
    seterror(sizeof(rec3));
end;

procedure test_long_r4(r_1,r_2,r_3,r_4,r_5,r_6,r_7,r_8,r9,r10,r11,r12,r13,r14,r15,r16,r17,r18,r19,r20,r : rec4);
begin
  if (r.c1<>r4.c1) or (r.c2<>r4.c2) or (r.c3<>r4.c3) or (r.c4<>r4.c4) then
    seterror(sizeof(rec4));
end;

procedure test_long_r5(r_1,r_2,r_3,r_4,r_5,r_6,r_7,r_8,r9,r10,r11,r12,r13,r14,r15,r16,r17,r18,r19,r20,r : rec5);
begin
  if (r.c1<>r5.c1) or (r.c2<>r5.c2) or (r.c3<>r5.c3) or (r.c4<>r5.c4) or (r.c5<>r5.c5) then
    seterror(sizeof(rec5));
end;

procedure test_long_r6(r_1,r_2,r_3,r_4,r_5,r_6,r_7,r_8,r9,r10,r11,r12,r13,r14,r15,r16,r17,r18,r19,r20,r : rec6);
begin
  if (r.c1<>r6.c1) or (r.c2<>r6.c2) or (r.c3<>r6.c3) or (r.c4<>r6.c4) or (r.c5<>r6.c5) or (r.c6<>r6.c6) then
    seterror(sizeof(rec6));
end;

procedure test_long_r7(r_1,r_2,r_3,r_4,r_5,r_6,r_7,r_8,r9,r10,r11,r12,r13,r14,r15,r16,r17,r18,r19,r20,r : rec7);
begin
  if (r.c1<>r7.c1) or (r.c2<>r7.c2) or (r.c3<>r7.c3) or (r.c4<>r7.c4) or (r.c5<>r7.c5) or (r.c6<>r7.c6) or (r.c7<>r7.c7) then
    seterror(sizeof(rec7));
end;

procedure test_long_r8(r_1,r_2,r_3,r_4,r_5,r_6,r_7,r_8,r9,r10,r11,r12,r13,r14,r15,r16,r17,r18,r19,r20,r : rec8);
begin
  if (r.c1<>r8.c1) or (r.c2<>r8.c2) or (r.c3<>r8.c3) or (r.c4<>r8.c4) or (r.c5<>r8.c5) or (r.c6<>r8.c6) or (r.c7<>r8.c7) or (r.c8<>r8.c8) then
    seterror(sizeof(rec8));
end;

begin
  test_r1(r1);
  test_r2(r2);
  test_r3(r3);
  test_r4(r4);
  test_r5(r5);
  test_r6(r6);
  test_r7(r7);
  test_r8(r8);
  test_long_r1(r1,r1,r1,r1,r1,r1,r1,r1,r1,r1,r1,r1,r1,r1,r1,r1,r1,r1,r1,r1,r1);
  test_long_r2(r2,r2,r2,r2,r2,r2,r2,r2,r2,r2,r2,r2,r2,r2,r2,r2,r2,r2,r2,r2,r2);
  test_long_r3(r3,r3,r3,r3,r3,r3,r3,r3,r3,r3,r3,r3,r3,r3,r3,r3,r3,r3,r3,r3,r3);
  test_long_r4(r4,r4,r4,r4,r4,r4,r4,r4,r4,r4,r4,r4,r4,r4,r4,r4,r4,r4,r4,r4,r4);
  test_long_r5(r5,r5,r5,r5,r5,r5,r5,r5,r5,r5,r5,r5,r5,r5,r5,r5,r5,r5,r5,r5,r5);
  test_long_r6(r6,r6,r6,r6,r6,r6,r6,r6,r6,r6,r6,r6,r6,r6,r6,r6,r6,r6,r6,r6,r6);
  test_long_r7(r7,r7,r7,r7,r7,r7,r7,r7,r7,r7,r7,r7,r7,r7,r7,r7,r7,r7,r7,r7,r7);
  test_long_r8(r8,r8,r8,r8,r8,r8,r8,r8,r8,r8,r8,r8,r8,r8,r8,r8,r8,r8,r8,r8,r8);
  if not haserrors then
    writeln('Test successful')
  else
    begin
      writeln('There are errors in the generated code');
      halt(1);
    end;
end.
 
