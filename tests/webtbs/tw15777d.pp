{ %opt=-vw -Sew }

{$mode macpas}

type
    Int8 = -128..127;
    Int16 = integer;
    Int32 = longint;
    Rec1 = packed record f1, f2: Int8 end;
    Rec2 = packed record f1, f2: Int16 end;
    Rec3 = packed record f1, f2: Int32 end;

procedure test1(l: univ Int32);
begin
    writeln(l)
end;

procedure test2(l: Int32);
begin
    writeln(l)
end;

procedure test3(var l: univ Int32);
begin
    writeln(l)
end;

procedure test4(const l: univ Int32);
begin
    writeln(l)
end;

procedure testit;
var
    s: single;
    d: double;
    i8: Int8;
    i16: Int16;
    i32: Int32;
    r1: rec1;
    r2: rec2;
    r3: rec3;
begin
    s:=1.0;
    d:=1.0;
    i8:=1;
    i16:=1;
    r2.f1:=1;
    r2.f1:=1;
    i32:= Int32( s);
    test1(s);
    test3(s);
    test4(s);
// not supported by FPC since the sizes differ
//    test1(d);
    test1(i32);
    test2(i32);
    test3(i32);
    test4(i32);
    test1(1.0);
    test4(1.0);
    test1(2.0);
    test4(2.0);
    test1(r2);
    test3(r2);
    test4(r2);
    test1(i8);
    test4(i8);
    test1(i16);
    test4(i16);
    i8:= Int8(i32);
    i8:= Int8(i16);
    i16:= Int16(i32);
    i32:= Int32(i16);
end;

begin
    testit
end.

