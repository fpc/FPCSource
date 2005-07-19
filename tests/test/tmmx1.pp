{ %CPU=i386 }

{ this contains currently only a basic test of mmx support }
{ the following instructions are tested:
   PSUBW
   PSUBUSW
   PADDW
   PADDUSW
}
uses
   mmx;

procedure do_error(l : longint);

  begin
     writeln('Error near number ',l);
     halt(1);
  end;

function equal(const v1,v2 : tmmxword) : boolean;

  var
     i : integer;

  begin
     equal:=false;
     for i:=0 to 3 do
       if v1[i]<>v2[i] then
         exit;
     equal:=true;
  end;

procedure testmmxword;

  var t1,t5 : tmmxword;

  const
     c0 : tmmxword = (0,0,0,0);
     c1 : tmmxword = (1,1,1,1);
     c2 : tmmxword = (1234,4321,1111,33333);
     c3 : tmmxword = (1234,4321,2222,11111);
     c4 : tmmxword = (2468,8642,3333,44444);
     c5 : tmmxword = ($ffff,$ffff,$ffff,$ffff);

  begin
     {$mmx+}
     { Intel: paddw }
     t1:=c2+c3;
     if not(equal(t1,c4)) then
       do_error(1000);

     { Intel: psubw }
     t5:=t1-c2;
     if not(equal(t5,c3)) then
       do_error(1001);
     t1:=not(c0);

     { does a not }
     if not(equal(t1,c5)) then
       do_error(1002);

     { test the saturation }
     {$saturation+}
     t1:=c5+c2+c3;
     if not(equal(t1,c5)) then
       do_error(1003);

     t1:=c4-c5-t1;
     if not(equal(t1,c0)) then
       do_error(1004);
     {$saturation-}
  end;

begin
   if not(is_mmx_cpu) then
     begin
        writeln('!!!! Warning: You need a mmx capable CPU to run this test !!!!');
        halt(0);
     end;
   writeln('Testing basic tmmxword support');
   testmmxword;
   writeln('Test succesful');
   writeln;
end.
