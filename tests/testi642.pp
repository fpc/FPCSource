{$i ..\rtl\inc\int64.inc}

procedure dumpqword(q : qword);

  begin
     write('$',hexstr(tqwordrec(q).high,8),' ',hexstr(tqwordrec(q).low,8));
  end;

procedure dumpqwordln(q : qword);

  begin
     dumpqword(q);
     writeln;
  end;

procedure assignqword(h,l : dword;var q : qword);

  begin
     tqwordrec(q).high:=h;
     tqwordrec(q).low:=l;
  end;

procedure do_error(l : longint);

  begin
     writeln('Error near number ',l);
     halt(1);
  end;

procedure do_error;

  begin
     do_error(0);
  end;

{ $define error:=do_error({$line});}

procedure simpletestcmpqword;

  var
     q1,q2,q3,q4 : qword;

  begin
     assignqword(0,5,q1);
     assignqword(6,0,q2);
     assignqword(6,1,q3);
     assignqword(6,5,q4);
     { first test the code generation of the operators }
     if q1<>q1 then
       do_error(0);
     if q2<>q2 then
       do_error(0);
     if q3<>q3 then
       do_error(0);
     if not(q1=q1) then
       do_error(0);
     if not(q2=q2) then
       do_error(0);
     if not(q3=q3) then
       do_error(0);
     writeln('  <>,= succesfully tested');
  
     if q1>q2 then
       do_error(1100);
     if q2>q3 then
       do_error(1101);
     if q2<q1 then
       do_error(1102);
     if q3<q2 then
       do_error(1103);
     writeln('  <,> succesfully tested');

     if q1>=q2 then
       do_error(1104);
     if q2>=q3 then
       do_error(1105);
     if q2<=q1 then
       do_error(1106);
     if q3<=q2 then
       do_error(1107);
     writeln('  >=,<= succesfully tested');

     if q1=q2 then
       do_error(1108);
     if q2=q3 then
       do_error(1109);
     if q3=q1 then
       do_error(1111);

     if q1=q4 then
       do_error(1112);
     if q2=q4 then
       do_error(1113);
     if q3=q4 then
       do_error(1114);
     writeln('  More comparisations successful tested');
  end;

procedure testaddqword;

  var
     q1,q2,q3,q4,q5,q6 : qword;

  begin
     { without overflow between 32 bit }
     assignqword(0,5,q1);
     assignqword(0,6,q2);
     assignqword(0,1,q3);
     assignqword(0,11,q4);
     assignqword(0,1,q5);
     if q1+q2<>q4 then
       do_error(1200);
     if q1+q3+q1<>q4 then
       do_error(1201);
     if q1+(q3+q1)<>q4 then
       do_error(1202);
     if (q1+q3)+q1<>q4 then
       do_error(1203);
     { a more complex expression }
     if ((((q5+q3)+(q3+q5))+((q5+q3)+(q3+q5)))+q5+q3+q5)<>q4 then       
       do_error(1204);
     { with overflow between 32 bit }
     assignqword(0,$ffffffff,q1);
     assignqword(1,3,q2);
     assignqword(0,4,q3);
     assignqword(1,4,q4);
     assignqword(0,1,q5);
     assignqword(1,$fffffffe,q6);
     if q1+q3<>q2 then
       do_error(1205);
     if q3+q1<>q2 then
       do_error(1206);
     if q1+(q3+q5)<>q4 then
       do_error(1207);
     if (q1+q3)+q5<>q4 then
       do_error(1208);
     if (q1+q1)<>q6 then
       do_error(1209);
  end;

procedure testcmpqword;

  var
     q1,q2,q3,q4,q5,q6 : qword;

  begin
     assignqword(0,$ffffffff,q1);
     assignqword(0,$ffffffff,q2);
     assignqword(1,$fffffffe,q3);
     assignqword(0,2,q4);
     assignqword(1,$fffffffc,q5);
     if (q1+q2)<>q3 then
       do_error(1300);
     if not(q3=(q1+q2)) then
       do_error(1301);
     if (q1+q2)>q3 then
       do_error(1302);
     if (q1+q2)<q3 then
       do_error(1303);
     if not(q3<=(q1+q2)) then
       do_error(1304);
     if not(q3>=(q1+q2)) then
       do_error(1305);

     if (q1+q2)<>(q4+q5) then
       do_error(1306);
     if not((q4+q5)=(q1+q2)) then
       do_error(1307);
     if (q1+q2)>(q4+q5) then
       do_error(1308);
     if (q1+q2)<(q4+q5) then
       do_error(1309);
     if not((q4+q5)<=(q1+q2)) then
       do_error(1310);
     if not((q4+q5)>=(q1+q2)) then
       do_error(1311);
  end;

procedure testlogqword;

  var
     q0,q1,q2,q3,q4,q5,q6 : qword;

  begin
     assignqword(0,0,q0);
     assignqword($ffffffff,$ffffffff,q1);
     assignqword(0,$ffffffff,q2);
     assignqword($ffffffff,0,q3);
     assignqword($a0a0a0a0,$50505050,q4);
     assignqword(0,$50505050,q5);
     assignqword($a0a0a0a0,0,q6);

     { here we don't need to test all cases of locations, }
     { this is already done by the addtion test           }
     if (q2 or q3)<>q1 then
       do_error(1400);
     if (q5 or q6)<>q4 then
       do_error(1401);

     if (q2 and q3)<>q0 then
       do_error(1402);
     if (q5 and q6)<>q0 then
       do_error(1403);

     if (q2 xor q3)<>q1 then
       do_error(1404);
     if (q5 xor q6)<>q4 then
       do_error(1405);
     { the test before could be also passed by the or operator! }
     if (q4 xor q4)<>q0 then
       do_error(1406);
  end;

procedure testshlshrqword;

  var
     q0,q1,q2,q3,q4,q5 : qword;
     l1,l2 : longint;

  begin
     assignqword(0,0,q0);
     assignqword($ffff,$ffff0000,q1);
     assignqword(0,$ffffffff,q2);
     assignqword($ffffffff,0,q3);
     assignqword(0,1,q4);
     assignqword($80000000,0,q5);

     l1:=16;
     l2:=0;
     if (q1 shl 16)<>q3 then
       do_error(1500);
     if (q1 shl 48)<>q0 then
       do_error(1501);
     if (q1 shl 47)<>q5 then
       do_error(1501);
     if ((q1+q0) shl 16)<>q3 then
       do_error(1502);
     if ((q1+q0) shl 48)<>q0 then
       do_error(1503);
     if ((q1+q0) shl 47)<>q5 then
       do_error(15031);

     if (q1 shl l1)<>q3 then
       do_error(1504);
     if (q1 shl (3*l1))<>q0 then
       do_error(1505);
     if ((q1+q0) shl l1)<>q3 then
       do_error(1506);
     if ((q1+q0) shl (3*l1))<>q0 then
       do_error(1507);
     if ((q1+q0) shl (3*l1-1))<>q5 then
       do_error(15071);

     if (q1 shl (l1+l2))<>q3 then
       do_error(1508);
     if ((q1+q0) shl (l1+l2))<>q3 then
       do_error(1509);

     if (q1 shr 16)<>q2 then
       do_error(1510);
     if (q1 shr 48)<>q0 then
       do_error(1511);
     if (q1 shr 47)<>q4 then
       do_error(15111);

     if ((q1+q0) shr 16)<>q2 then
       do_error(1512);
     if ((q1+q0) shr 48)<>q0 then
       do_error(1513);
     if (q1 shr l1)<>q2 then
       do_error(1514);
     if (q1 shr (3*l1))<>q0 then
       do_error(1515);
     if (q1 shr (3*l1-1))<>q4 then
       do_error(15151);

     if ((q1+q0) shr l1)<>q2 then
       do_error(1516);
     if ((q1+q0) shr (3*l1))<>q0 then
       do_error(1517);
     if ((q1+q0) shr (3*l1-1))<>q4 then
       do_error(15171);

     if (q1 shr (l1+l2))<>q2 then
       do_error(1518);
     if ((q1+q0) shr (l1+l2))<>q2 then
       do_error(1519);
  end;

procedure testsubqword;

  var
     q0,q1,q2,q3,q4,q5,q6 : qword;

  begin
     { without overflow between 32 bit }
     assignqword(0,0,q0);
     assignqword(0,6,q1);
     assignqword(0,5,q2);
     assignqword(0,1,q3);
     assignqword(0,11,q4);
     assignqword(0,1,q5);
     if q1-q2<>q3 then
       do_error(1600);
     if q1-q0-q1<>q0 then
       do_error(1601);
     if q1-(q0-q1)<>q1+q1 then
       do_error(1602);
     if (q1-q0)-q1<>q0 then
       do_error(1603);

     { a more complex expression }
     if ((((q5-q3)-(q3-q5))-((q5-q3)-(q3-q5))))<>q0 then       
       do_error(1604);

     { with overflow between 32 bit }
     assignqword(1,0,q1);
     assignqword(0,$ffffffff,q2);
     assignqword(0,1,q3);
     assignqword(1,$ffffffff,q4);

     if q1-q2<>q3 then
       do_error(1605);
     if q1-q0-q2<>q3 then
       do_error(1606);
     if q1-(q0-q2)<>q4 then
       do_error(1607);
     if (q1-q0)-q1<>q0 then
       do_error(1608);

     assignqword(1,$ffffffff,q5);
     assignqword(1,$ffffffff,q4);

     { a more complex expression }
     if ((((q5-q3)-(q3-q5))-((q5-q3)-(q3-q5))))<>q0 then       
       do_error(1609);
  end;

procedure testnotqword;

  var
     q0,q1,q2,q3,q4 : qword;

  begin
     assignqword($f0f0f0f0,$f0f0f0f0,q1);
     assignqword($f0f0f0f,$f0f0f0f,q2);
     assignqword($f0f0f0f0,0,q3);
     assignqword(0,$f0f0f0f0,q4);
     if not(q1)<>q2 then
       do_error(1700);
     if not(q3 or q4)<>q2 then
       do_error(1701);

     { do a more complex expression to stress the register saving }
     if not(q3 or q4)<>not(q3 or q4) then
       do_error(1702);
  end;

procedure testmulqword;

  var
     q0,q1,q2,q3,q4,q5,q6 : qword;
     i : longint;

  begin
     assignqword(0,0,q0);
     assignqword(0,1,q1);
     assignqword(0,4,q2);
     assignqword(2,0,q3);
     assignqword(8,0,q4);
     assignqword(0,1,q5);
     assignqword($ffff,$1234431,q6);
     { to some trivial tests       }
     { to test the code generation }
     if q1*q2<>q2 then
       do_error(1800);
     if q1*q2*q3<>q4 then
       do_error(1801);
     if q1*(q2*q3)<>q4 then
       do_error(1802);
     if (q1*q2)*q3<>q4 then
       do_error(1803);

     if (q6*q5)*(q1*q2)<>q1*q2*q5*q6 then
       do_error(1804);

     { a more complex expression }
     if ((((q1*q5)*(q1*q5))*((q5*q1)*(q1*q5)))*q5*q1*q5)<>q1 then
       do_error(1805);

     { now test the multiplication procedure with random bit patterns }
     for i:=1 to 1000000 do
       begin
          tqwordrec(q1).high:=0;
          tqwordrec(q1).low:=random($ffffffff);
          tqwordrec(q2).high:=0;
          tqwordrec(q2).low:=random($ffffffff);
          if q1*q2<>q2*q1 then
            begin
               write('Multiplication of ');
               dumpqword(q1);
               write(' and ');
               dumpqword(q2);
               writeln(' failed');
               do_error(1806);
            end;
       end;
     for i:=1 to 1000000 do
       begin
          tqwordrec(q1).high:=0;
          tqwordrec(q1).low:=random($ffffffff);
          q1:=q1 shl 16;
          tqwordrec(q2).high:=0;
          tqwordrec(q2).low:=random($ffff);
          if q1*q2<>q2*q1 then
            begin
               write('Multiplication of ');
               dumpqword(q1);
               write(' and ');
               dumpqword(q2);
               writeln(' failed');
               do_error(1806);
            end;
       end;
  end;

function testf : qword;

  var
     q : qword;

  begin
     assignqword($ffffffff,$a0a0a0a0,q);
     testf:=q;
  end;

procedure testfuncword;

  var
     q : qword;

  begin
     assignqword($ffffffff,$a0a0a0a0,q);
     if testf<>q then
       do_error(1900);
     if q<>testf then
       do_error(1901);       
  end;

var
   q : qword;

begin
   randomize;
   writeln('------------------------------------------------------');
   writeln('                    QWord test ');
   writeln('------------------------------------------------------');
   writeln;

   writeln('Testing assignqword and dumpqword ... ');
   assignqword($12345678,$9ABCDEF0,q);
   dumpqword(q);
   writeln;
   writeln('The output should be:');
   writeln('$12345678 9ABCDEF0');
   writeln;

   writeln('Testing simple QWord comparisations');
   simpletestcmpqword;
   writeln('Testing simple QWord comparisations was successful');
   writeln;

   writeln('Testing QWord additions');
   testaddqword;
   writeln('Testing QWord additions was successful');
   writeln;

   writeln('Testing more QWord comparisations');
   testcmpqword;
   writeln('Testing more QWord comparisations was successful');
   writeln;

   writeln('Testing QWord subtraction');
   testsubqword;
   writeln('Testing QWord subtraction was successful');
   writeln;

   writeln('Testing QWord logical operators (or,xor,and)');
   testlogqword;
   writeln('Testing QWord logical operators (or,xor,and) was successful');
   writeln;

   writeln('Testing QWord logical not operator');
   testnotqword;
   writeln('Testing QWord logical not operator was successful');
   writeln;

   writeln('Testing QWord logical shift operators (shr,shr)');
   testshlshrqword;
   writeln('Testing QWord logical shift operators (shr,shr) was successful');
   writeln;

   writeln('Testing QWord function results');
   testfuncword;
   writeln('Testing QWord function results was successful');
   writeln;

   writeln('Testing QWord multiplications');
   testmulqword;
   writeln('Testing QWord multiplications was successful');
   writeln;

   writeln('------------------------------------------------------');
   writeln('              QWord test successful');
   writeln('------------------------------------------------------');
   halt(0);
end.
