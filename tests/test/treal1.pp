{$E-}

 Procedure TestSub;
 var
  i : Real;
  j : Real;
 Begin
  i:=99.9;
  j:=10.0;
  i:=i-j;
  Write('RESULT SHOULD BE: 89.9 :');
  WriteLn(i);
  i:=j-i;
  Write('RESULT SHOULD BE: -79.9 :');
  WriteLn(i);
  j:=j-10.0;
  Write('RESULT SHOULD BE: 0.0 :');
  WriteLn(j);
 end;

 Function TestAdd(i : real): Real;
 Begin
   i:=i+1.5;
   if i > 10.0 then
   Begin
     Write('RESULT SHOULD BE: 10.5 :');
     WriteLn(i);
     exit;
   end;
   TestAdd:=TestAdd(i);
 end;

 Procedure TestDiv;
 var
  i : Real;
  j : Real;
 Begin
  i:=-99.9;
  j:=10.0;
  i:=i / j;
  Write('RESULT SHOULD BE: -9.9 :');
  WriteLn(i);
  i:=j / i;
  Write('RESULT SHOULD BE: -1.01 :');
  WriteLn(i);
  j:=i / 10.0;
  Write('RESULT SHOULD BE: -0.1001 :');
  WriteLn(j);
 end;



 Procedure TestComplex;
 var
  i : real;
 Begin
   Write('RESULT SHOULD BE 2.09 :');
   i := 4.4;
   WriteLn(Sqrt(i));
   Write('RESULT SHOULD BE PI :');
   WriteLn(Pi);
   Write('RESULT SHOULD BE 4.0 :');
   WriteLn(Round(3.6));
 end;


Begin
 WriteLn('------------ SUB ---------------');
 TestSub;
 WriteLn('------------ ADD ---------------');
 TestAdd(0);
 WriteLn('------------ DIV ---------------');
 TestDiv;
 WriteLn('------------ COMPLEX ---------------');
 TestComplex;
end.
