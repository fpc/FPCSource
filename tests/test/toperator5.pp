{ %version=1.1 }

Program toperator5;

uses ucomplex;

const
   REAL_ONE = 14.0;
   REAL_TWO = 12.0;
   REAL_THREE = 1999.0;
   IM_ONE = 7.5;
   IM_TWO = 15.2;
   IM_THREE = 11.1;


   procedure fail;
    begin
      WriteLn('Failed!');
      Halt(1);
    end;

    procedure TestAssign;
     var
      j: real;
      z: complex;
     begin
      Write('Testing assignment operator...');
      j:=12.4;
      z:=j;
      if trunc(z.re) <> trunc(12.4) then
        fail;
      WriteLn('Success!');
     end;

     procedure TestComplexAdd;
      var
       i,j: complex;
      begin
        Write('Testing add operator...');
        i.re:=REAL_ONE;
        i.im:=IM_ONE;
        j.re:=REAL_TWO;
        j.im:=IM_TWO;
        i:=i + j;
        if trunc(i.re) <> trunc(REAL_ONE+REAL_TWO) then
          fail;
        if trunc(i.im) <> trunc(IM_ONE+IM_TWO) then
          fail;
        WriteLn('Success!');
      end;

    procedure TestComplexSubtract;
      var
       i,j: complex;
     begin
        Write('Testing subtract operator...');
        i.re:=REAL_ONE;
        i.im:=IM_ONE;
        j.re:=REAL_TWO;
        j.im:=IM_TWO;
        i:=i - j;
        if trunc(i.re) <> trunc(REAL_ONE-REAL_TWO) then
          fail;
        if trunc(i.im) <> trunc(IM_ONE-IM_TWO) then
          fail;
        WriteLn('Success!');
     end;


    procedure TestComplexMultiply;
      var
       i,j: complex;
     begin
        Write('Testing multiply operator...');
        i.re:=REAL_ONE;
        i.im:=IM_ONE;
        j.re:=REAL_TWO;
        j.im:=IM_TWO;
        i:=i * j;
        if trunc(i.re) <> trunc((REAL_ONE*REAL_TWO)-(IM_ONE*IM_TWO)) then
          fail;
        if trunc(i.im) <> trunc((IM_ONE*REAL_TWO) + (IM_TWO*REAL_ONE)) then
          fail;
        WriteLn('Success!');
     end;



    procedure TestComplexEqual;
      var
       i,j: complex;
     begin
        Write('Testing equality operator...');
        i.re:=REAL_ONE;
        i.im:=IM_ONE;
        j.re:=REAL_ONE;
        j.im:=IM_ONE;
        if not (i = j) then
          fail;
        WriteLn('Success!');
     end;


    procedure TestComplexNegate;
      var
       i : complex;
     begin
        Write('Testing negate operator...');
        i.re:=REAL_ONE;
        i.im:=IM_ONE;
        i:=-i;
        if trunc(i.re) <> trunc(-REAL_ONE) then
          fail;
        if trunc(i.im) <> trunc(-IM_ONE) then
          fail;
        WriteLn('Success!');
     end;

Begin
  TestAssign;
  TestComplexAdd;
  TestComplexSubtract;
  TestComplexMultiply;
  TestComplexEqual;
  TestComplexNegate;
end.
