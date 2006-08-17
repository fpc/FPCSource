{ %result=201 }

{$r+}
{$packenum 1}
Type
  MySet = (msOne,msTwo,msThree,msFour,msFive);

  Var
    aVar1: array[msOne..msFive] of LongInt = (1,2,3,4,5);
    aVar2: array[msTwo..msFive] of LongInt = (2,3,4,5);
    mVar: MySet;
    v1,v2: LongInt;

   begin
     byte(mVar) := 100;
     v1 := aVar1[mVar];
  end.

