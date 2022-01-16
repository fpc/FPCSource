{ %cpu=i386 }
{ %norun }

program asm_test004;
{$APPTYPE CONSOLE}
{$OPTIMIZATION SIZE}
{$OPTIMIZATION STACKFRAME}
{$OPTIMIZATION REGVAR}
{$CODEALIGN VARMIN=1}
{$CODEALIGN VARMAX=1}
{$CODEALIGN CONSTMIN=1}
{$CODEALIGN CONSTMAX=1}
{$mode delphi}
{$asmmode intel}



const
wmoven = 1;
            //  eax edx ecx  stack
procedure test1(a , b , c,   flags :longint); register; //__ pbby in ver 3.0.0 the "flags" is reserved word _   with ver 2.6.4 no compilation error
var
f       :boolean;
asm
   mov a,0
   mov b,0
   mov c,0
   test [flags] , wmoven
   setnz f
end;

var
i       :cardinal;

begin
 test1(i,i,i,i);
end.
