{ %CPU=x86_64 }
{ Tests that object fields with zero offset are handled by Intel assembler reader
  the same way as fields with nonzero offset }
{$ifdef fpc}
{$mode delphi}
{$asmmode intel}
{$PIC OFF}
{$endif}

type
  TTest=object
    Data1,Data2:Integer;
    procedure Setter;
    function Getter1: TTest;
    function Getter2: TTest;
  end;


 procedure TTest.Setter; assembler;
 asm
   mov Self.Data1,1234
   mov Self.Data2,5678
 end;
 
 function TTest.Getter1: TTest; assembler;
 asm
   mov eax,Self.Data1
   mov result.Data1, eax
   mov eax,Self.Data2
   mov result.Data2, eax
 end;
 
 
 function TTest.Getter2: TTest; assembler;
 asm
   mov  eax,Self.Data1
   mov  @result.Data1, eax
   mov  eax,Self.Data2
   mov  @result.Data2, eax
 end;
 
 var
   testobj, testobj2: TTest;
 
 begin
   testobj.Setter;
   testobj2:=testobj.Getter1;
   if (testobj2.Data1 <> 1234) or (testobj2.Data2 <> 5678) then
     Halt(1);
   testobj2:=testobj.Getter2;
   if (testobj2.Data1 <> 1234) or (testobj2.Data2 <> 5678) then
     Halt(2);
   writeln('ok');
 end.
 
