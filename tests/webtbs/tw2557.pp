{ Source provided for Free Pascal Bug Report 2557 }
{ Submitted by "Sergey Kosarevsky" on  2003-07-01 }
{ e-mail: netsurfer@au.ru }
Unit tw2557;

{$mode objfpc}

Interface

Type tVMThread=Class;

Type tInstructionProcessor=Procedure(Thread:tVMThread) Of Object;

Const MAX_OPCODES=1;

Type tVMThread=Class       Private
        Class Procedure Opcode_HLT(Thread:tVMThread);
     End;

Const MappingTable:Array[1..MAX_OPCODES] Of tInstructionProcessor=
       (@tVMThread.Opcode_HLT);

Begin
End.
