{ %cpu=i386}

{$ifdef fpc}
{$mode delphi}
{$endif fpc}

function MakeProcInstanceData(M: TMethod): Pointer;
begin
    asm
      MOV EAX, M.Data
      MOV @RESULT, EAX
    end;
end;

function MakeProcInstanceCode(M: TMethod): Pointer;
begin
    asm
      MOV EAX, M.Code
      MOV @RESULT, EAX
    end;
end;

function MakeProcInstanceDataAsm(M: TMethod): Pointer; assembler;
asm
  MOV EAX, M.Data
  MOV @RESULT, EAX
end;

function MakeProcInstanceCodeAsm(M: TMethod): Pointer; assembler;
asm
  MOV EAX, M.Code
  MOV @RESULT, EAX
end;

var
  m: tmethod;
begin
  m.code:=pointer(1);
  m.data:=pointer(2);
  if MAkeProcInstanceData(M)<>pointer(2) then
    halt(1);
  m.code:=pointer(1);
  m.data:=pointer(2);
  if MAkeProcInstanceCode(M)<>pointer(1) then
    halt(2);
  m.code:=pointer(1);
  m.data:=pointer(2);
  if MAkeProcInstanceDataAsm(M)<>pointer(2) then
    halt(3);
  m.code:=pointer(1);
  m.data:=pointer(2);
  if MAkeProcInstanceCodeAsm(M)<>pointer(1) then
    halt(4);
end.
