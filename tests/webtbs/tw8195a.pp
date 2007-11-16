{ %cpu=i386 }

{$APPTYPE CONSOLE}

{$ifdef fpc}
  {$mode delphi}
{$endif}

program AsmTest;

type
  TMyObject = class(TObject)
    Field1 : Integer;
    Field2 : Integer;
    procedure VirtualMethod1; virtual;
    procedure VirtualMethod2; virtual;
  end;

  TMyRecord = record
    EAX : Integer;
    EBX : Integer;
    ECX : Integer;
    EDX : Integer;
  end;

{ TMyObject }

procedure TMyObject.VirtualMethod1;
begin

end;

procedure TMyObject.VirtualMethod2;
begin

end;

function VirtualMethodVMTOFFSET1: Integer;
asm
  mov eax, VMTOFFSET TMyObject.VirtualMethod1;
end;

function VirtualMethodVMTOFFSET2: Integer;
asm
  mov eax, VMTOFFSET TMyObject.VirtualMethod2;
end;

function IUnknownAddRefVMTOFFSET1: Integer;
asm
  mov eax, VMTOFFSET IUnknown._AddRef;
end;

function Field1: Integer;
asm
  mov eax, TMyObject.Field1;
end;

function Field1OFFSET: Integer;
asm
  mov eax, OFFSET TMyObject.Field1;
end;

var
  _Test: Integer;

function Test: Integer;
asm
  mov eax, _Test;
end;

function TestOFFSET: Integer;
asm
  mov eax, OFFSET _Test;
end;

function LabelOFFSET: Integer;
asm
  mov eax, OFFSET @@Exit
 @@Exit:
end;

function TMyObjectTYPE: Integer;
asm
  mov eax, TYPE TMyObject
end;

function TMyRecordTYPE: Integer;
asm
  mov eax, TYPE TMyRecord
end;

function FillMyRecord: TMyRecord;
asm
  mov [eax + TMyRecord.&eax], eax
  mov [eax + TMyRecord.&ebx], ebx
  mov [eax + TMyRecord.&ecx], ecx
  mov [eax + TMyRecord.&edx], edx
end;

var
  MyRecord : TMyRecord;

begin
  _Test := 123;

  WriteLn('VirtualMethodVMTOFFSET1: ', VirtualMethodVMTOFFSET1);
  WriteLn('VirtualMethodVMTOFFSET2: ', VirtualMethodVMTOFFSET2);
  WriteLn('IUnknownAddRefVMTOFFSET1: ', IUnknownAddRefVMTOFFSET1);
  WriteLn('Field1: ', Field1);
  WriteLn('Field1OFFSET: ', Field1OFFSET);
  WriteLn('Test: ', Test);
  WriteLn('TestOFFSET: ', TestOFFSET);
  WriteLn('LabelOFFSET: ', LabelOFFSET);
  WriteLn('TMyObjectTYPE: ', TMyObjectTYPE);
  WriteLn('TMyRecordTYPE: ', TMyRecordTYPE);

  MyRecord.eax := 0;
  MyRecord.ebx := 0;
  MyRecord.ecx := 0;
  MyRecord.edx := 0;

  MyRecord := FillMyRecord;

  WriteLn('MyRecord.eax: ', MyRecord.eax);
  WriteLn('MyRecord.ebx: ', MyRecord.ebx);
  WriteLn('MyRecord.ecx: ', MyRecord.ecx);
  WriteLn('MyRecord.edx: ', MyRecord.edx);
end.
