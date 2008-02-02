program testcont;

{$mode objfpc}

uses contnrs;

Type
  TTestObject = Class(TObject)
    FID : Integer;
    Fmsg : String;
    Constructor Create (ID : INteger; Msg : String);
    Procedure Print;
  end;

Constructor TTestObject.Create (ID : INteger; Msg : String);

begin
  FID:=ID;
  FMsg:=Msg;
end;

procedure TTestObject.Print;

begin
  Writeln(FID,' (',FMsg,')');
end;

Var
   S : TOrderedList;
   O : TTEstObject;
   I : Integer;
begin
//   S:=TObjectStack.Create;
   S:=TObjectQueue.Create;
   For I:=1 to 10 do
     S.Push(TTestObject.Create(I,''));
   Writeln('Popping list');
   Repeat
     O:=TTestObject(S.Pop);
     If O<>Nil then
       begin
       Write('Popped : ');
       O.Print;
       Write (S.Count,' elements left. ');
       O:=TTestObject(S.Peek);
       If (O<>NIl) then
         begin
         Write('Next element is : ');
         O.Print;
         end
       else
         Writeln;
       end;
   Until (O=Nil);
   Writeln('Popped stack');
end.
