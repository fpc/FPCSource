Program Example43;

{ This program demonstrates the FindFirst function }

Uses sysutils;

Var Info : TSearchRec;
    Count : Longint;

Begin
  Count:=0;
  If FindFirst ('*.pp',faAnyFile,Info)=0 then
    begin
    Repeat
      Inc(Count);
      With Info do 
        Writeln (Name:40,Size:15);
    Until FindNext(info)<>0;
    end;
  FindClose(Info);
  Writeln ('Finished search. Found ',Count,' matches');
  
End.