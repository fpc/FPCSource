Program Example43;

{ This program demonstrates the FindFirst function }

Uses SysUtils;

Var Info : TSearchRec;
    Count : Longint;

Begin
  Count:=0;
  If FindFirst ('*',faAnyFile and faDirectory,Info)=0 then
    begin
    Repeat
      Inc(Count);
      With Info do
        begin
        If (Attr and faDirectory) = faDirectory then
          Write('Dir : ');
        Writeln (Name:40,Size:15);
        end;
    Until FindNext(info)<>0;
    end;
  FindClose(Info);
  Writeln ('Finished search. Found ',Count,' matches');

End.