unit FPCodCmp; { CodeComplete }

interface

uses Objects,
     WUtils;

const CodeCompleteWords : PTextCollection = nil;

function FPCompleteCodeWord(const WordS: string; var Text: string): boolean;

procedure InitCodeComplete;
procedure DoneCodeComplete;

implementation

uses WEditor,
     FPViews;

function FPCompleteCodeWord(const WordS: string; var Text: string): boolean;
var OK: boolean;
    Index: sw_integer;
begin
  OK:=Assigned(CodeCompleteWords);
  if OK then
  begin
    Text:=CodeCompleteWords^.Lookup(WordS,Index);
    OK:=(Index<>-1) and (length(Text)<>length(WordS));
  end;
  if OK=false then Text:='';
  FPCompleteCodeWord:=OK;
end;

procedure InitCodeComplete;
var I:integer;
    S: string;
begin
  if Assigned(CodeCompleteWords) then Exit;

  New(CodeCompleteWords, Init(10,10));
  for I:=0 to GetReservedWordCount-1 do
    begin
      S:=LowCaseStr(GetReservedWord(I));
      if length(S)>=CodeCompleteMinLen then
        CodeCompleteWords^.Insert(NewStr(S));
    end;
  {
    there should be also a user front-end for customizing CodeComplete !
     any volunteers to implement? ;) - Gabor
  }
end;

procedure DoneCodeComplete;
begin
  if Assigned(CodeCompleteWords) then Dispose(CodeCompleteWords, Done);
  CodeCompleteWords:=nil;
end;

END.
