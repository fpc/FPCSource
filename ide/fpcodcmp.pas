unit FPCodCmp; { CodeComplete }

interface

uses Objects,Drivers,
     WUtils,WViews;

type
     PCodeCompleteWordList = ^TCodeCompleteWordList;
     TCodeCompleteWordList = object(TTextCollection)
     end;

    PCodeCompleteDialog = ^TCodeCompleteDialog;
    TCodeCompleteDialog = object(TCenterDialog)
      constructor Init;
      function    Execute: Word; virtual;
      procedure   HandleEvent(var Event: TEvent); virtual;
    private
      CodeCompleteLB : PAdvancedListBox;
      procedure Add;
      procedure Edit;
      procedure Delete;
    end;

function FPCompleteCodeWord(const WordS: string; var Text: string): boolean;

procedure InitCodeComplete;
function  LoadCodeComplete(var S: TStream): boolean;
function  StoreCodeComplete(var S: TStream): boolean;
procedure DoneCodeComplete;

const CodeCompleteWords : PCodeCompleteWordList = nil;

procedure RegisterCodeComplete;

implementation

uses Views,Dialogs,MsgBox,
{$ifdef FVISION}
     FVConsts,
{$else}
     Commands,
{$endif}
     WEditor,
     FPConst,FPString,FPViews;

{$ifndef NOOBJREG}
const
  RCodeCompleteWordList: TStreamRec = (
     ObjType: 14401;
     VmtLink: Ofs(TypeOf(TCodeCompleteWordList)^);
     Load:    @TCodeCompleteWordList.Load;
     Store:   @TCodeCompleteWordList.Store
  );
{$endif}

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

function LoadCodeComplete(var S: TStream): boolean;
var C: PCodeCompleteWordList;
    OK: boolean;
begin
  New(C, Load(S));
  OK:=Assigned(C) and (S.Status=stOk);
  if OK then
    begin
      if Assigned(CodeCompleteWords) then Dispose(CodeCompleteWords, Done);
      CodeCompleteWords:=C;
    end
  else
    if Assigned(C) then
      Dispose(C, Done);
  LoadCodeComplete:=OK;
end;

function StoreCodeComplete(var S: TStream): boolean;
var OK: boolean;
begin
  OK:=Assigned(CodeCompleteWords);
  if OK then
  begin
    CodeCompleteWords^.Store(S);
    OK:=OK and (S.Status=stOK);
  end;
  StoreCodeComplete:=OK;
end;

procedure DoneCodeComplete;
begin
  if Assigned(CodeCompleteWords) then Dispose(CodeCompleteWords, Done);
  CodeCompleteWords:=nil;
end;

constructor TCodeCompleteDialog.Init;
var R,R2,R3: TRect;
    SB: PScrollBar;
begin
  R.Assign(0,0,46,16);
  inherited Init(R,dialog_codecomplete);
  HelpCtx:=hcCodeCompleteOptions;
  GetExtent(R); R.Grow(-3,-2); Inc(R.A.Y); R3.Copy(R); Dec(R.B.X,12);
  R2.Copy(R); R2.Move(1,0); R2.A.X:=R2.B.X-1;
  New(SB, Init(R2)); Insert(SB);
  New(CodeCompleteLB, Init(R,1,SB));
  Insert(CodeCompleteLB);
  R2.Copy(R); R2.Move(0,-1); R2.B.Y:=R2.A.Y+1; Dec(R2.A.X);
  Insert(New(PLabel, Init(R2, label_codecomplete_keywords, CodeCompleteLB)));

  R.Copy(R3); R.A.X:=R.B.X-10; R.B.Y:=R.A.Y+2;
  Insert(New(PButton, Init(R, button_OK, cmOK, bfNormal)));
  R.Move(0,2);
  Insert(New(PButton, Init(R, button_Edit, cmEditItem, bfDefault)));
  R.Move(0,2);
  Insert(New(PButton, Init(R, button_New, cmAddItem, bfNormal)));
  R.Move(0,2);
  Insert(New(PButton, Init(R, button_Delete, cmDeleteItem, bfNormal)));
  R.Move(0,2);
  Insert(New(PButton, Init(R, button_Cancel, cmCancel, bfNormal)));
  SelectNext(false);
end;

procedure TCodeCompleteDialog.HandleEvent(var Event: TEvent);
var DontClear: boolean;
begin
  case Event.What of
    evKeyDown :
      begin
        DontClear:=false;
        case Event.KeyCode of
          kbIns  :
            Message(@Self,evCommand,cmAddItem,nil);
          kbDel  :
            Message(@Self,evCommand,cmDeleteItem,nil);
        else DontClear:=true;
        end;
        if DontClear=false then ClearEvent(Event);
      end;
    evBroadcast :
      case Event.Command of
        cmListItemSelected :
          if Event.InfoPtr=pointer(CodeCompleteLB) then
            Message(@Self,evCommand,cmEditItem,nil);
      end;
    evCommand :
      begin
        DontClear:=false;
        case Event.Command of
          cmAddItem    : Add;
          cmDeleteItem : Delete;
          cmEditItem   : Edit;
        else DontClear:=true;
        end;
        if DontClear=false then ClearEvent(Event);
      end;
  end;
  inherited HandleEvent(Event);
end;

function TCodeCompleteDialog.Execute: Word;
var R: word;
    C: PCodeCompleteWordList;
    I: integer;
begin
  New(C, Init(10,20));
  if Assigned(CodeCompleteWords) then
  for I:=0 to CodeCompleteWords^.Count-1 do
    C^.Insert(NewStr(GetStr(CodeCompleteWords^.At(I))));
  CodeCompleteLB^.NewList(C);
  R:=inherited Execute;
  if R=cmOK then
    begin
      if Assigned(CodeCompleteWords) then Dispose(CodeCompleteWords, Done);
      CodeCompleteWords:=C;
    end
  else
    Dispose(C, Done);
  Execute:=R;
end;

procedure TCodeCompleteDialog.Add;
var IC: boolean;
    S: string;
    P: PString;
    Cmd: word;
    CanExit: boolean;
    I: sw_integer;
begin
  IC:=CodeCompleteLB^.Range=0;
  if IC=false then
    S:=GetStr(CodeCompleteLB^.List^.At(CodeCompleteLB^.Focused))
  else
    S:='';

  repeat
    Cmd:=InputBox(dialog_codecomplete_add,label_codecomplete_add_keyword,S,255);
    CanExit:=Cmd<>cmOK;
    if CanExit=false then
      begin
        CanExit:=PCodeCompleteWordList(CodeCompleteLB^.List)^.Search(@S,I)=false;
        if CanExit=false then
        begin
          ClearFormatParams; AddFormatParamStr(S);
          ErrorBox(msg_codecomplete_alreadyinlist,@FormatParams);
        end;
      end;
  until CanExit;

  if Cmd=cmOK then
    begin
      P:=NewStr(S);
      with CodeCompleteLB^ do
      begin
        List^.Insert(P);
        SetRange(List^.Count);
        SetFocusedItem(P);
      end;
      ReDraw;
    end;
end;

procedure TCodeCompleteDialog.Edit;
var S: string;
    I,T: sw_integer;
    Cmd: word;
    CanExit: boolean;
    P: PString;
begin
  if CodeCompleteLB^.Range=0 then Exit;
  I:=CodeCompleteLB^.Focused;
  S:=GetStr(CodeCompleteLB^.List^.At(I));
  repeat
    Cmd:=InputBox(dialog_codecomplete_edit,label_codecomplete_edit_keyword,S,255);
    CanExit:=Cmd<>cmOK;
    if CanExit=false then
      begin
        CanExit:=PCodeCompleteWordList(CodeCompleteLB^.List)^.Search(@S,T)=false;
        CanExit:=CanExit or (T=I);
        if CanExit=false then
        begin
          ClearFormatParams; AddFormatParamStr(S);
          ErrorBox(msg_codecomplete_alreadyinlist,@FormatParams);
        end;
      end;
  until CanExit;

  if Cmd=cmOK then
    begin
      P:=NewStr(S);
      with CodeCompleteLB^ do
      begin
        List^.AtFree(I);
        List^.Insert(P);
        SetFocusedItem(P);
      end;
      ReDraw;
    end;
end;

procedure TCodeCompleteDialog.Delete;
begin
  if CodeCompleteLB^.Range=0 then Exit;
  CodeCompleteLB^.List^.AtFree(CodeCompleteLB^.Focused);
  CodeCompleteLB^.SetRange(CodeCompleteLB^.List^.Count);
  ReDraw;
end;

procedure RegisterCodeComplete;
begin
{$ifndef NOOBJREG}
  RegisterType(RCodeCompleteWordList);
{$endif}
end;

END.
