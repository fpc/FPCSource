{
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998 by Berczi Gabor

    Code Complete routines

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


unit FPCodCmp; { CodeComplete }

interface

uses Objects,Drivers,Dialogs,
     WEditor,WUtils,WViews;

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
      RB : PRadioButtons;
      CB : PCheckBoxes;
      MinInputL,InputL : PEditorInputLine;
      procedure Add;
      procedure Edit;
      procedure Delete;
    end;

function FPCompleteCodeWord(const WordS: string; var Text: string): boolean;

procedure InitCodeComplete;
function  LoadCodeComplete(var S: TStream): boolean;
procedure AddStandardUnitsToCodeComplete;
procedure AddAvailableUnitsToCodeComplete(OnlyStandard : boolean);
function  StoreCodeComplete(var S: TStream): boolean;
procedure DoneCodeComplete;

const CodeCompleteWords : PCodeCompleteWordList = nil;
type
      TCodeCompleteCase = (ccc_unchanged, ccc_lower, ccc_upper, ccc_mixed);
const
     CodeCompleteCase : TCodeCompleteCase = ccc_unchanged;
     UnitsCodeCompleteWords : PCodeCompleteWordList = nil;

procedure RegisterCodeComplete;

implementation

uses App,Views,MsgBox,Validate,
     FVConsts,
     systems, BrowCol,
     FPSwitch, FPCompil,
     FPVars, FPSymbol,
     FPConst,FPViews;

{$ifndef NOOBJREG}
const
  RCodeCompleteWordList: TStreamRec = (
     ObjType: 14401;
     VmtLink: Ofs(TypeOf(TCodeCompleteWordList)^);
     Load:    @TCodeCompleteWordList.Load;
     Store:   @TCodeCompleteWordList.Store
  );
{$endif}

{$ifdef useresstrings}
resourcestring
{$else}
const
{$endif}
      { CodeComplete dialog }
      dialog_codecomplete     = 'CodeComplete';
      label_codecomplete_keywords = '~K~eywords';

      dialog_codecomplete_add = 'Add new keyword';
      label_codecomplete_add_keyword = 'Keyword';

      dialog_codecomplete_edit = 'Edit keyword';
      label_codecomplete_edit_keyword = 'Keyword';

      msg_codecomplete_alreadyinlist = '"%s" is already in the list';

      { standard button texts }
      button_OK          = 'O~K~';
      button_Cancel      = 'Cancel';
      button_New         = '~N~ew';
      button_Edit        = '~E~dit';
      button_Delete      = '~D~elete';

function FPCompleteCodeWord(const WordS: string; var Text: string): boolean;
var OK: boolean;
    CIndex, Index, i : sw_integer;
    St, UpWordS : string;
begin
  if ShowOnlyUnique then
    UpWordS:=UpCaseStr(WordS);
  OK:=Assigned(CodeCompleteWords);
  if OK then
  begin
    Text:=CodeCompleteWords^.Lookup(WordS,CIndex);
    OK:=(CIndex<>-1) and (length(Text)<>length(WordS));
    Index:=-1;
    if OK and ShowOnlyUnique and (CIndex<CodeCompleteWords^.Count-1) then
      begin
        St:=PString(CodeCompleteWords^.At(CIndex+1))^;
        if (UpCaseStr(Copy(St,1,length(WordS)))=UpWordS) then
          begin
            {if UpCase(st[Length(UpWordS)+1])<>Upcase(Text[Length(UpWordS)+1]) then}
              begin
                Text:='';
                FPCompleteCodeWord:=false;
                exit;
            (*  end
            else
              { only give the common part }
              begin
                i:=Length(UpWordS)+1;
                while (i<=length(st)) and (i<=length(text)) and (UpCase(st[i])=Upcase(Text[i])) do
                  inc(i);
                SetLength(Text,i-1);    *)
              end;
          end;
      end;
  end;
  if (ShowOnlyUnique or not OK) and Assigned(UnitsCodeCompleteWords) then
  begin
    Text:=UnitsCodeCompleteWords^.Lookup(WordS,Index);
    OK:=(Index<>-1) and (length(Text)<>length(WordS));
    if ShowOnlyUnique and (Index<UnitsCodeCompleteWords^.Count-1) then
      begin
        St:=PString(UnitsCodeCompleteWords^.At(Index+1))^;
        if UpCaseStr(Copy(St,1,length(WordS)))=UpWordS then
          begin
            {if UpCase(st[Length(UpWordS)+1])<>Upcase(Text[Length(UpWordS)+1]) then}
              begin
                Text:='';
                FPCompleteCodeWord:=false;
                exit;
            (*  end
            else
              { only give the common part }
              begin
                i:=Length(UpWordS)+1;
                while (i<=length(st)) and (i<=length(text)) and (UpCase(st[i])=Upcase(Text[i])) do
                  inc(i);
                SetLength(Text,i-1); *)
              end;
          end;
      end;
  end;

  if ShowOnlyUnique and (Index<>-1) and (CIndex<>-1) then
    begin
      {St:=PString(CodeCompleteWords^.At(CIndex+1))^;
       Was wrong, CIndex+1 could be above count => collection.error
       generated RTE 213
      if UpCase(st[Length(UpWordS)+1])<>Upcase(Text[Length(UpWordS)+1]) then}
        begin
          Text:='';
          FPCompleteCodeWord:=false;
          exit;
      (*  end
      else
        { only give the common part }
        begin
          i:=Length(UpWordS)+1;
          while (i<=length(st)) and (i<=length(text)) and (UpCase(st[i])=Upcase(Text[i])) do
            inc(i);
          SetLength(Text,i-1); *)
        end;
    end;
  if OK=false then Text:=''
  else case CodeCompleteCase of
    ccc_upper : Text:=UpcaseStr(Text);
    ccc_lower : Text:=LowcaseStr(Text);
    ccc_mixed : Text:=UpCase(Text[1])+LowCaseStr(Copy(Text,2,High(Text)));
  end;
  FPCompleteCodeWord:=OK;
end;

procedure InitCodeComplete;
var I:integer;
    S: string;
begin
  if Assigned(CodeCompleteWords) then
    Dispose(CodeCompleteWords, Done);
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


procedure AddAvailableUnitsToCodeComplete(OnlyStandard : boolean);

var
  I : sw_integer;
  Overflow: boolean;
  Level : longint;
  UpStandardUnits : string;

  procedure InsertInS(P: PSymbol);

    procedure InsertItemsInS(P: PSymbolCollection);
    var I: Sw_integer;
    begin
      for I:=0 to P^.Count-1 do
        InsertInS(P^.At(I));
    end;
  Var
    st : string;
    CIndex : sw_integer;
  begin
    Inc(level);
    if UnitsCodeCompleteWords^.Count=MaxCollectionSize then
       begin Overflow:=true; Exit; end;
    st:=P^.GetName;
    if Length(st)>=CodeCompleteMinLen then
      if not ((level=1) and OnlyStandard and (st=UpCaseStr(CodeCompleteUnitName))) then
        begin
          st:=Lowcasestr(st);
          UnitsCodeCompleteWords^.LookUp(st,CIndex);
          if CIndex<>-1 then
          UnitsCodeCompleteWords^.Insert(NewStr(st));
        end;
    { this is wrong because it inserted args or locals of proc
      in the globals list !! PM}
    if (P^.Items<>nil) and (level=1) and
        ((not OnlyStandard or (Pos(P^.GetName+',',UpStandardUnits)>0) or
        { don't exclude system unit ... }
        (Pos('SYS',P^.GetName)>0))) then
      InsertItemsInS(P^.Items);
    Dec(level);
  end;

begin
  if OnlyStandard then
    UpStandardunits:=UpCaseStr(StandardUnits)+',';
  if IsSymbolInfoAvailable then
    begin
      if Assigned(UnitsCodeCompleteWords) then
        begin
          Dispose(UnitsCodeCompleteWords,done);
          UnitsCodeCompleteWords:=nil;
        end;

      New(UnitsCodeCompleteWords, Init(10,10));
      level:=0;
      Overflow:=false;
      BrowCol.Modules^.ForEach(@InsertInS);
      { if Overflow then
        WarningBox(msg_toomanysymbolscantdisplayall,nil); }
    end;
end;

procedure AddStandardUnitsToCodeComplete;
var
  HiddenSource : PSourceWindow;
  R : TRect;
  StoreBrowserSwitchesConfig : string;
begin
  Desktop^.GetExtent(R);
  New(HiddenSource,init(R,'*'));
  HiddenSource^.NoNameCount:=0;
  HiddenSource^.UpdateTitle;
  HiddenSource^.Hide;
  CompilingHiddenFile:=HiddenSource;
  { compile a dummy file to get symbol info }
  with HiddenSource^.Editor^ do
    begin
      FileName:=CodeCompleteUnitName+'.pp';
      Addline('unit '+CodeCompleteUnitName+';');
      Addline('interface');
      if StandardUnits<>'' then
        begin
          AddLine('uses');
          Addline(StandardUnits);
          Addline('  ;');
        end;
      Addline('implementation');
      Addline('end.');
      SetModified(true);
      // SaveFile;
    end;
  StoreBrowserSwitchesConfig:=BrowserSwitches^.GetCurrSelParam;
  BrowserSwitches^.ReadItemsCfg('+');
  DoCompile(cCompile);
  BrowserSwitches^.SetCurrSelParam(StoreBrowserSwitchesConfig);
  AddAvailableUnitsToCodeComplete(true);
  { Now add the interface declarations to the Code Complete list }
  CompilingHiddenFile:=nil;
  Dispose(HiddenSource,Done);
end;

function LoadCodeComplete(var S: TStream): boolean;
var C: PCodeCompleteWordList;
    OK: boolean;
    NewCodeCompleteMinLen : byte;
    NewUseStandardUnitsInCodeComplete,
    NewUseAllUnitsInCodeComplete,
    NewShowOnlyUnique : boolean;
    NewCodeCompleteCase : TCodeCompleteCase;
    StPtr : PString;
begin
  New(C, Load(S));
  OK:=Assigned(C) and (S.Status=stOk);
  if OK then
    begin
      if Assigned(CodeCompleteWords) then Dispose(CodeCompleteWords, Done);
      CodeCompleteWords:=C;
      S.Read(NewCodeCompleteCase,Sizeof(TCodeCompleteCase));
      OK:=(S.Status=stOk);
      if OK then
        CodeCompleteCase:=NewCodeCompleteCase;
      { Old version of Code complete, also OK PM }
      if not OK or (S.getPos=S.getSize) then
        begin
          LoadCodeComplete:=OK;
          exit;
        end;

      if S.Status=stOK then
        S.Read(NewUseStandardUnitsInCodeComplete,Sizeof(UseStandardUnitsInCodeComplete));
      if S.Status=stOK then
        UseStandardUnitsInCodeComplete:=NewUseStandardUnitsInCodeComplete;
      if S.Status=stOK then
        S.Read(NewUseAllUnitsInCodeComplete,Sizeof(UseAllUnitsInCodeComplete));
      if S.Status=stOK then
        UseAllUnitsInCodeComplete:=NewUseAllUnitsInCodeComplete;
      if S.Status=stOK then
        S.Read(NewShowOnlyUnique,Sizeof(ShowOnlyUnique));
      if S.Status=stOK then
        ShowOnlyUnique:=NewShowOnlyUnique;
      if S.Status=stOK then
        S.Read(NewCodeCompleteMinLen,Sizeof(CodeCompleteMinLen));
      if S.Status=stOK then
        CodeCompleteMinLen:=NewCodeCompleteMinLen;
      if S.Status=stOK then
        StPtr:=S.ReadStr
      else
        StPtr:=nil;
      if (S.Status=stOK) then
        StandardUnits:=GetStr(StPtr);
      if assigned(StPtr) then
        FreeMem(StPtr,Length(StandardUnits)+1);
      OK:=S.Status=stOK;
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
    S.Write(CodeCompleteCase,Sizeof(TCodeCompleteCase));
    { New fields added }
    S.Write(UseStandardUnitsInCodeComplete,Sizeof(UseStandardUnitsInCodeComplete));
    S.Write(UseAllUnitsInCodeComplete,Sizeof(UseAllUnitsInCodeComplete));
    S.Write(ShowOnlyUnique,Sizeof(ShowOnlyUnique));
    S.Write(CodeCompleteMinLen,Sizeof(CodeCompleteMinLen));
    S.WriteStr(@StandardUnits);
    OK:=OK and (S.Status=stOK);
  end;
  StoreCodeComplete:=OK;
end;

procedure DoneCodeComplete;
begin
  if Assigned(CodeCompleteWords) then
    begin
      Dispose(CodeCompleteWords, Done);
      CodeCompleteWords:=nil;
    end;
  if Assigned(UnitsCodeCompleteWords) then
    begin
      Dispose(UnitsCodeCompleteWords,done);
      UnitsCodeCompleteWords:=nil;
    end;
end;

constructor TCodeCompleteDialog.Init;
var R,R2,R3: TRect;
    Items: PSItem;
    SB: PScrollBar;
begin
  R.Assign(0,0,50,22);
  inherited Init(R,dialog_codecomplete);
  HelpCtx:=hcCodeCompleteOptions;

  { name list dialog }
  GetExtent(R); R.Grow(-3,-2); Inc(R.A.Y); R3.Copy(R); Dec(R.B.X,12);
  Dec(R.B.Y,7);
  R2.Copy(R); R2.Move(1,0); R2.A.X:=R2.B.X-1;
  New(SB, Init(R2)); Insert(SB);
  New(CodeCompleteLB, Init(R,1,SB));
  Insert(CodeCompleteLB);
  R2.Copy(R); R2.Move(0,-1); R2.B.Y:=R2.A.Y+1; Dec(R2.A.X);
  Insert(New(PLabel, Init(R2, label_codecomplete_keywords, CodeCompleteLB)));

  { Case choice }
  R.Copy(R3); Dec(R.B.Y,2); R.A.Y:=R.B.Y-4; Inc(R.A.X); R.B.X:=R.A.X+15;
  Items:=NewSItem('Unc~h~anged',
           NewSItem('~L~ower',
           NewSItem('~U~pper',
           NewSItem('~M~ixed',nil))));
  RB:=New(PRadioButtons,Init(R,Items));
  RB^.SetData(ord(CodeCompleteCase));
  R2.Copy(R); R2.Move(0,-1); R2.B.Y:=R2.A.Y+1; Dec(R2.A.X);
  Insert(New(PLabel, Init(R2, 'Case handling', RB)));
  Insert(RB);

  { Mininum length inputline }
  R.Copy(R3); R.A.Y:=R.B.Y-7;R.B.Y:=R.A.Y+1; Dec(R.B.X); R.A.X:=R.B.X -5;
  New(MinInputL, Init(R,5));
  MinInputL^.SetValidator(New(PRangeValidator, Init(1,255)));
  Insert(MinInputL);
  R2.Copy(R); R2.A.X:=20;Dec(R2.B.X,5);
  Insert(New(PLabel, Init(R2, 'Min. length', MinInputL)));

  { Standard/all units booleans }
  Items:=nil;
  Items:=NewSItem('Add standard units', Items);
  Items:=NewSItem('Add all units', Items);
  Items:=NewSItem('Show only unique', Items);
  R.Copy(R3); R.A.Y:=R.B.Y-5;R.B.Y:=R.A.Y+3; Inc(R.A.X,18); Dec(R.B.X);
  New(CB, Init(R, Items));
  Insert(CB);
  R2.Copy(R); R2.Move(0,-1); R2.B.Y:=R2.A.Y+1; Dec(R2.A.X);
  Insert(New(PLabel, Init(R2, 'Unit handling', CB)));
  R2.Copy(R); R2.Move(0,-1); R2.B.Y:=R2.A.Y+1;
  If ShowOnlyUnique then
    CB^.Press(0);
  If UseAllUnitsInCodeComplete then
    CB^.Press(1);
  If UseStandardUnitsInCodeComplete then
    CB^.Press(2);

  { Standard unit name boolean }
  R.Copy(R3); R.A.Y:=R.B.Y-1; Inc(R.A.X); Dec(R.B.X);
  New(InputL,Init(R,255));
  Insert(InputL);
  InputL^.SetValidator(New(PFilterValidator,Init(NumberChars+AlphaChars+[','])));
  R2.Copy(R); R2.Move(0,-1); R2.B.Y:=R2.A.Y+1; Dec(R2.A.X);R2.B.X:=R2.A.X+25;
  Insert(New(PLabel, Init(R2, '~S~tandard unit list', InputL)));

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
    NewVal, I: integer;
    NewValStr : string;
begin
  New(C, Init(10,20));
  if Assigned(CodeCompleteWords) then
  for I:=0 to CodeCompleteWords^.Count-1 do
    C^.Insert(NewStr(GetStr(CodeCompleteWords^.At(I))));
  CodeCompleteLB^.NewList(C);
  InputL^.SetData(StandardUnits);
  NewValStr:=IntToStr(CodeCompleteMinLen);
  MinInputL^.SetData(NewValStr);
  R:=inherited Execute;
  if R=cmOK then
    begin
      if Assigned(CodeCompleteWords) then Dispose(CodeCompleteWords, Done);
      CodeCompleteWords:=C;
      CodeCompleteCase:=TCodeCompleteCase(RB^.Value);
      MinInputL^.GetData(NewValStr);
      NewVal:=StrToInt(NewValStr);
      if (NewVal>0) and (NewVal<>CodeCompleteMinLen) then
        begin
          CodeCompleteMinLen:=NewVal;
          InitCodeComplete;
        end;
      ShowOnlyUnique:=CB^.Mark(0);
      UseAllUnitsInCodeComplete:=CB^.Mark(1);
      UseStandardUnitsInCodeComplete:=CB^.Mark(2);
      if UseStandardUnitsInCodeComplete and (not UseAllUnitsInCodeComplete or not assigned(UnitsCodeCompleteWords)) and
         ((StandardUnits<>GetStr(InputL^.Data)) or not assigned(UnitsCodeCompleteWords)) then
        begin
          InputL^.GetData(StandardUnits);
          AddStandardUnitsToCodeComplete;
        end
      else
        InputL^.GetData(StandardUnits);
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
