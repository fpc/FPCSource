{
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998 by Berczi Gabor

    Code Template routines

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit FPCodTmp; { Code Templates }

{2.0 compatibility}
{$ifdef VER2_0}
  {$macro on}
  {$define resourcestring := const}
{$endif}

interface

uses Objects,Drivers,Dialogs,
     WUtils,WViews,WEditor,
     FPViews;

type
    PCodeTemplate = ^TCodeTemplate;
    TCodeTemplate = object(TObject)
      constructor Init(const AShortCut: string; AText: PUnsortedStringCollection);
      function    GetShortCut: string;
      procedure   GetText(AList: PUnsortedStringCollection);
      procedure   SetShortCut(const AShortCut: string);
      procedure   SetText(AList: PUnsortedStringCollection);
      procedure   GetParams(var AShortCut: string; Lines: PUnsortedStringCollection);
      procedure   SetParams(const AShortCut: string; Lines: PUnsortedStringCollection);
      constructor Load(var S: TStream);
      procedure   Store(var S: TStream);
      destructor  Done; virtual;
    private
      ShortCut: PString;
      Text: PUnsortedStringCollection;
    end;

    PCodeTemplateCollection = ^TCodeTemplateCollection;
    TCodeTemplateCollection = object(TSortedCollection)
      function Compare(Key1, Key2: Pointer): sw_Integer; virtual;
      function SearchByShortCut(const ShortCut: string): PCodeTemplate; virtual;
      function LookUp(const S: string; AcceptMulti: boolean; var Idx: sw_integer): string; virtual;
    end;

    PCodeTemplateListBox = ^TCodeTemplateListBox;
    TCodeTemplateListBox = object(TAdvancedListBox)
      function GetText(Item,MaxLen: Sw_Integer): String; virtual;
    end;

    PCodeTemplateDialog = ^TCodeTemplateDialog;
    TCodeTemplateDialog = object(TCenterDialog)
      constructor Init(const ATitle: string; ATemplate: PCodeTemplate);
      function    Execute: Word; virtual;
    private
      Template   : PCodeTemplate;
      ShortcutIL : PInputLine;
      CodeMemo   : PFPCodeMemo;
    end;

    PCodeTemplatesDialog = ^TCodeTemplatesDialog;
    TCodeTemplatesDialog = object(TCenterDialog)
      SelMode: boolean;
      constructor Init(ASelMode: boolean;const AShortCut : string);
      function    Execute: Word; virtual;
      procedure   HandleEvent(var Event: TEvent); virtual;
      function    GetSelectedShortCut: string;
    private
      CodeTemplatesLB : PCodeTemplateListBox;
      TemplateViewer  : PFPCodeMemo;
      StartIdx : sw_integer;
      procedure Add;
      procedure Edit;
      procedure Delete;
      procedure Update;
    end;

const CodeTemplates : PCodeTemplateCollection = nil;

function FPTranslateCodeTemplate(var Shortcut: string; ALines: PUnsortedStringCollection): boolean;

procedure InitCodeTemplates;
function  LoadCodeTemplates(var S: TStream): boolean;
function  StoreCodeTemplates(var S: TStream): boolean;
procedure DoneCodeTemplates;

procedure RegisterCodeTemplates;

implementation

uses Views,App,Validate,
     FVConsts,
     FPConst;

resourcestring  label_codetemplate_shortcut = '~S~hortcut';
                label_codetemplate_content = '~T~emplate content';
                label_codetemplate_templates = '~T~emplates';
                msg_codetemplate_alreadyinlist = 'A template named "%s" is already in the list';
                dialog_modifytemplate = 'Modify template';
                dialog_newtemplate = 'New template';

                { standard button texts }
                button_OK          = 'O~K~';
                button_Cancel      = 'Cancel';
                button_New         = '~N~ew';
                button_Edit        = '~E~dit';
                button_Delete      = '~D~elete';

{$ifndef NOOBJREG}
const
  RCodeTemplate: TStreamRec = (
     ObjType: 14501;
     VmtLink: Ofs(TypeOf(TCodeTemplate)^);
     Load:    @TCodeTemplate.Load;
     Store:   @TCodeTemplate.Store
  );
  RCodeTemplateCollection: TStreamRec = (
     ObjType: 14502;
     VmtLink: Ofs(TypeOf(TCodeTemplateCollection)^);
     Load:    @TCodeTemplateCollection.Load;
     Store:   @TCodeTemplateCollection.Store
  );
{$endif}

constructor TCodeTemplate.Init(const AShortCut: string; AText: PUnsortedStringCollection);
procedure CopyIt(P: PString); {$ifndef FPC}far;{$endif}
begin
  Text^.Insert(NewStr(GetStr(P)));
end;
begin
  inherited Init;
  ShortCut:=NewStr(AShortCut);
  SetText(AText);
end;

function TCodeTemplate.GetShortCut: string;
begin
  GetShortCut:=GetStr(ShortCut);
end;

procedure TCodeTemplate.GetText(AList: PUnsortedStringCollection);
procedure CopyIt(P: PString); {$ifndef FPC}far;{$endif}
begin
  AList^.Insert(NewStr(GetStr(P)));
end;
begin
  if Assigned(AList) and Assigned(Text) then
    Text^.ForEach(@CopyIt);
end;

procedure TCodeTemplate.SetShortCut(const AShortCut: string);
begin
  if Assigned(ShortCut) then DisposeStr(ShortCut);
  ShortCut:=NewStr(AShortCut);
end;

procedure TCodeTemplate.SetText(AList: PUnsortedStringCollection);
begin
  if Assigned(Text) then Dispose(Text, Done);
  New(Text, CreateFrom(AList));
end;

procedure TCodeTemplate.GetParams(var AShortCut: string; Lines: PUnsortedStringCollection);
begin
  AShortCut:=GetShortCut;
  GetText(Lines);
end;

procedure TCodeTemplate.SetParams(const AShortCut: string; Lines: PUnsortedStringCollection);
begin
  SetShortCut(AShortCut);
  SetText(Lines);
end;

constructor TCodeTemplate.Load(var S: TStream);
begin
  ShortCut:=S.ReadStr;
  New(Text, Load(S));
end;

procedure TCodeTemplate.Store(var S: TStream);
begin
  S.WriteStr(ShortCut);
  Text^.Store(S);
end;

destructor TCodeTemplate.Done;
begin
  if Assigned(ShortCut) then DisposeStr(ShortCut); ShortCut:=nil;
  if Assigned(Text) then Dispose(Text, Done); Text:=nil;
  inherited Done;
end;

function TCodeTemplateCollection.Compare(Key1, Key2: Pointer): sw_Integer;
var K1: PCodeTemplate absolute Key1;
    K2: PCodeTemplate absolute Key2;
    R: Sw_integer;
    S1,S2: string;
begin
  S1:=UpCaseStr(K1^.GetShortCut);
  S2:=UpCaseStr(K2^.GetShortCut);
  if S1<S2 then R:=-1 else
  if S1>S2 then R:=1 else
  R:=0;
  Compare:=R;
end;

function TCodeTemplateCollection.SearchByShortCut(const ShortCut: string): PCodeTemplate;
var T: TCodeTemplate;
    Index: sw_integer;
    P: PCodeTemplate;
begin
  T.Init(ShortCut,nil);
  if Search(@T,Index)=false then P:=nil else
    P:=At(Index);
  T.Done;
  SearchByShortCut:=P;
end;

function TCodeTemplateCollection.LookUp(const S: string; AcceptMulti: boolean; var Idx: sw_integer): string;
var OLI,ORI,Left,Right,Mid: sw_integer;
    MidP: PCodeTemplate;
    MidS: string;
    FoundS: string;
    UpS : string;
begin
  Idx:=-1; FoundS:='';
  Left:=0; Right:=Count-1;
  UpS:=UpCaseStr(S);
  while Left<=Right do
    begin
      OLI:=Left; ORI:=Right;
      Mid:=Left+(Right-Left) div 2;
      MidP:=At(Mid);
      MidS:=UpCaseStr(MidP^.GetShortCut);
      if copy(MidS,1,length(UpS))=UpS then
        begin
          if (Idx<>-1) and (Idx<>Mid) and not AcceptMulti then
            begin
              { several solutions possible, return nothing }
              Idx:=-1;
              FoundS:='';
              break;
            end
          else if Idx=-1 then
            begin
              Idx:=Mid;
              FoundS:=MidP^.GetShortCut;
            end;
        end;
      if UpS<MidS then
        Right:=Mid
      else
        Left:=Mid;
      if (OLI=Left) and (ORI=Right) then
        begin
          if (Left<Right) then
            Left:=Right
          else
            Break;
        end;
    end;
  { check if next also fits...
    return '' in that case }
  if (Idx<>-1) and (Idx<Count-1) and not AcceptMulti then
    begin
      MidP:=At(Idx+1);
      MidS:=UpCaseStr(MidP^.GetShortCut);
      if copy(MidS,1,length(UpS))=UpS then
        begin
          Idx:=-1;
          FoundS:='';
        end;
    end;
  LookUp:=FoundS;
end;


function FPTranslateCodeTemplate(var Shortcut: string; ALines: PUnsortedStringCollection): boolean;
var OK: boolean;
    P: PCodeTemplate;
    CompleteName: String;
    Idx : sw_integer;
begin
  OK:=Assigned(CodeTemplates);
  if OK then
  begin
    P:=CodeTemplates^.SearchByShortCut(ShortCut);
    if not assigned(P) then
      begin
        CompleteName:=CodeTemplates^.Lookup(ShortCut,false,Idx);
        if Idx<>-1 then
          begin
            P:=CodeTemplates^.At(Idx);
            ShortCut:=CompleteName;
          end;
      end;
    OK:=Assigned(P);
    if OK then
      P^.GetText(ALines);
  end;
  FPTranslateCodeTemplate:=OK;
end;

procedure InitCodeTemplates;
begin
  if Assigned(CodeTemplates) then Exit;

  New(CodeTemplates, Init(10,10));
end;

function LoadCodeTemplates(var S: TStream): boolean;
var C: PCodeTemplateCollection;
    OK: boolean;
begin
  New(C, Load(S));
  OK:=Assigned(C) and (S.Status=stOk);
  if OK then
    begin
      if Assigned(CodeTemplates) then Dispose(CodeTemplates, Done);
      CodeTemplates:=C;
    end
  else
    if Assigned(C) then
      Dispose(C, Done);
  LoadCodeTemplates:=OK;
end;

function StoreCodeTemplates(var S: TStream): boolean;
var OK: boolean;
begin
  OK:=Assigned(CodeTemplates);
  if OK then
  begin
    CodeTemplates^.Store(S);
    OK:=OK and (S.Status=stOK);
  end;
  StoreCodeTemplates:=OK;
end;

procedure DoneCodeTemplates;
begin
  if Assigned(CodeTemplates) then Dispose(CodeTemplates, Done);
  CodeTemplates:=nil;
end;

function TCodeTemplateListBox.GetText(Item,MaxLen: Sw_Integer): String;
var P: PCodeTemplate;
begin
  P:=List^.At(Item);
  GetText:=P^.GetShortCut;
end;

constructor TCodeTemplateDialog.Init(const ATitle: string; ATemplate: PCodeTemplate);
var R,R2,R3: TRect;
begin
  R.Assign(0,0,52,15);
  inherited Init(R,ATitle);
  Template:=ATemplate;

  GetExtent(R); R.Grow(-3,-2); R3.Copy(R);
  Inc(R.A.Y); R.B.Y:=R.A.Y+1; R.B.X:=R.A.X+46;
  New(ShortCutIL, Init(R, 128)); Insert(ShortcutIL);
  ShortCutIL^.SetValidator(New(PFilterValidator,Init(NumberChars+AlphaChars)));
  R2.Copy(R); R2.Move(-1,-1);
  Insert(New(PLabel, Init(R2, label_codetemplate_shortcut, ShortcutIL)));
  R.Move(0,3); R.B.Y:=R.A.Y+8;
  New(CodeMemo, Init(R, nil,nil,nil{,4096 does not compile !! }));
  Insert(CodeMemo);
  R2.Copy(R); R2.Move(-1,-1); R2.B.Y:=R2.A.Y+1;
  Insert(New(PLabel, Init(R2, label_codetemplate_content, CodeMemo)));

  InsertButtons(@Self);

  ShortcutIL^.Select;
end;

function TCodeTemplateDialog.Execute: Word;
var R: word;
    S: string;
    L: PUnsortedStringCollection;
begin
  New(L, Init(10,10));
  S:=Template^.GetShortCut;
  Template^.GetText(L);
  ShortcutIL^.SetData(S);
  CodeMemo^.SetContent(L);
  R:=inherited Execute;
  if R=cmOK then
  begin
    L^.FreeAll;
    ShortcutIL^.GetData(S);
    CodeMemo^.GetContent(L);
    Template^.SetShortcut(S);
    Template^.SetText(L);
  end;
  Execute:=R;
end;

constructor TCodeTemplatesDialog.Init(ASelMode: boolean;const AShortCut : string);
function B2I(B: boolean; I1,I2: longint): longint;
begin
  if B then B2I:=I1 else B2I:=I2;
end;
var R,R2,R3: TRect;
    SB: PScrollBar;
begin
  R.Assign(0,0,46,20);
  inherited Init(R,'Code Templates');
  HelpCtx:=hcCodeTemplateOptions;
  SelMode:=ASelMode;
  GetExtent(R); R.Grow(-3,-2); Inc(R.A.Y); R.B.Y:=R.A.Y+10;
  R3.Copy(R); Dec(R.B.X,12);
  R2.Copy(R); R2.Move(1,0); R2.A.X:=R2.B.X-1;
  New(SB, Init(R2)); Insert(SB);
  New(CodeTemplatesLB, Init(R,1,SB));
  Insert(CodeTemplatesLB);
  if AShortCut<>'' then
    begin
      If assigned(CodeTemplates) then
        CodeTemplates^.Lookup(AShortCut,true,StartIdx)
      else
        StartIdx:=-1;
    end
  else
    StartIdx:=-1;
  R2.Copy(R); R2.Move(0,-1); R2.B.Y:=R2.A.Y+1; Dec(R2.A.X);
  Insert(New(PLabel, Init(R2, label_codetemplate_templates, CodeTemplatesLB)));

  GetExtent(R); R.Grow(-2,-2); Inc(R.A.Y,12);
  R2.Copy(R); R2.Move(1,0); R2.A.X:=R2.B.X-1;
  New(SB, Init(R2)); Insert(SB);
  New(TemplateViewer, Init(R,nil,SB,nil{,4096 does not compile }));
  with TemplateViewer^ do
  begin
    ReadOnly:=true;
    AlwaysShowScrollBars:=true;
  end;
  Insert(TemplateViewer);

  R.Copy(R3); R.A.X:=R.B.X-10; R.B.Y:=R.A.Y+2;
  Insert(New(PButton, Init(R, button_OK, cmOK, B2I(SelMode,bfDefault,bfNormal))));
  R.Move(0,2);
  Insert(New(PButton, Init(R, button_Edit, cmEditItem, B2I(SelMode,bfNormal,bfDefault) )));
  R.Move(0,2);
  Insert(New(PButton, Init(R, button_New, cmAddItem, bfNormal)));
  R.Move(0,2);
  Insert(New(PButton, Init(R, button_Delete, cmDeleteItem, bfNormal)));
  R.Move(0,2);
  Insert(New(PButton, Init(R, button_Cancel, cmCancel, bfNormal)));
  SelectNext(false);
end;

procedure TCodeTemplatesDialog.Update;
var C: PUnsortedStringCollection;
begin
  if CodeTemplatesLB^.Range=0 then C:=nil else
    C:=PCodeTemplate(CodeTemplatesLB^.GetFocusedItem)^.Text;
  TemplateViewer^.SetContent(C);
  ReDraw;
end;

function TCodeTemplatesDialog.GetSelectedShortCut: string;
var S: string;
begin
  if CodeTemplatesLB^.Range=0 then S:='' else
    S:=GetStr(PCodeTemplate(CodeTemplatesLB^.GetFocusedItem)^.ShortCut);
  GetSelectedShortCut:=S;
end;

procedure TCodeTemplatesDialog.HandleEvent(var Event: TEvent);
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
          if Event.InfoPtr=pointer(CodeTemplatesLB) then
            Message(@Self,evCommand,cmEditItem,nil);
        cmListFocusChanged :
          if Event.InfoPtr=pointer(CodeTemplatesLB) then
            Message(@Self,evBroadcast,cmUpdate,nil);
        cmUpdate :
          Update;
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

function TCodeTemplatesDialog.Execute: Word;
var R: word;
    P: PCodeTemplate;
    C: PCodeTemplateCollection;
    L: PUnsortedStringCollection;
    I: integer;
begin
  New(C, Init(10,20));
  if Assigned(CodeTemplates) then
  for I:=0 to CodeTemplates^.Count-1 do
    begin
      P:=CodeTemplates^.At(I);
      New(L, Init(10,50));
      P^.GetText(L);
      C^.Insert(New(PCodeTemplate, Init(P^.GetShortCut,L)));
      Dispose(L, Done);
    end;
  CodeTemplatesLB^.NewList(C);
  if StartIdx<>-1 then
    CodeTemplatesLB^.SetFocusedItem(CodeTemplates^.At(StartIdx));
  Update;
  R:=inherited Execute;
  if R=cmOK then
    begin
      if Assigned(CodeTemplates) then Dispose(CodeTemplates, Done);
      CodeTemplates:=C;
    end
  else
    Dispose(C, Done);
  Execute:=R;
end;

procedure TCodeTemplatesDialog.Add;
var P,P2: PCodeTemplate;
    IC: boolean;
    S: string;
    L: PUnsortedStringCollection;
    Cmd: word;
    CanExit: boolean;
begin
  New(L, Init(10,10));
  IC:=CodeTemplatesLB^.Range=0;
  if IC=false then
    begin
      P:=CodeTemplatesLB^.List^.At(CodeTemplatesLB^.Focused);
      P^.GetParams(S,L);
    end
  else
    begin
      S:='';
    end;
  New(P, Init(S,L));
  repeat
    Cmd:=Application^.ExecuteDialog(New(PCodeTemplateDialog, Init(dialog_newtemplate,P)), nil);
    CanExit:=(Cmd<>cmOK);
    if CanExit=false then
      begin
        P2:=PCodeTemplateCollection(CodeTemplatesLB^.List)^.SearchByShortCut(P^.GetShortCut);
        CanExit:=(Assigned(P2)=false);
        if CanExit=false then
        begin
          ClearFormatParams; AddFormatParamStr(P^.GetShortCut);
          ErrorBox(msg_codetemplate_alreadyinlist,@FormatParams);
        end;
      end;
  until CanExit;
  if Cmd=cmOK then
    begin
      CodeTemplatesLB^.List^.Insert(P);
      CodeTemplatesLB^.SetRange(CodeTemplatesLB^.List^.Count);
      CodeTemplatesLB^.SetFocusedItem(P);
      Update;
    end
  else
    Dispose(P, Done);
  Dispose(L, Done);
end;

procedure TCodeTemplatesDialog.Edit;
var P,O,P2: PCodeTemplate;
    I: sw_integer;
    S: string;
    L: PUnsortedStringCollection;
    Cmd: word;
    CanExit: boolean;
begin
  if CodeTemplatesLB^.Range=0 then Exit;
  New(L, Init(10,10));
  I:=CodeTemplatesLB^.Focused;
  O:=CodeTemplatesLB^.List^.At(I);
  O^.GetParams(S,L);
  P:=New(PCodeTemplate, Init(S, L));
  repeat
    Cmd:=Application^.ExecuteDialog(New(PCodeTemplateDialog, Init(dialog_modifytemplate,P)), nil);
    CanExit:=(Cmd<>cmOK);
    if CanExit=false then
      begin
        P2:=PCodeTemplateCollection(CodeTemplatesLB^.List)^.SearchByShortCut(P^.GetShortCut);
        CanExit:=(Assigned(P2)=false) or (CodeTemplatesLB^.List^.IndexOf(P2)=I);
        if CanExit=false then
        begin
          ClearFormatParams; AddFormatParamStr(P^.GetShortCut);
          ErrorBox(msg_codetemplate_alreadyinlist,@FormatParams);
        end;
      end;
  until CanExit;
  if Cmd=cmOK then
    begin
      with CodeTemplatesLB^ do
      begin
        List^.AtFree(I); O:=nil;
        List^.Insert(P);
        SetFocusedItem(P);
      end;
      Update;
    end;
  Dispose(L, Done);
end;

procedure TCodeTemplatesDialog.Delete;
begin
  if CodeTemplatesLB^.Range=0 then Exit;
  CodeTemplatesLB^.List^.AtFree(CodeTemplatesLB^.Focused);
  CodeTemplatesLB^.SetRange(CodeTemplatesLB^.List^.Count);
  Update;
end;


procedure RegisterCodeTemplates;
begin
{$ifndef NOOBJREG}
  RegisterType(RCodeTemplate);
  RegisterType(RCodeTemplateCollection);
{$endif}
end;

END.
