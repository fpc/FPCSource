{
    $Id$
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998 by Berczi Gabor

    Tool support for the IDE

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FPTools;

interface

uses Objects,Drivers,Views,Dialogs,Validate,
     BrowCol,
     WViews,
     FPViews;

const
      MsgFilterSign = 'BI#PIP#OK'#0;

type
    TCaptureTarget = (capNone,capMessageWindow,capEditWindow);

    PTool = ^TTool;
    TTool = object(TObject)
      constructor Init(const ATitle, AProgramPath, ACommandLine: string; AHotKey: word);
      function    GetTitle: string; virtual;
      procedure   GetParams(var ATitle, AProgramPath, ACommandLine: string; var AHotKey: word); virtual;
      procedure   SetParams(const ATitle, AProgramPath, ACommandLine: string; const AHotKey: word); virtual;
      destructor  Done; virtual;
    private
      Title       : PString;
      ProgramPath : PString;
      CommandLine : PString;
      HotKey      : word;
    end;

    PToolCollection = ^TToolCollection;
    TToolCollection = object(TCollection)
      function At(Index: sw_Integer): PTool;
    end;

    PToolListBox = ^TToolListBox;
    TToolListBox = object(TAdvancedListBox)
      function GetText(Item,MaxLen: Sw_Integer): String; virtual;
    end;

    PToolParamValidator = ^TToolParamValidator;
    TToolParamValidator = object(TValidator)
      function  IsValid(const S: string): Boolean; virtual;
      procedure Error; virtual;
    private
      ErrorPos: integer;
    end;

    PToolItemDialog = ^TToolItemDialog;
    TToolItemDialog = object(TCenterDialog)
      constructor Init(ATool: PTool);
      function    Execute: Word; virtual;
    private
      Tool     : PTool;
      TitleIL  : PInputLine;
      ProgramIL: PInputLine;
      ParamIL  : PInputLine;
      HotKeyRB : PRadioButtons;
    end;

    PToolsDialog = ^TToolsDialog;
    TToolsDialog = object(TCenterDialog)
      constructor Init;
      function    Execute: Word; virtual;
      procedure   HandleEvent(var Event: TEvent); virtual;
    private
      ToolsLB : PToolListBox;
      procedure Add;
      procedure Edit;
      procedure Delete;
    end;

    PToolMessage = ^TToolMessage;
    TToolMessage = object(TMessageItem)
      constructor Init(AModule: PString; ALine: string; ARow, ACol: sw_integer);
      function    GetText(MaxLen: Sw_integer): string; virtual;
    end;

    PToolMessageListBox = ^TToolMessageListBox;
    TToolMessageListBox = object(TMessageListBox)
      procedure  NewList(AList: PCollection); virtual;
      procedure  Clear; virtual;
      procedure  Update; virtual;
      function   GetPalette: PPalette; virtual;
      destructor Done; virtual;
    end;

    PMessagesWindow = ^TMessagesWindow;
    TMessagesWindow = object(TFPWindow)
      constructor Init;
      procedure   Update; virtual;
      procedure   HandleEvent(var Event: TEvent); virtual;
      function    GetPalette: PPalette; virtual;
      destructor  Done; virtual;
    private
      MsgLB : PToolMessageListBox;
    end;

procedure InitTools;
function  GetToolCount: sw_integer;
function  GetToolName(Idx: sw_integer): string;
function  AddTool(Title, ProgramPath, Params: string; HotKey: word): sw_integer;
procedure GetToolParams(Idx: sw_integer; var Title, ProgramPath, Params: string; var HotKey: word);
procedure SetToolParams(Idx: sw_integer; Title, ProgramPath, Params: string; HotKey: word);
procedure DoneTools;

function GetHotKeyName(Key: word): string;

function ParseToolParams(var Params: string; CheckOnly: boolean): integer;

function  ProcessMessageFile(const MsgFileName: string): boolean;
procedure AddToolCommand(Command: string);
procedure AddToolMessage(ModuleName, Text: string; Row, Col: longint);
procedure ClearToolMessages;
procedure UpdateToolMessages;

const
     ToolFilter     : string           = '';
     CaptureToolTo  : TCaptureTarget   = capNone;
     ToolMessages   : PCollection      = nil;
     ToolModuleNames: PStoreCollection = nil;
     MessagesWindow : PMessagesWindow  = nil;
     LastToolMessageFocused : PToolMessage = nil;

implementation

uses Dos,
     Commands,App,MsgBox,
     FPConst,FPVars,FPUtils;

type
    THotKeyDef = record
      Name     : string[12];
      KeyCode  : word;
    end;

const
     HotKeys : array[0..8] of THotKeyDef =
      ( (Name : '~U~nassigned' ; KeyCode : kbNoKey   ),
{        (Name : 'Shift+F~2~'   ; KeyCode : kbShiftF2 ), }
        (Name : 'Shift+F~3~'   ; KeyCode : kbShiftF3 ),
        (Name : 'Shift+F~4~'   ; KeyCode : kbShiftF4 ),
        (Name : 'Shift+F~5~'   ; KeyCode : kbShiftF5 ),
        (Name : 'Shift+F~6~'   ; KeyCode : kbShiftF6 ),
        (Name : 'Shift+F~7~'   ; KeyCode : kbShiftF7 ),
        (Name : 'Shift+F~8~'   ; KeyCode : kbShiftF8 ),
        (Name : 'Shift+F~9~'   ; KeyCode : kbShiftF9 ),
        (Name : 'Shift+F~1~0'  ; KeyCode : kbShiftF10));

     Tools     : PToolCollection = nil;

function GetHotKeyCount: integer;
begin
  GetHotKeyCount:=ord(High(HotKeys))-ord(Low(HotKeys))+1;
end;

function GetHotKeyNameByIdx(Idx: integer): string;
begin
  GetHotKeyNameByIdx:=HotKeys[Idx].Name;
end;

function HotKeyToIdx(Key: word): integer;
var Count,I: integer;
    Found: boolean;
begin
  Count:=GetHotKeyCount; Found:=false;
  I:=0;
  while (I<Count) and (Found=false) do
  begin
    Found:=HotKeys[I].KeyCode=Key;
    if Found=false then
    Inc(I);
  end;
  if Found=false then I:=-1;
  HotKeyToIdx:=I;
end;

function IdxToHotKey(Idx: integer): word;
var Count: integer;
    Key: word;
begin
  Count:=GetHotKeyCount;
  if (0<=Idx) and (Idx<Count) then
    Key:=HotKeys[Idx].KeyCode
  else
    Key:=kbNoKey;
  IdxToHotKey:=Key;
end;

function GetHotKeyName(Key: word): string;
var Idx: integer;
    S: string;
begin
  Idx:=HotKeyToIdx(Key);
  if Idx=0 then S:='' else
   if Idx=-1 then S:='???' else
    S:=GetHotKeyNameByIdx(Idx);
  GetHotKeyName:=S;
end;

constructor TTool.Init(const ATitle, AProgramPath, ACommandLine: string; AHotKey: word);
begin
  inherited Init;
  SetParams(ATitle,AProgramPath,ACommandLine,AHotKey);
end;

function TTool.GetTitle: string;
begin
  GetTitle:=KillTilde(GetStr(Title));
end;

procedure TTool.GetParams(var ATitle, AProgramPath, ACommandLine: string; var AHotKey: word);
begin
  ATitle:=GetStr(Title); AProgramPath:=GetStr(ProgramPath);
  ACommandLine:=GetStr(CommandLine);
  AHotKey:=HotKey;
end;

procedure TTool.SetParams(const ATitle, AProgramPath, ACommandLine: string; const AHotKey: word);
begin
  if Title<>nil then DisposeStr(Title); Title:=nil;
  if ProgramPath<>nil then DisposeStr(ProgramPath); ProgramPath:=nil;
  if CommandLine<>nil then DisposeStr(CommandLine); CommandLine:=nil;
  Title:=NewStr(ATitle); ProgramPath:=NewStr(AProgramPath);
  CommandLine:=NewStr(ACommandLine);
  HotKey:=AHotKey;
end;

destructor TTool.Done;
begin
  inherited Done;
  if Title<>nil then DisposeStr(Title);
  if ProgramPath<>nil then DisposeStr(ProgramPath);
  if CommandLine<>nil then DisposeStr(CommandLine);
end;

function TToolCollection.At(Index: sw_Integer): PTool;
begin
  At:=inherited At(Index);
end;

function TToolListBox.GetText(Item,MaxLen: sw_integer): String;
var S: string;
    P: PTool;
begin
  P:=List^.At(Item);
  S:=P^.GetTitle;
  GetText:=copy(S,1,MaxLen);
end;

procedure InitTools;
begin
  if Tools<>nil then DoneTools;
  New(Tools, Init(10,20));
end;

function  GetToolCount: sw_integer;
var Count: integer;
begin
  if Tools=nil then Count:=0 else
    Count:=Tools^.Count;
  GetToolCount:=Count;
end;

function GetToolName(Idx: sw_integer): string;
var S1,S2: string;
    W: word;
begin
  GetToolParams(Idx,S1,S2,S2,W);
  GetToolName:=KillTilde(S1);
end;

function AddTool(Title, ProgramPath, Params: string; HotKey: word): sw_integer;
var P: PTool;
begin
  if Tools=nil then InitTools;
  New(P, Init(Title,ProgramPath,Params,HotKey));
  Tools^.Insert(P);
  AddTool:=Tools^.IndexOf(P);
end;

procedure GetToolParams(Idx: sw_integer; var Title, ProgramPath, Params: string; var HotKey: word);
var P: PTool;
begin
  P:=Tools^.At(Idx);
  P^.GetParams(Title,ProgramPath,Params,HotKey);
end;

procedure SetToolParams(Idx: sw_integer; Title, ProgramPath, Params: string; HotKey: word);
var P: PTool;
begin
  P:=Tools^.At(Idx);
  P^.GetParams(Title,ProgramPath,Params,HotKey);
end;

procedure DoneTools;
begin
  if Tools<>nil then Dispose(Tools, Done); Tools:=nil;
end;

procedure TToolParamValidator.Error;
begin
  MsgParms[1].Long:=ErrorPos;
  ErrorBox(^C'Error parsing parameters line at line position %d.',@MsgParms);
end;

function TToolParamValidator.IsValid(const S: string): Boolean;
var P: string;
begin
  P:=S;
  ErrorPos:=ParseToolParams(P,true);
  IsValid:=ErrorPos=0;
end;

constructor TToolItemDialog.Init(ATool: PTool);
var R,R2,R3: TRect;
    Items: PSItem;
    I,KeyCount: sw_integer;
begin
  KeyCount:=GetHotKeyCount;

  R.Assign(0,0,60,Max(3+KeyCount,12));
  inherited Init(R,'Modify/New Tool');
  Tool:=ATool;

  GetExtent(R); R.Grow(-3,-2); R3.Copy(R);
  Inc(R.A.Y); R.B.Y:=R.A.Y+1; R.B.X:=R.A.X+36;
  New(TitleIL, Init(R, 128)); Insert(TitleIL);
  R2.Copy(R); R2.Move(-1,-1); Insert(New(PLabel, Init(R2, '~T~itle', TitleIL)));
  R.Move(0,3);
  New(ProgramIL, Init(R, 128)); Insert(ProgramIL);
  R2.Copy(R); R2.Move(-1,-1); Insert(New(PLabel, Init(R2, 'Program ~p~ath', ProgramIL)));
  R.Move(0,3);
  New(ParamIL, Init(R, 128)); Insert(ParamIL);
  ParamIL^.SetValidator(New(PToolParamValidator, Init));
  R2.Copy(R); R2.Move(-1,-1); Insert(New(PLabel, Init(R2, 'Command ~l~ine', ParamIL)));

  R.Copy(R3); Inc(R.A.X,38); R.B.Y:=R.A.Y+KeyCount;
  Items:=nil;
  for I:=KeyCount-1 downto 0 do
    Items:=NewSItem(GetHotKeyNameByIdx(I), Items);
  New(HotKeyRB, Init(R, Items));
  Insert(HotKeyRB);

  InsertButtons(@Self);

  TitleIL^.Select;
end;

function TToolItemDialog.Execute: Word;
var R: word;
    S1,S2,S3: string;
    W: word;
    L: longint;
begin
  Tool^.GetParams(S1,S2,S3,W);
  TitleIL^.SetData(S1); ProgramIL^.SetData(S2); ParamIL^.SetData(S3);
  L:=HotKeyToIdx(W); if L=-1 then L:=255;
  HotKeyRB^.SetData(L);
  R:=inherited Execute;
  if R=cmOK then
  begin
    TitleIL^.GetData(S1); ProgramIL^.GetData(S2); ParamIL^.GetData(S3);
    HotKeyRB^.GetData(L); W:=IdxToHotKey(L);
    Tool^.SetParams(S1,S2,S3,W);
  end;
  Execute:=R;
end;

constructor TToolsDialog.Init;
var R,R2,R3: TRect;
    SB: PScrollBar;
begin
  R.Assign(0,0,46,16);
  inherited Init(R,'Tools');

  GetExtent(R); R.Grow(-3,-2); Inc(R.A.Y); R3.Copy(R); Dec(R.B.X,12);
  R2.Copy(R); R2.Move(1,0); R2.A.X:=R2.B.X-1;
  New(SB, Init(R2)); Insert(SB);
  New(ToolsLB, Init(R,1,SB));
  Insert(ToolsLB);
  R2.Copy(R); R2.Move(0,-1); R2.B.Y:=R2.A.Y+1; Dec(R2.A.X);
  Insert(New(PLabel, Init(R2, '~P~rogram titles', ToolsLB)));

  R.Copy(R3); R.A.X:=R.B.X-10; R.B.Y:=R.A.Y+2;
  Insert(New(PButton, Init(R, 'O~K~', cmOK, bfNormal)));
  R.Move(0,2);
  Insert(New(PButton, Init(R, '~E~dit', cmEditItem, bfDefault)));
  R.Move(0,2);
  Insert(New(PButton, Init(R, '~N~ew', cmAddItem, bfNormal)));
  R.Move(0,2);
  Insert(New(PButton, Init(R, '~D~elete', cmDeleteItem, bfNormal)));
  R.Move(0,2);
  Insert(New(PButton, Init(R, 'Cancel', cmCancel, bfNormal)));
  SelectNext(false);
end;

procedure TToolsDialog.HandleEvent(var Event: TEvent);
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
          if Event.InfoPtr=pointer(ToolsLB) then
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

function TToolsDialog.Execute: Word;
var R: word;
    C: PToolCollection;
    I: integer;
    S1,S2,S3: string;
    W: word;
begin
  New(C, Init(10,20));
  if Tools<>nil then
  for I:=0 to Tools^.Count-1 do
    begin
      Tools^.At(I)^.GetParams(S1,S2,S3,W);
      C^.Insert(New(PTool, Init(S1,S2,S3,W)));
    end;
  ToolsLB^.NewList(C);
  R:=inherited Execute;
  if R=cmOK then
    begin
      if Tools<>nil then Dispose(Tools, Done);
      Tools:=C;
      Message(Application,evBroadcast,cmUpdateTools,nil);
    end
  else
    Dispose(C, Done);
  Execute:=R;
end;

procedure TToolsDialog.Add;
var P: PTool;
    IC: boolean;
    S1,S2,S3: string;
    W: word;
begin
  if ToolsLB^.Range>=MaxToolCount then
    begin InformationBox(^C'Can''t install more tools...',nil); Exit; end;
  IC:=ToolsLB^.Range=0;
  if IC=false then
    begin
      P:=ToolsLB^.List^.At(ToolsLB^.Focused);
      P^.GetParams(S1,S2,S3,W);
    end
  else
    begin
      S1:=''; S2:=''; S3:=''; W:=0;
    end;
  New(P, Init(S1,S2,S3,W));
  if Application^.ExecuteDialog(New(PToolItemDialog, Init(P)), nil)=cmOK then
    begin
      ToolsLB^.List^.Insert(P);
      ToolsLB^.SetRange(ToolsLB^.List^.Count);
      ReDraw;
    end
  else
    Dispose(P, Done);
end;

procedure TToolsDialog.Edit;
var P: PTool;
begin
  if ToolsLB^.Range=0 then Exit;
  P:=ToolsLB^.List^.At(ToolsLB^.Focused);
  Application^.ExecuteDialog(New(PToolItemDialog, Init(P)), nil);
  ReDraw;
end;

procedure TToolsDialog.Delete;
begin
  if ToolsLB^.Range=0 then Exit;
  ToolsLB^.List^.AtFree(ToolsLB^.Focused);
  ToolsLB^.SetRange(ToolsLB^.List^.Count);
  ReDraw;
end;

procedure ReplaceStr(var S: string; const What,NewS: string);
var I : integer;
begin
  repeat
    I:=Pos(What,S);
    if I>0 then
    begin
      Delete(S,I,length(What));
      Insert(NewS,S,I);
    end;
  until I=0;
end;

procedure ReplaceStrI(var S: string; What: string; const NewS: string);
var I : integer;
    UpcaseS: string;
begin
  UpcaseS:=UpcaseStr(S); What:=UpcaseStr(What);
  repeat
    I:=Pos(What,UpcaseS);
    if I>0 then
    begin
      Delete(S,I,length(What));
      Insert(NewS,S,I);
    end;
  until I=0;
end;

function ParseToolParams(var Params: string; CheckOnly: boolean): integer;
var Err: integer;
    W: PSourceWindow;
procedure ParseParams(Pass: sw_integer);
var I: sw_integer;
function IsAlpha(Ch: char): boolean;
begin
  IsAlpha:=(Upcase(Ch) in['A'..'Z','_','$']);
end;
function ReplacePart(StartP,EndP: integer; const S: string): integer;
begin
  Params:=copy(Params,1,StartP-1)+S+copy(Params,EndP+1,255);
  ReplacePart:=length(S)-(EndP-StartP+1);
end;
function Consume(Ch: char): boolean;
var OK: boolean;
begin
  OK:=Params[I]=Ch;
  if OK then Inc(I);
  Consume:=OK;
end;
function ReadTill(var S: string; C: char): boolean;
var Found: boolean;
begin
  Found:=false; S:='';
  while (I<=length(Params)) and (Found=false) do
    begin
      Found:=Params[I]=C;
      if Found=false then
        begin
          S:=S+Params[I];
          Inc(I);
        end;
    end;
  ReadTill:=Found;
end;
var C,PrevC: char;
    WordS: string;
    LastWordStart: sw_integer;
    L: longint;
    S: string;
    D: DirStr; N: NameStr; E: ExtStr;
begin
  I:=1; WordS:=''; LastWordStart:=I; PrevC:=' ';
  while (I<=length(Params)+1) and (Err=0) do
  begin
    if I<=length(Params) then C:=Params[I];
    if (I<=length(Params)) and IsAlpha(C) then
     begin
       if (I=1) or (IsAlpha(PrevC)=false) then
         begin WordS:=''; LastWordStart:=I; end;
{       if IsAlpha(C) then ForceConcat:=false;}
       WordS:=WordS+C;
     end
    else
      begin
        WordS:=UpcaseStr(Trim(WordS));
        if WordS<>'' then
        if (WordS='$CAP') then
          begin
            if (Pass=0) then
              if (Params[I]=' ') and (I<=255) then Params[I]:='_';
          end else
        if (WordS='$CAP_MSG') then
          begin
            if (Pass=2) then
              if Consume('(')=false then Err:=I else
              if ReadTill(S,')')=false then Err:=I else
              begin
                Consume(')');
                I:=I+ReplacePart(LastWordStart,I-1,'');
                ToolFilter:=S;
                CaptureToolTo:=capMessageWindow;
              end;
          end else
        if (WordS='$CAP_EDIT') then
          begin
            if (Pass=2) then
              begin
                I:=I+ReplacePart(LastWordStart,I-1,'');
                CaptureToolTo:=capEditWindow;
              end;
          end else
        if (WordS='$COL') then
          begin
            if (Pass=1) then
            begin
              if W=nil then L:=0 else
                L:=W^.Editor^.CurPos.X+1;
              I:=I+ReplacePart(LastWordStart,I-1,IntToStr(L));
            end;
          end else
        if (WordS='$CONFIG') then
          begin
            if (Pass=1) then
              I:=I+ReplacePart(LastWordStart,I-1,INIPath);
          end else
        if (WordS='$DIR') then
          begin
            if (Pass=2) then
              if Consume('(')=false then Err:=I else
              if ReadTill(S,')')=false then Err:=I else
              begin
                Consume(')');
                FSplit(S,D,N,E);
                I:=I+ReplacePart(LastWordStart,I-1,D);
              end;
          end else
        if (WordS='$DRIVE') then
          begin
            if (Pass=2) then
              if Consume('(')=false then Err:=I else
              if ReadTill(S,')')=false then Err:=I else
              begin
                Consume(')');
                FSplit(S,D,N,E);
                L:=Pos(':',D); if L=0 then L:=-1;
                D:=copy(D,1,L+1);
                I:=I+ReplacePart(LastWordStart,I-1,D);
              end;
          end else
        if (WordS='$EDNAME') then
          begin
            if (Pass=1) then
            begin
              if W=nil then S:='' else
                S:=W^.Editor^.FileName;
              I:=I+ReplacePart(LastWordStart,I-1,S);
            end;
          end else
        if (WordS='$EXENAME') then
          begin
            if (Pass=1) then
              I:=I+ReplacePart(LastWordStart,I-1,EXEFile);
          end else
        if (WordS='$EXT') then
          begin
            if (Pass=2) then
              if Consume('(')=false then Err:=I else
              if ReadTill(S,')')=false then Err:=I else
              begin
                Consume(')');
                FSplit(S,D,N,E); E:=copy(E,2,255);
                I:=I+ReplacePart(LastWordStart,I-1,E);
              end;
          end else
        if (WordS='$LINE') then
          begin
            if (Pass=1) then
            begin
              if W=nil then L:=0 else
                L:=W^.Editor^.CurPos.Y+1;
              I:=I+ReplacePart(LastWordStart,I-1,IntToStr(L));
            end;
          end else
        if (WordS='$NAME') then
          begin
            if (Pass=2) then
              if Consume('(')=false then Err:=I else
              if ReadTill(S,')')=false then Err:=I else
              begin
                Consume(')');
                FSplit(S,D,N,E);
                I:=I+ReplacePart(LastWordStart,I-1,N);
              end;
          end else
        if (WordS='$NAMEEXT') then
          begin
            if (Pass=2) then
              if Consume('(')=false then Err:=I else
              if ReadTill(S,')')=false then Err:=I else
              begin
                Consume(')');
                FSplit(S,D,N,E);
                I:=I+ReplacePart(LastWordStart,I-1,N+E);
              end;
          end else
        if (WordS='$NOSWAP') then
          begin
            if (Pass=1) then
            begin
              I:=I+ReplacePart(LastWordStart,I-1,'');
            end;
          end else
        if (WordS='$DRIVE') then
          begin
            if (Pass=2) then
              if Consume('(')=false then Err:=I else
              if ReadTill(S,')')=false then Err:=I else
              begin
                Consume(')');
                FSplit(S,D,N,E);
                L:=Pos(':',D); if L=0 then L:=-1;
                D:=copy(D,1,L+1);
                I:=I+ReplacePart(LastWordStart,I-1,D);
              end;
          end else
        if (WordS='$PROMPT') then
          begin
            if (Pass=3) then
              if Params[I]='(' then
                begin
                  if Consume('(')=false then Err:=I else
                  if ReadTill(S,')')=false then Err:=I else
                  begin
                    Consume(')');

                  end;
                end
              else { just prompt for parms }
                begin
                  I:=I+ReplacePart(LastWordStart,I-1,'');
                  if CheckOnly=false then
                    begin
                      S:=copy(Params,I+1,255);
                      if InputBox('Program Arguments', '~E~nter program argument',
                        S,255-I+1)=cmOK then
                        begin
                          ReplacePart(LastWordStart,255,S);
                          I:=255;
                        end
                      else
                        Err:=-1;
                    end;
                end;
          end else
        if (WordS='$SAVE') then
          begin
            if (Pass=0) then
              if (Params[I]=' ') and (I<=255) then Params[I]:='_';
          end else
        if (WordS='$SAVE_ALL') then
          begin
            if (Pass=2) then
              begin
                I:=I+ReplacePart(LastWordStart,I-1,'');
                Message(Application,evCommand,cmSaveAll,nil);
              end;
          end else
        if (WordS='$SAVE_CUR') then
          begin
            if (Pass=2) then
              begin
                I:=I+ReplacePart(LastWordStart,I-1,'');
                Message(W,evCommand,cmSave,nil);
              end;
          end else
        if (WordS='$SAVE_PROMPT') then
          begin
            if (Pass=2) then
              begin
                I:=I+ReplacePart(LastWordStart,I-1,'');
                if W<>nil then
                  if W^.Editor^.SaveAsk=false then
                    Err:=-1;
              end;
          end else
        if copy(WordS,1,1)='$' then
          Err:=LastWordStart;
        WordS:='';
      end;
    PrevC:=C;
    Inc(I);
  end;
end;
var Pass: sw_integer;
begin
  CaptureToolTo:=capNone;
  ToolFilter:='';
  W:=FirstEditorWindow;
  Err:=0;
  for Pass:=0 to 3 do
    begin
      ParseParams(Pass);
      if Err<>0 then Break;
    end;
  ParseToolParams:=Err;
end;

function ProcessMessageFile(const MsgFileName: string): boolean;
var OK,Done: boolean;
    S: PBufStream;
    C: char;
    Sign: array[1..10] of char;
    InFileName,InReference: boolean;
    AddChar: boolean;
    FileName,Line: string;
    Row,Col: longint;
procedure AddLine;
begin
  Row:=ord(Line[1])+ord(Line[2]) shl 8;
  Col:=ord(Line[3])+ord(Line[4]) shl 8;
  AddToolMessage(FileName,copy(Line,5,255),Row,Col);
end;
begin
  New(S, Init(MsgFileName, stOpenRead, 4096));
  OK:=(S<>nil) and (S^.Status=stOK);
  if OK then S^.Read(Sign,SizeOf(Sign));
  OK:=OK and (Sign=MsgFilterSign);
  Done:=false; InFileName:=false; InReference:=false;
  while OK and (Done=false) do
    begin
      S^.Read(C,SizeOf(C));
      OK:=(S^.Status=stOK);
      AddChar:=false;
      if OK then
      case C of
        #0   : if InFileName then
                 begin InFileName:=false end else
               if InReference then
                 begin
                   if (length(Line)>4) then
                     begin
                       AddLine;
                       InReference:=false;
                     end
                   else
                     AddChar:=true;
                 end else
               begin InFileName:=true; FileName:=''; end;
        #1   : if InReference then AddChar:=true else
                 begin InReference:=true; Line:=''; end;
        #127 : if InReference then AddChar:=true else
                 Done:=true;
      else AddChar:=true;
      end;
      if AddChar then
        if InFileName then
          FileName:=FileName+C else
        if InReference then
          Line:=Line+C;
    end;
  if S<>nil then Dispose(S, Done);
  ProcessMessageFile:=OK;
end;

constructor TToolMessage.Init(AModule: PString; ALine: string; ARow, ACol: sw_integer);
begin
  inherited Init(0,ALine,AModule,ARow,ACol);
end;

function TToolMessage.GetText(MaxLen: Sw_integer): string;
var S: string;
begin
  if Module=nil then
    S:=GetStr(Text)
  else
    S:=NameAndExtOf(GetModuleName)+
       '('+IntToStr(Row)+'): '+GetStr(Text);
  GetText:=copy(S,1,MaxLen);
end;

procedure AddToolCommand(Command: string);
begin
  AddToolMessage('',Command,0,0);
  LastToolMessageFocused:=ToolMessages^.At(ToolMessages^.Count-1);
end;

procedure AddToolMessage(ModuleName, Text: string; Row, Col: longint);
var MN: PString;
begin
  if ToolMessages=nil then
    New(ToolMessages, Init(500,1000));
  if ToolModuleNames=nil then
    New(ToolModuleNames, Init(50,100));
  MN:=ToolModuleNames^.Add(ModuleName);
  ToolMessages^.Insert(New(PToolMessage, Init(MN,Text,Row,Col)));
end;

procedure ClearToolMessages;
begin
  if ToolMessages<>nil then Dispose(ToolMessages,Done); ToolMessages:=nil;
  if ToolModuleNames<>nil then Dispose(ToolModuleNames, Done); ToolModuleNames:=nil;
  LastToolMessageFocused:=nil;
end;

procedure UpdateToolMessages;
begin
  if Assigned(MessagesWindow) then
    MessagesWindow^.Update;
end;

procedure TToolMessageListBox.Update;
var P: PMessageItem;
    Idx: integer;
begin
  P:=LastToolMessageFocused;
  NewList(ToolMessages);
  if (Range>0) and (P<>nil) then
    begin
      Idx:=List^.IndexOf(P);
      if Idx>=0 then
        begin
          FocusItem(Idx);
          DrawView;
        end;
    end;
  DrawView;
end;

procedure TToolMessageListBox.NewList(AList: PCollection);
begin
  if (List=ToolMessages) or (ToolMessages=nil) then
    begin List:=nil; SetRange(0); end;
  inherited NewList(AList);
end;

procedure TToolMessageListBox.Clear;
begin
  ClearToolMessages;
  Update;
end;

function TToolMessageListBox.GetPalette: PPalette;
const
  P: string[length(CBrowserListBox)] = CBrowserListBox;
begin
  GetPalette:=@P;
end;

destructor TToolMessageListBox.Done;
begin
  HScrollBar:=nil; VScrollBar:=nil;
  if List=ToolMessages then begin List:=nil; SetRange(0); end;
  inherited Done;
end;

constructor TMessagesWindow.Init;
var R: TRect;
    HSB,VSB: PScrollBar;
begin
  Desktop^.GetExtent(R); R.A.Y:=R.B.Y-7;
  inherited Init(R,'Messages',SearchFreeWindowNo);
  HelpCtx:=hcMessagesWindow;

  HSB:=StandardScrollBar(sbHorizontal+sbHandleKeyboard); Insert(HSB);
  VSB:=StandardScrollBar(sbVertical+sbHandleKeyboard); Insert(VSB);

  GetExtent(R); R.Grow(-1,-1);
  New(MsgLB, Init(R, HSB, VSB));
  Insert(MsgLB);

  Update;

  MessagesWindow:=@Self;
end;

procedure TMessagesWindow.Update;
begin
  MsgLB^.Update;
end;

procedure TMessagesWindow.HandleEvent(var Event: TEvent);
begin
  case Event.What of
    evBroadcast :
      case Event.Command of
        cmListFocusChanged :
          if Event.InfoPtr=MsgLB then
            LastToolMessageFocused:=MsgLB^.List^.At(MsgLB^.Focused);
      end;
  end;
  inherited HandleEvent(Event);
end;

function TMessagesWindow.GetPalette: PPalette;
const S: string[length(CBrowserWindow)] = CBrowserWindow;
begin
  GetPalette:=@S;
end;

destructor TMessagesWindow.Done;
begin
  MessagesWindow:=nil;
  inherited Done;
end;

END.
{
  $Log$
  Revision 1.4  1999-03-01 15:42:04  peter
    + Added dummy entries for functions not yet implemented
    * MenuBar didn't update itself automatically on command-set changes
    * Fixed Debugging/Profiling options dialog
    * TCodeEditor converts spaces to tabs at save only if efUseTabChars is set
    * efBackSpaceUnindents works correctly
    + 'Messages' window implemented
    + Added '$CAP MSG()' and '$CAP EDIT' to available tool-macros
    + Added TP message-filter support (for ex. you can call GREP thru
      GREP2MSG and view the result in the messages window - just like in TP)
    * A 'var' was missing from the param-list of THelpFacility.TopicSearch,
      so topic search didn't work...
    * In FPHELP.PAS there were still context-variables defined as word instead
      of THelpCtx
    * StdStatusKeys() was missing from the statusdef for help windows
    + Topic-title for index-table can be specified when adding a HTML-files

  Revision 1.3  1999/02/22 02:15:19  peter
    + default extension for save in the editor
    + Separate Text to Find for the grep dialog
    * fixed redir crash with tp7

  Revision 1.2  1999/02/19 15:43:21  peter
    * compatibility fixes for FV

  Revision 1.1  1999/01/21 11:54:25  peter
    + tools menu
    + speedsearch in symbolbrowser
    * working run command

  Revision 1.0  1999/01/16 10:43:31  gabor
      Original implementation

}
