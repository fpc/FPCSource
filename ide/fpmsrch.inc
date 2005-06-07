{
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998 by Berczi Gabor

    Search menu entries

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

function ProcedureDialog(S : string) : PDialog;
var D: PDialog;
    R,R1,R2: TRect;
    IL: PEditorInputLine;
begin
  R.Assign(0,0,40,8);
  New(D, Init(R, dialog_proceduredialog));
  with D^ do
  begin
    Options:=Options or ofCentered;
    GetExtent(R); R.Grow(-3,-2); R.B.Y:=R.A.Y+1;
    R1.Copy(R);
    R2.Copy(R); Inc(R2.A.Y);Inc(R2.B.Y);
    New(IL, Init(R2,255));
    Insert(IL);
    IL^.SetData(S);
    Insert(New(PLabel, Init(R1, label_enterproceduretofind, IL)));
    GetExtent(R); R.Grow(-8,-1); R.A.Y:=R.B.Y-2; R.B.X:=R.A.X+10;
    Insert(New(PButton, Init(R, button_OK, cmOK, bfDefault)));
    R.Move(15,0);
    Insert(New(PButton, Init(R, button_Cancel, cmCancel, bfNormal)));
  end;
  IL^.Select;
  ProcedureDialog:=D;
end;


procedure TIDEApp.FindProcedure;
var R: TRect;
    S: PSortedSymbolCollection;
    Overflow: boolean;
    ProcS : string;
    Level : longint;

  function NameMatches(const St : string) : boolean;
    begin
      NameMatches:=(ProcS='') or (Pos(ProcS,UpcaseStr(St)) > 0);
    end;

  procedure InsertInS(P: PSymbol); {$ifndef FPC}far;{$endif}

    procedure InsertItemsInS(P: PSymbolCollection);
    var I: Sw_integer;
    begin
      for I:=0 to P^.Count-1 do
        InsertInS(P^.At(I));
    end;

  begin
    Inc(level);
    if S^.Count=MaxCollectionSize then
       begin Overflow:=true; Exit; end;
    if {(P^.typ = procsym) this needs symconst unit which I prefer to avoid }
       ((P^.GetTypeName='proc') or (P^.GetTypeName='func'))
       and NameMatches(P^.GetName) then
      S^.Insert(P);
    { this is wrong because it inserted args or locals of proc
      in the globals list !! PM}
    if (P^.Items<>nil) and (level=1) then
      InsertItemsInS(P^.Items);
    Dec(level);
  end;

var
    EditorWindow : PSourceWindow;
begin
  level:=0;
  if BrowCol.Modules=nil then
     begin ErrorBox(msg_nodebuginfoavailable,nil); Exit; end;
  EditorWindow:=FirstEditorWindow;
  If assigned(EditorWindow) then
    ProcS:=LowerCaseStr(EditorWindow^.Editor^.GetCurrentWord)
  else
    ProcS:='';
  if ExecuteDialog(ProcedureDialog(ProcS),@ProcS)=cmCancel then
    exit;
  ProcS:=UpcaseStr(ProcS);
  Overflow:=false;
  if assigned(ProcedureCollection) then
    begin
      ProcedureCollection^.deleteAll;
      Dispose(ProcedureCollection,done);
    end;
  New(S, Init(500,500));
  ProcedureCollection:=S;
  BrowCol.Modules^.ForEach(@InsertInS);
  if Overflow then
    WarningBox(msg_toomanysymbolscantdisplayall,nil);
  Desktop^.GetExtent(R); R.A.X:=R.B.X-35;
  Desktop^.Insert(New(PBrowserWindow, Init(R,
    label_sym_findprocedure,SearchFreeWindowNo,nil,label_sym_findprocedure2+ProcS,'',S,nil,nil,nil)));
end;

procedure TIDEApp.Objects;
begin
  if ObjectTree=nil then
     begin ErrorBox(msg_nodebuginfoavailable,nil); Exit; end;

  OpenSymbolBrowser(0,0,label_sym_objects,label_sym_globalscope,nil,nil,nil,nil,ObjectTree,nil);
end;

procedure TIDEApp.Globals;
var R: TRect;
    S: PSortedSymbolCollection;
    Overflow: boolean;
    Level : longint;

  procedure InsertInS(P: PSymbol); {$ifndef FPC}far;{$endif}

    procedure InsertItemsInS(P: PSymbolCollection);
    var I: Sw_integer;
    begin
      for I:=0 to P^.Count-1 do
        InsertInS(P^.At(I));
    end;

  begin
    Inc(level);
    if S^.Count=MaxCollectionSize then
       begin Overflow:=true; Exit; end;
    S^.Insert(P);
    { this is wrong because it inserted args or locals of proc
      in the globals list !! PM}
    if (P^.Items<>nil) and (level=1) then
      InsertItemsInS(P^.Items);
    Dec(level);
  end;

begin
  level:=0;
  if BrowCol.Modules=nil then
     begin ErrorBox(msg_nodebuginfoavailable,nil); Exit; end;
  Overflow:=false;
  if assigned(GlobalsCollection) then
    begin
      GlobalsCollection^.deleteAll;
      Dispose(GlobalsCollection,done);
    end;
  New(S, Init(500,500));
  GlobalsCollection:=S;
  BrowCol.Modules^.ForEach(@InsertInS);
  if Overflow then
    WarningBox(msg_toomanysymbolscantdisplayall,nil);
  Desktop^.GetExtent(R); R.A.X:=R.B.X-35;
  Desktop^.Insert(New(PBrowserWindow, Init(R,
    label_sym_globals,SearchFreeWindowNo,nil,label_sym_globalscope,'',S,nil,nil,nil)));
end;

procedure TIDEApp.Modules;
var
    R: TRect;
    S: PSortedSymbolCollection;
procedure InsertInS(P: PSymbol); {$ifndef FPC}far;{$endif}
begin
  S^.Insert(P);
end;
begin
  if BrowCol.Modules=nil then
     begin ErrorBox(msg_nodebuginfoavailable,nil); Exit; end;
  if assigned(ModulesCollection) then
    begin
      ModulesCollection^.deleteAll;
      Dispose(ModulesCollection,done);
    end;
  New(S, Init(500,500));
  ModulesCollection:=S;
  BrowCol.Modules^.ForEach(@InsertInS);
  Desktop^.GetExtent(R); R.A.X:=R.B.X-35;
  Desktop^.Insert(New(PBrowserWindow, Init(R,
    dialog_units,SearchFreeWindowNo,nil,label_sym_globalscope,'',S,nil,nil,nil)));
end;

function SymbolDialog(S : string) : PDialog;
var D: PDialog;
    R,R1,R2: TRect;
    IL: PEditorInputLine;
begin
  R.Assign(0,0,40,8);
  New(D, Init(R, dialog_browsesymbol));
  with D^ do
  begin
    Options:=Options or ofCentered;
    GetExtent(R); R.Grow(-3,-2); R.B.Y:=R.A.Y+1;
    R1.Copy(R);
    R2.Copy(R); Inc(R2.A.Y);Inc(R2.B.Y);
    New(IL, Init(R2,255));
    Insert(IL);
    IL^.SetData(S);
    Insert(New(PLabel, Init(R1, label_entersymboltobrowse, IL)));
    GetExtent(R); R.Grow(-8,-1); R.A.Y:=R.B.Y-2; R.B.X:=R.A.X+10;
    Insert(New(PButton, Init(R, button_OK, cmOK, bfDefault)));
    R.Move(15,0);
    Insert(New(PButton, Init(R, button_Cancel, cmCancel, bfNormal)));
  end;
  IL^.Select;
  SymbolDialog:=D;
end;

procedure TIDEApp.SearchSymbol;
var
    EditorWindow : PSourceWindow;
    S : string;
begin
  EditorWindow:=FirstEditorWindow;
  If assigned(EditorWindow) then
    S:=LowerCaseStr(EditorWindow^.Editor^.GetCurrentWord)
  else
    S:='';
  if ExecuteDialog(SymbolDialog(S),@S)<>cmCancel then
    OpenOneSymbolBrowser(S);
end;

