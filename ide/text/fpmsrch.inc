{
    $Id$
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998 by Berczi Gabor

    Search menu entries

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

procedure TIDEApp.FindProcedure;
begin
  NotImplemented;
end;

procedure TIDEApp.Objects;
begin
  if ObjectTree=nil then
     begin ErrorBox('No debug info available.',nil); Exit; end;

  OpenSymbolBrowser(0,0,'Objects','Global scope',nil,nil,nil,nil,ObjectTree,nil);
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
     begin ErrorBox('No debug info available.',nil); Exit; end;
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
    WarningBox('Too many symbols. Can''t display all of them.',nil);
  Desktop^.GetExtent(R); R.A.X:=R.B.X-35;
  Desktop^.Insert(New(PBrowserWindow, Init(R,
    'Browse: Globals',SearchFreeWindowNo,nil,'Global scope','',S,nil,nil,nil)));
end;

procedure TIDEApp.Modules;
var
    S: PSortedSymbolCollection;
procedure InsertInS(P: PSymbol); {$ifndef FPC}far;{$endif}
begin
  S^.Insert(P);
end;
begin
  if BrowCol.Modules=nil then
     begin ErrorBox('No debug info available.',nil); Exit; end;
  if assigned(ModulesCollection) then
    begin
      ModulesCollection^.deleteAll;
      Dispose(ModulesCollection,done);
    end;
  New(S, Init(500,500));
  ModulesCollection:=S;
  BrowCol.Modules^.ForEach(@InsertInS);
  OpenSymbolBrowser(0,0,'Units','Global scope',nil,nil,S,nil,nil,nil);
end;

function SymbolDialog(S : string) : PDialog;
var D: PDialog;
    R,R1,R2: TRect;
    IL: PInputLine;
begin
  R.Assign(0,0,40,8);
  New(D, Init(R, 'Browse Symbol'));
  with D^ do
  begin
    Options:=Options or ofCentered;
    GetExtent(R); R.Grow(-3,-2); R.B.Y:=R.A.Y+1;
    R1.Copy(R);
    R2.Copy(R); Inc(R2.A.Y);Inc(R2.B.Y);
    New(IL, Init(R2,255));
    Insert(IL);
    IL^.SetData(S);
    Insert(New(PLabel, Init(R1, 'Enter S~y~mbol to browse', IL)));
    GetExtent(R); R.Grow(-8,-1); R.A.Y:=R.B.Y-2; R.B.X:=R.A.X+10;
    Insert(New(PButton, Init(R, '~O~K', cmOK, bfDefault)));
    R.Move(15,0);
    Insert(New(PButton, Init(R, '~C~ancel', cmCancel, bfNormal)));
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

{
  $Log$
  Revision 1.12  2000-03-07 21:57:01  pierre
   * adapted to change of OpenSymbolBrowser

  Revision 1.11  1999/07/28 23:11:21  peter
    * fixes from gabor

  Revision 1.10  1999/06/29 12:49:55  pierre
   * SearchSymbol works

  Revision 1.9  1999/06/28 19:32:22  peter
    * fixes from gabor

  Revision 1.8  1999/06/28 12:38:19  pierre
   * Globals(Modules)Collection tracing

  Revision 1.7  1999/06/25 00:34:50  pierre
   + SearchSymbol function

  Revision 1.6  1999/04/07 21:55:51  peter
    + object support for browser
    * html help fixes
    * more desktop saving things
    * NODEBUG directive to exclude debugger

  Revision 1.5  1999/03/01 15:41:59  peter
    + Added dummy entries for functions not yet implemented
    * MenuBar didn't update itself automatically on command-set changes
    * Fixed Debugging/Profiling options dialog
    * TCodeEditor converts spaces to tabs at save only if efUseTabChars is
 set
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

  Revision 1.4  1999/02/10 09:51:03  pierre
   * Adapted to TBrowserWindow changes

  Revision 1.3  1999/02/04 13:32:08  pierre
    * Several things added (I cannot commit them independently !)
    + added TBreakpoint and TBreakpointCollection
    + added cmResetDebugger,cmGrep,CmToggleBreakpoint
    + Breakpoint list in INIFile
    * Select items now also depend of SwitchMode
    * Reading of option '-g' was not possible !
    + added search for -Fu args pathes in TryToOpen
    + added code for automatic opening of FileDialog
      if source not found

  Revision 1.2  1999/01/14 21:42:23  peter
    * source tracking from Gabor

  Revision 1.1  1999/01/12 14:29:37  peter
    + Implemented still missing 'switch' entries in Options menu
    + Pressing Ctrl-B sets ASCII mode in editor, after which keypresses (even
      ones with ASCII < 32 ; entered with Alt+<###>) are interpreted always as
      ASCII chars and inserted directly in the text.
    + Added symbol browser
    * splitted fp.pas to fpide.pas

  Revision 1.9  1999/01/09 18:00:47  peter
     Original implementation

}