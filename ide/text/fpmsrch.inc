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

procedure TIDEApp.Objects;
begin
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
  New(S, Init(500,500));
  BrowCol.Modules^.ForEach(@InsertInS);
  if Overflow then
    WarningBox('Too many symbols. Can''t display all of them.',nil);
  Desktop^.GetExtent(R); R.A.X:=R.B.X-35;
  Desktop^.Insert(New(PBrowserWindow, Init(R,
    'Browse: Globals',SearchFreeWindowNo,nil,'Global scope',S,nil)));
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
  New(S, Init(500,500));
  BrowCol.Modules^.ForEach(@InsertInS);
  OpenSymbolBrowser(0,0,'Units','Global scope',nil,S,nil);
end;

{
  $Log$
  Revision 1.4  1999-02-10 09:51:03  pierre
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
