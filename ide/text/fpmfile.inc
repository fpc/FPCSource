{
    $Id$
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998 by Berczi Gabor

    File menu entries

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

procedure TIDEApp.NewEditor;
begin
  OpenEditorWindow(nil,'',0,0);
end;


procedure TIDEApp.NewFromTemplate;
var D: PCenterDialog;
    R,R2: TRect;
    SB: PScrollBar;
    LB: PAdvancedListBox;
    I: integer;
    C: PUnsortedStringCollection;
    TE: PSourceWindow;
begin
  if GetTemplateCount=0 then
     begin InformationBox('No templates available.',nil); Exit; end;
  New(C, Init(10,10));
  R.Assign(0,0,40,14);
  New(D, Init(R, 'New from template'));
  with D^ do
  begin
    GetExtent(R); R.Grow(-2,-2); Inc(R.A.Y); Dec(R.B.X,12);
    R2.Copy(R); R2.Move(1,0); R2.A.X:=R2.B.X-1;
    New(SB, Init(R2)); Insert(SB);

    New(LB, Init(R,1,SB));
    LB^.Default:=true;
    for I:=0 to GetTemplateCount-1 do
      C^.Insert(NewStr(GetTemplateName(I)));
    LB^.NewList(C);
    Insert(LB);
    Dec(R.A.Y); R.B.Y:=R.A.Y+1;
    Insert(New(PLabel, Init(R, 'Available ~t~emplates', LB)));

    GetExtent(R2); R2.Grow(-2,-3); R2.A.X:=R.B.X+2; R2.B.Y:=R2.A.Y+2;
    Insert(New(PButton, Init(R2, 'O~K~', cmOK, bfDefault)));
    R2.Move(0,2);
    Insert(New(PButton, Init(R2, 'Cancel', cmCancel, bfNormal)));
  end;
  LB^.Select;
  if Desktop^.ExecView(D)=cmOK then
  begin
    Desktop^.Lock;
    TE:=OpenEditorWindow(nil,'',0,0);
    if TE<>nil then
    begin
      StartTemplate(LB^.Focused,TE^.Editor);
      TE^.Editor^.Modified:=false; { if nothing changes, we don't need to save it }
      TE^.Hide; { we need this trick to get the editor updated }
      TE^.Show;
    end;
    Desktop^.UnLock;
  end;
  Dispose(D, Done);
  Dispose(C, Done);
end;


procedure TIDEApp.Open(FileName: string);
var D: PFileDialog;
    OpenIt: boolean;
begin
  New(D, Init('*.pas','Open a file','*.pas',fdOpenButton,0));
  OpenIt:=FileName<>'';
  if not OpenIt then
  begin
     OpenIt:=Desktop^.ExecView(D)<>cmCancel;
     if OpenIt then D^.GetFileName(FileName);
  end;
  if OpenIt then
  begin
    FileName := FExpand(FileName);
    OpenEditorWindow(nil,FileName,0,0);
  end;
  Dispose(D, Done);
end;


procedure TIDEApp.OpenRecentFile(RecentIndex: integer);
begin
  with RecentFiles[RecentIndex] do
  if OpenEditorWindow(nil,FileName,LastPos.X,LastPos.Y)<>nil then
     RemoveRecentFile(RecentIndex);
end;

procedure TIDEApp.SaveAll;

  procedure SendSave(P: PView); {$ifndef FPC}far;{$endif}
  begin
    Message(P,evCommand,cmSave,nil);
  end;

begin
  Desktop^.ForEach(@SendSave);
end;


procedure TIDEApp.ChangeDir;
begin
  ExecuteDialog(New(PChDirDialog, Init(cdNormal, hisChDirDialog)),nil);
  CurDirChanged;
end;

{
  $Log$
  Revision 1.5  1999-01-21 11:54:17  peter
    + tools menu
    + speedsearch in symbolbrowser
    * working run command

  Revision 1.4  1999/01/14 21:42:21  peter
    * source tracking from Gabor

  Revision 1.3  1999/01/04 11:49:46  peter
   * 'Use tab characters' now works correctly
   + Syntax highlight now acts on File|Save As...
   + Added a new class to syntax highlight: 'hex numbers'.
   * There was something very wrong with the palette managment. Now fixed.
   + Added output directory (-FE<xxx>) support to 'Directories' dialog...
   * Fixed some possible bugs in Running/Compiling, and the compilation/run
     process revised

  Revision 1.2  1998/12/28 15:47:47  peter
    + Added user screen support, display & window
    + Implemented Editor,Mouse Options dialog
    + Added location of .INI and .CFG file
    + Option (INI) file managment implemented (see bottom of Options Menu)
    + Switches updated
    + Run program

  Revision 1.4  1998/12/24 08:27:12  gabor
    + displaying 'opening source file...' text while reading
  Revision 1.3  1998/12/22 10:39:46  peter
    + options are now written/read
    + find and replace routines

}
