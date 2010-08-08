{
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
     begin InformationBox(msg_notemplatesavailable,nil); Exit; end;
  New(C, Init(10,10));
  R.Assign(0,0,40,14);
  New(D, Init(R, dialog_newfromtemplate));
  with D^ do
  begin
    HelpCtx:=hcNewFromTemplate;
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
    Insert(New(PLabel, Init(R, label_availabletemplates, LB)));

    GetExtent(R2); R2.Grow(-2,-3); R2.A.X:=R.B.X+2; R2.B.Y:=R2.A.Y+2;
    Insert(New(PButton, Init(R2, button_OK, cmOK, bfDefault)));
    R2.Move(0,2);
    Insert(New(PButton, Init(R2, button_Cancel, cmCancel, bfNormal)));
  end;
  LB^.Select;
  if Desktop^.ExecView(D)=cmOK then
  begin
{    Desktop^.Lock;}
    TE:=OpenEditorWindow(nil,'',0,0);
    if TE<>nil then
    begin
      TE^.Editor^.SetModified(false); { if nothing changes, we don't need to save it }
      StartTemplate(LB^.Focused,TE^.Editor);
(*      TE^.Hide; { we need this trick to get the editor updated }
      TE^.Show;*)
    end;
{    Desktop^.UnLock;}
  end;
  Dispose(D, Done);
  Dispose(C, Done);
end;


procedure TIDEApp.Open(FileName: string;FileDir:string);
var D: PFileDialog;
    OpenIt: boolean;
    DriveNumber : byte;
    StoreDir,StoreDir2 : DirStr;
    NewPSW : PSourceWindow;
begin
  OpenIt:=FileName<>'';
  DriveNumber:=0;
  if not OpenIt then
   begin
     GetDir(0,StoreDir);
     if (Length(FileDir)>1) and (FileDir[2]=':') then
       begin
         { does not assume that lowercase are greater then uppercase ! }
         if (FileDir[1]>='a') and (FileDir[1]<='z') then
           DriveNumber:=Ord(FileDir[1])-ord('a')+1
         else
           DriveNumber:=Ord(FileDir[1])-ord('A')+1;
         GetDir(DriveNumber,StoreDir2);
       end;
     if (FileDir<>'') and ExistsDir(FileDir) then
       ChDir(TrimEndSlash(FileDir));
     New(D, Init(OpenExts,dialog_openafile,label_filetoopen,fdOpenButton,hidOpenSourceFile));
     D^.HelpCtx:=hcOpen;
     OpenIt:=Desktop^.ExecView(D)<>cmCancel;
     { if I go to root under go32v2 and there is no
       floppy I get a InOutRes = 152
       get rid of it ! }
     EatIO;
     if OpenIt then
       Begin
         D^.GetFileName(FileName);
         OpenExts:=D^.WildCard;
         if ExistsDir(DirOf(FExpand(FileName))) then
           FileDir:=DirOf(FExpand(FileName));
       End;
     Dispose(D, Done);
     if DriveNumber<>0 then
       ChDir(TrimEndSlash(StoreDir2));
     ChDir(TrimEndSlash(StoreDir));
   end;
  if OpenIt then
   begin
     FileName:=FExpand(LocatePasFile(FileName));
     if ExistsFile(FileName) then
       begin
     { like for BP unexistant files should  be created PM }
         OpenEditorWindow(nil,FileName,0,0);
         if (MiscOptions and moChangeDirOnOpen)<>0 then
          begin
            ChDir(DirOf(filename)); 
            CurDirChanged;
            GetDir(0,StartUpDir);
          end;
       end
     else
       {ErrorBox(FormatStrStr(msg_cantfindfile,FileName),nil);}
       begin
         NewPSW:=OpenEditorWindow(nil,'',0,0);
         NewPSW^.Editor^.FileName:=FileName;
         NewPSW^.SetTitle(FileName);
         Message(Application,evBroadcast,cmFileNameChanged,NewPSW^.Editor);
       end;
   end;
end;

function TIDEApp.OpenSearch(FileName: string) : boolean;
var D: PFileDialog;
    OpenIt: boolean;
    P : PString;
    Dir,S : String;
begin
  OpenIt:=False;
  if not OpenIt then
   begin
     ClearFormatParams; AddFormatParamStr(FileName);
     FormatStr(S,label_lookingfor,FormatParams);
     New(D, Init(FileName,dialog_openafile,S,fdOpenButton,hidOpenSourceFile));
     D^.HelpCtx:=hcOpen;
     OpenIt:=Desktop^.ExecView(D)<>cmCancel;
     if OpenIt then
       Begin
         D^.GetFileName(FileName);
       End;
     Dispose(D, Done);
   end;
  if OpenIt then
   begin
     FileName:=FExpand(LocatePasFile(FileName));
     Dir:=DirOf(FileName);
     P:=@Dir;
     If Pos(Dir+';',GetSourceDirectories)=0 then
       if ConfirmBox(msg_confirmsourcediradd,@P,false)=cmYes then
         begin
           SourceDirs:=SourceDirs+';'+Dir;
           {$IFNDEF NODEBUG}
           if assigned(Debugger) then
             Debugger^.SetSourceDirs;
           {$ENDIF}
         end;
     OpenEditorWindow(nil,FileName,0,0);
   end;
  OpenSearch:=OpenIt;
end;


procedure TIDEApp.OpenRecentFile(RecentIndex: integer);
begin
  with RecentFiles[RecentIndex] do
  if OpenEditorWindow(nil,FileName,LastPos.X,LastPos.Y)<>nil then
     RemoveRecentFile(RecentIndex);
end;

function TIDEApp.AskSaveAll: boolean;
function CanClose(P: PView): boolean;
begin
  CanClose:=not P^.Valid(cmAskSaveAll);
end;
begin
  AskSaveAll:=Desktop^.FirstThat(@CanClose)=nil;
end;

function TIDEApp.SaveAll: boolean;

  procedure SendSave(P: PView);
  begin
    Message(P,evCommand,cmSave,nil);
  end;

begin
  SaveCancelled:=false;
  Desktop^.ForEach(@SendSave);
  SaveAll:=not SaveCancelled;
end;


procedure TIDEApp.ChangeDir;
var
  D : PChDirDialog;
begin
  New(D, Init(cdNormal, hisChDirDialog));
  D^.HelpCtx:=hcChangeDir;
  ExecuteDialog(D,nil);
  CurDirChanged;
  { Set new startup dir }
  GetDir(0,StartUpDir);
end;


procedure TIDEApp.PrinterSetup;
var R,R2: TRect;
    D: PCenterDialog;
    IL: PEditorInputLine;
begin
  R.Assign(0,0,round(ScreenWidth*54/80),4);
  New(D, Init(R, dialog_setupprinter));
  with D^ do
  begin
    GetExtent(R); R.Grow(-2,-1); Inc(R.A.Y); R.B.Y:=R.A.Y+1;
    R2.Copy(R); R2.A.X:=16; Dec(R2.B.X,4);
    New(IL, Init(R2, 255));
    IL^.Data^:=GetPrinterDevice;
    Insert(IL);
    R2.Copy(R); R2.A.X:=R2.B.X-3; R2.B.X:=R2.A.X+3;
    Insert(New(PHistory, Init(R2, IL, hidPrinterDevice)));
    R2.Copy(R); R2.B.X:=16;
    Insert(New(PLabel, Init(R2, label_setupprinter_device, IL)));
  end;
  InsertButtons(D);
  IL^.Select;
  if Desktop^.ExecView(D)=cmOK then
    SetPrinterDevice(IL^.Data^);
  Dispose(D, Done);
end;


procedure TIDEApp.Print;
  var
    d : string;
    P : PSourceWindow;
    i : longint;
    f : text;
  begin
    d:=GetPrinterDevice;
    { sanity check }
    if d='' then
      d:='prn';

    P:=Message(Desktop,evBroadcast,cmSearchWindow,nil);
    if assigned(P) then
      begin
        assign(f,d);
{$I-}
        rewrite(f);
{$I+}
        if ioresult<>0 then
          begin
            MessageBox(#3+msg_printernotopened,nil,mferror+mfokbutton);
            exit;
          end;
        for i:=0 to P^.Editor^.Core^.GetLineCount-1 do
          begin
            writeln(f,P^.Editor^.Core^.GetLineText(i));
            if ioresult<>0 then
              begin
                MessageBox(#3+msg_PrintError,nil,mferror+mfokbutton);
                close(f);
                exit;
              end;
          end;
        write(f,#12);
        close(f);
      end;
  end;
