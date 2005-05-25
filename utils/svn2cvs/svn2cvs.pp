{$mode objfpc}
{$H+}
program svn2cvs;

uses Classes,sysutils,process,DOM,xmlread,custapp,IniFiles;

Const
  SGlobal       = 'Global';
  KeyCVSBin     = 'CVSBinary';
  KeySVNBin     = 'SVNBinary';
  KeySVNURL     = 'SVNURL';
  KeyCVSROOT    = 'CVSROOT';
  KeyRepository = 'CVSRepository';
  KeyRevision   = 'Revision';
  KeyWorkDir    = 'WorkingDir';

Resourcestring
  SErrFailedToCheckOut     = 'Failed to check out SVN repository';
  SErrFailedToInitCVS      = 'Failed to initialize CVS: ';
  SErrNoRepository         = 'Cannot initialize CVS: no CVS Repository specified';
  SErrDirectoryFailed      = 'Failed to create directory : %s';
  SErrFailedToGetVersions  = 'Failed to retrieve SVN versions';
  SErrInValidSVNLog        = 'Invalid SVN log.';
  SErrUpdateFailed         = 'Update to revision %d failed.';
  SErrFailedToCommit       = 'Failed to commit to CVS.';
  SErrFailedToRemove       = 'Failed to remove file: %s';
  SErrFailedToAddDirectory = 'Failed to add directory to CVS: %s';
  SErrFailedToAddFile      = 'Failed to add file to CVS: %s';
  SErrDirectoryNotInCVS    = 'Directory not in CVS: %s';
  SLogRevision             = 'Revision %s by %s :';
  SConvertingRevision      = 'Converting revision : %d';
  SWarnUnknownAction       = 'Warning: Unknown action: "%s" for filename : "%s"';
  SWarnErrorInLine         = 'Warning: Erroneous file line : %s';
  SExecuting               = 'Executing: %s';
  
Type

 { TSVN2CVSApp }
 TVersion = Class(TCollectionItem)
 private
   FAuthor: String;
   FDate: string;
   FLogMessage: String;
   FRevision: Integer;
 Public
   Property Revision : Integer read FRevision;
   Property LogMessage : String Read FLogMessage;
   Property Date : string Read FDate;
   Property Author : String Read FAuthor;
 end;
 
 { TVersions }

 TVersions = Class(TCollection)
 private
   function GetVersion(Index : INteger): TVersion;
   procedure SetVersion(Index : INteger; const AValue: TVersion);
 Protected
   procedure ConvertLogEntry(E : TDomElement);
 public
   Procedure LoadFromXML(Doc : TXMlDocument);
   property Versions[Index : INteger] : TVersion Read GetVersion Write SetVersion; Default;
 end;

 { TSVN2CVSApp }

 TSVN2CVSApp = Class(TCustomApplication)
 Public
   SVNBin : String;
   CVSBin : String;
   versions : TVersions;
   WorkingDir : String;
   StartRevision : Integer;
   SVNURL : String;
   CVSROOT : String;
   CVSRepository : String;
   Function RunCmd(Cmd: String; CmdOutput: TStream): Boolean;
   Function RunSVN(Cmd : String; CmdOutput : TStream) : Boolean;
   Function RunCVS(Cmd : String; CmdOutput : TStream) : Boolean;
   Function UpdateSVN(Version : TVersion; Files : TStrings) : Boolean;
   Procedure WriteLogMessage(Version : TVersion);
   Procedure UpdateEntry(AFileName : String);
   Procedure DeleteEntry(AFileName : String);
   Procedure DoCVSEntries(Version : TVersion;Files : TStrings);
   procedure CheckInCVS;
   procedure CheckOutSVN(Files : TStrings);
   Procedure ConvertVersion(Version : TVersion);
   Procedure ConvertRepository;
   procedure GetVersions;
   procedure ProcessConfigFile;
   Function  ProcessArguments : Boolean;
   Procedure DoRun; override;
 end;

 AppError = Class(Exception);

{ TVersions }

function TVersions.GetVersion(Index : INteger): TVersion;
begin
  Result:=Items[Index] as Tversion;
end;

procedure TVersions.SetVersion(Index : INteger; const AValue: TVersion);
begin
  Items[Index]:=AValue;
end;

procedure TVersions.ConvertLogEntry(E : TDomElement);

  Function GetNodeText(N : TDomNode) : String;
  
  begin
    N:=N.FirstChild;
    If N<>Nil then
      Result:=N.NodeValue;
  end;

Var
  N : TDomNode;
  V : TVersion;

begin
  V:=Add as TVersion;
  V.FRevision:=StrToIntDef(E['revision'],-1);
  N:=E.FirstChild;
  While (N<>Nil) do
    begin
    If (N.NodeType=ELEMENT_NODE) then
      begin
      if (N.NodeName='author') then
        V.FAuthor:=GetNodeText(N)
      else If (N.NodeName='date') then
        V.FDate:=GetNodeText(N)
      else If (N.NodeName='msg') then
        V.FLogMessage:=GetNodeText(N);
      end;
    N:=N.NextSibling;
    end;
end;

procedure TVersions.LoadFromXML(Doc: TXMlDocument);

var
  L : TDomNode;
  E : TDomElement;

begin
  L:=Doc.FirstChild;
  While (L<>Nil) and not ((L.NodeType=ELEMENT_NODE) and (L.NodeName='log')) do
    L:=L.NextSibling;
  if (L=Nil) then
    Raise AppError.Create(SErrInValidSVNLog);
  L:=L.FirstChild;
  While (L<>Nil) do
    begin
    If (L.NodeType=ELEMENT_NODE) and (L.NodeName='logentry') then
      E:=TDomElement(L);
    ConvertLogEntry(E);
    L:=L.NextSibling;
    end;
end;
 

{ TSVN2CVSApp }

function TSVN2CVSApp.RunCmd(Cmd: String; CmdOutput: TStream): Boolean;

Var
  Buf : Array[1..4096] of Byte;
  Count : Integer;

begin
  With TProcess.Create(Self) do
    Try
      CommandLine:=cmd;
      Writeln(Format(SExecuting,[CommandLine]));
      if (CmdOutput<>Nil) then
        Options:=[poUsePipes];
      Execute;
      If (CmdOutPut=Nil) then
        WaitOnExit
      else
        Repeat
          Count:=Output.Read(Buf,SizeOf(Buf));
          If (Count>0) then
            cmdOutput.Write(Buf,Count);
        Until (Count=0);
      Result:=(ExitStatus=0);
    finally
      Free;
    end;
end;

function TSVN2CVSApp.RunSVN(Cmd: String; CmdOutput: TStream): Boolean;


begin
  Result:=RunCmd(SVNbin+' '+Cmd,CmdOutput);
end;

function TSVN2CVSApp.RunCVS(Cmd: String; CmdOutput: TStream): Boolean;
begin
  Result:=RunCmd(CVSbin+' '+Cmd,CmdOutput);
end;

procedure TSVN2CVSApp.CheckOutSVN(Files : TStrings);

Var
  S : TStringStream;

begin
  S:=TStringStream.Create('');
  Try
    if not RunSVN(Format('co -r %d %s .',[StartRevision,SVNURL]),S) then
      Raise AppError.Create(SErrFailedToCheckOut);
    Files.Text:=S.DataString;
  Finally
    FreeAndNil(S);
  end;
end;

procedure TSVN2CVSApp.CheckInCVS;

Var
  F : Text;

begin
  If not ForceDirectories(WorkingDir+'CVS') then
  Try
    AssignFile(F,WorkingDir+'CVS/Root');
    Rewrite(F);
    Try
      Writeln(F,CVSRoot);
    Finally
      CloseFile(F);
    end;
    AssignFile(F,WorkingDir+'CVS/Repository');
    Rewrite(F);
    Try
      Writeln(F,CVSRepository);
    Finally
      Close(F);
    end;
    AssignFile(F,WorkingDir+'CVS/Entries');
    Rewrite(F);
    Try
      // Do nothing.
    Finally
      Close(F);
    end;
  except
    On E : Exception do
      begin
      E.Message:=SErrFailedToInitCVS+E.Message;
      Raise;
      end;
  end;
end;

procedure TSVN2CVSApp.Convertrepository;

Var
  InitCVS,INITSVN : Boolean;
  I : Integer;
  Files : TStringList;

begin
  If Not DirectoryExists(WorkingDir) then
    begin
    if Not ForceDirectories(WorkingDir) then
      Raise AppError.CreateFmt(SErrDirectoryFailed,[WorkingDir]);
    InitSVN:=True;
    InitCVS:=true;
    end
  else
    begin
    if Not DirectoryExists(WorkingDir+'.svn') then
      InitSVN:=True;
    if Not DirectoryExists(WorkingDir+'CVS') then
      InitCVS:=True;
    end;
  ChDir(WorkingDir);
  if InitCVS and (CVSRepository='') then
    Raise AppError.Create(SErrNoRepository);
  if InitSVN then
    begin
    Files:=TStringList.Create;
    Try
      CheckoutSVN(Files);
      if InitCVS then
        begin
        CheckinCVS;
        DoCVSEntries(Nil,Files);
        end
      else
        DoCVSEntries(Nil,Files);
    finally
      FreeAndNil(Files);
    end;
    end;
  GetVersions;
  For I:=0 to Versions.Count-1 do
    ConvertVersion(Versions[i]);
end;

procedure TSVN2CVSApp.GetVersions;

Var
  S : TStringStream;
  Doc : TXMLDocument;

begin
  Versions:=TVersions.Create(TVersion);
  S:=TStringStream.Create('');
  Try
    if not RunSVN(Format('log --xml -r %d:HEAD',[StartRevision]),S) then
      Raise AppError(SErrFailedToGetVersions);
    S.Position:=0;
    ReadXMLFile(Doc,S);
    Try
      Versions.LoadFromXML(Doc);
    finally
      Doc.Free;
    end;
  Finally
    S.Free;
  end;
end;


procedure TSVN2CVSApp.ConvertVersion(Version: TVersion);

Var
  Files : TStringList;

begin
  Writeln(Format(SConvertingRevision,[Version.revision]));
  Files:=TStringList.Create;
  Try
    If Not UpdateSVN(Version,Files) then
      Raise AppError.CreateFmt(SErrUpdateFailed,[Version.Revision]);
    DoCVSEntries(Version,Files);
  Finally
    Files.Free;
  end;
end;

Function TSVN2CVSApp.UpdateSVN(Version : TVersion; Files : TStrings) : Boolean;

Var
  S : TStringStream;

begin
  S:=TStringStream.Create('');
  Try
    Result:=RunSVN(Format('up -r %d',[version.revision]),S);
    if Result then
      Files.Text:=S.DataString;
  Finally
    S.Free;
  end;
end;

Procedure TSVN2CVSApp.WriteLogMessage(Version : TVersion);

Var
  F : Text;
  
begin
  AssignFile(F,'logmsg.txt');
  Rewrite(F);
  Try
    Writeln(F,Format(SLogRevision,[Version.Revision,Version.Author]));
    Writeln(F, Version.LogMessage);
  Finally
    CloseFile(F);
  end;
end;

Procedure TSVN2CVSApp.DoCVSEntries(Version : TVersion;Files : TStrings);

Var
  I,P        : Integer;
  Action   : Char;
  FileName : String;

begin
  For I:=0 to Files.Count-1 do
   begin
   FileName:=trim(Files[i]);
   P:=Pos(' ',FileName);
   if (P=0) then
     Writeln(StdErr,Format(SWarnErrorInLine,[FileName]))
   else
     begin
     Action:=FileName[1];
     system.Delete(FileName,1,P);
     FileName:=Trim(FileName);
     end;
   Case UpCase(action) of
     'U' : UpdateEntry(FileName);
     'D' : DeleteEntry(FileName);
   else
      Writeln(stdErr,Format(SWarnUnknownAction,[Action,FileName]));
   end;
   end;
 WriteLogMessage(version);
 Try
   If not RunCVS('commit -m -F logmsg.txt .',Nil) then
     Raise AppError.Create(SErrFailedToCommit);
 Finally
   if not DeleteFile('logmsg.txt') then
     Writeln(StdErr,'Warning: failed to remove log message file.');
 end;
end;

Procedure TSVN2CVSApp.UpdateEntry(AFileName : String);

Var
  FD : String;
  L : TStringList;
  I : Integer;
  Found : Boolean;
  
begin
  If ((FileGetAttr(AFileName) and faDirectory)<>0) then
    begin
    if Not RunCVS('add '+AFileName,Nil) then
      Raise AppError.CreateFmt(SErrFailedToAddDirectory,[AFileName]);
    end
  else // Check if file is under CVS control by checking the Entries file.
    begin
    FD:=ExtractFilePath(AFileName);
    If not DirectoryExists(FD+'Entries') then
      Raise AppError.CreateFmt(SErrDirectoryNotInCVS,[FD]);
    Found:=False;
    L:=TStringList.Create;
    Try
      L.LoadFromFile(FD+'Entries');
      Found:=False;
      I:=0;
      While (not found) and (I<L.Count) do
        begin
        Inc(I);
        end;
      if not found then
       if Not RunCVS('add '+AFileName,Nil) then
         Raise AppError.CreateFmt(SErrFailedToAddFile,[AFileName]);
    finally
      L.Free;
    end;
    end;
end;

Procedure TSVN2CVSApp.DeleteEntry(AFileName : String);

begin
  If ((FileGetAttr(AFileName) and faDirectory)=0) then
    if Not RunCVS('rm '+AFileName,Nil) then
      Raise AppError.CreateFmt(SErrFailedToRemove,[AFileName]);
end;

procedure TSVN2CVSApp.DoRun;

begin
  If Not ProcessArguments then
    exit;
  ConvertRepository;
end;

procedure TSVN2CVSApp.ProcessConfigFile;

begin
  With TMemIniFile.Create(GetAppConfigFile(False)) do
    try
      SVNURL:=ReadString(SGlobal,KeySVNURL,'');
      CVSROOT:=ReadString(SGlobal,KeyCVSROOT,'');
      CVSRepository:=ReadString(SGlobal,KeyRepository,'');
      WorkingDir:=ReadString(SGLobal,KeyWorkDir,'');
      StartRevision:=ReadInteger(SGlobal,KeyRevision,-1)+1;
      SVNBin:=ReadString(SGlobal,KeySVNBin,'svn');
      CVSBin:=ReadString(SGlobal,KeyCVSBin,'cvs');
    finally
      Free;
    end;
end;


function TSVN2CVSApp.ProcessArguments: Boolean;

begin
  ProcessConfigFile;
  if HasOption('s','svn-repository') then
    SVNURL:=GetOptionValue('s','svn-repository');
  if HasOption('c','cvsroot') then
    CVSROOT:=GetOptionValue('c','cvsroot');
  if HasOption('c','cvsrepository') then
    CVSROOT:=GetOptionValue('p','cvsrepository');
  if HasOption('r','revision') then
    StartRevision:=StrToIntDef(GetOptionValue('c'),0);
  if HasOption('d','directory') then
    WorkingDir:=GetOptionValue('d','directory');
  Result:=(SVNUrl<>'') and (CVSROOT<>'');
  If Result then
    begin
    If (WorkingDir='') then
      WorkingDir:=GetCurrentDir;
    WorkingDir:=IncludeTrailingPathDelimiter(WorkingDir);
    end;
end;

begin
  With TSVN2CVSApp.Create(Nil) do
    try
      Initialize;
      Run;
    Finally
      free;
    end;  
end.
