{ Simple MAPI test provider: a mail client DLL that writes every message it is asked to send to a text file
  in %FPCMAPI_DIR% (default %TEMP%\fpcmapitest) instead of sending it. With FPCMAPI_DIALOG=1 a message box
  asks before "sending"; No returns MAPI_USER_ABORT. Register it as default mail client with regmapiprov. }
library fpcmapiprov;

{$mode objfpc}{$H+}

uses
  Windows, SysUtils, mapi;

const
  cSessionHandle = 1;

var
  MessageCount: Integer;


// The directory the messages are written to, created when missing
function OutputDir: UnicodeString;

begin
  Result:=GetEnvironmentVariable('FPCMAPI_DIR');
  if Result='' then
    Result:=IncludeTrailingPathDelimiter(GetTempDir(False))+'fpcmapitest';
  Result:=IncludeTrailingPathDelimiter(Result);
  ForceDirectories(Result);
end;


// aFlags as hex plus the names of the known MAPISendMail flags
function FlagsText(aFlags: ULONG): String;

begin
  Result:=Format('$%.8x', [aFlags]);
  if (aFlags and MAPI_LOGON_UI)<>0 then
    Result:=Result+' MAPI_LOGON_UI';
  if (aFlags and MAPI_NEW_SESSION)<>0 then
    Result:=Result+' MAPI_NEW_SESSION';
  if (aFlags and MAPI_DIALOG_MODELESS)=MAPI_DIALOG_MODELESS then
    Result:=Result+' MAPI_DIALOG_MODELESS'
  else if (aFlags and MAPI_DIALOG)<>0 then
    Result:=Result+' MAPI_DIALOG';
  if (aFlags and MAPI_FORCE_UNICODE)<>0 then
    Result:=Result+' MAPI_FORCE_UNICODE';
end;


// The label of recipient class aClass
function RecipClassText(aClass: ULONG): String;

begin
  case aClass of
    MAPI_ORIG: Result:='From';
    MAPI_TO: Result:='To';
    MAPI_CC: Result:='Cc';
    MAPI_BCC: Result:='Bcc';
  else
    Result:=Format('Class%d', [aClass]);
  end;
end;


// "aName <aAddress>", leaving out empty parts
function RecipText(const aName, aAddress: UnicodeString): UnicodeString;

begin
  Result:=aName;
  if aAddress<>'' then
    begin
    if Result<>'' then
      Result:=Result+' ';
    Result:=Result+'<'+aAddress+'>';
    end;
end;


// A line describing attachment aPath (shown as aName), with its size
function AttachmentText(const aPath, aName: UnicodeString; aPosition: ULONG): UnicodeString;

var
  lInfo: TSearchRec;

begin
  Result:=aPath;
  if aName<>'' then
    Result:=Result+' as "'+aName+'"';
  if aPosition<>ULONG(-1) then
    Result:=Result+Format(' at position %d', [aPosition]);
  if FindFirst(aPath, faAnyFile, lInfo)=0 then
    begin
    Result:=Result+Format(' (%d bytes)', [lInfo.Size]);
    FindClose(lInfo);
    end
  else
    Result:=Result+' (file not found)';
end;


// Ask the user to confirm the message aSummary; False when the user declines
function ConfirmSend(aUIParam: ULONG_PTR; const aSummary: UnicodeString): Boolean;

begin
  if GetEnvironmentVariable('FPCMAPI_DIALOG')<>'1' then
    Exit(True);
  Result:=MessageBoxW(HWND(aUIParam), PWideChar(aSummary+#13#10#13#10'Send this message?'),
    'FPC MAPI test provider', MB_YESNO or MB_ICONQUESTION)=IDYES;
end;


// Write the lines aLines to a new message file; True on success
function SaveMessage(aLines: TUnicodeStringArray): Boolean;

var
  lName: UnicodeString;
  lText: UTF8String;
  lFile: THandle;
  lIdx: Integer;

begin
  Inc(MessageCount);
  lName:=OutputDir+Format('msg-%s-%d-%d.txt', [FormatDateTime('yyyymmdd-hhnnsszzz', Now), GetCurrentProcessId, MessageCount]);
  lText:='';
  for lIdx:=0 to Length(aLines)-1 do
    lText:=lText+UTF8Encode(aLines[lIdx])+#13#10;
  lFile:=FileCreate(lName);
  Result:=lFile<>THandle(-1);
  if not Result then
    Exit;
  try
    Result:=FileWrite(lFile, PAnsiChar(lText)^, Length(lText))=Length(lText);
  finally
    FileClose(lFile);
  end;
end;


// Append aLine to aLines
procedure Add(var aLines: TUnicodeStringArray; const aLine: UnicodeString);

begin
  SetLength(aLines, Length(aLines)+1);
  aLines[High(aLines)]:=aLine;
end;


// The header lines of a message received through aFunction
function Header(const aFunction: String; aSession: LHANDLE; aFlags: ULONG): TUnicodeStringArray;

begin
  Result:=[];
  Add(Result, 'FPC MAPI test provider');
  Add(Result, 'Function: '+aFunction);
  Add(Result, 'Time: '+FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now));
  Add(Result, Format('Process: %d', [GetCurrentProcessId]));
  Add(Result, Format('Session: %d', [aSession]));
  Add(Result, 'Flags: '+FlagsText(aFlags));
end;


// Write the message and return the MAPI result for it
function Deliver(aUIParam: ULONG_PTR; const aLines: TUnicodeStringArray; const aSummary: UnicodeString): ULONG;

begin
  if not ConfirmSend(aUIParam, aSummary) then
    Exit(MAPI_USER_ABORT);
  if SaveMessage(aLines) then
    Result:=SUCCESS_SUCCESS
  else
    Result:=MAPI_E_FAILURE;
end;


// True if aSession is 0 or a session from MAPILogon
function ValidSession(aSession: LHANDLE): Boolean;

begin
  Result:=(aSession=0) or (aSession=cSessionHandle);
end;


// MAPILogon: return the single test session
function ProvLogon(aUIParam: ULONG_PTR; aProfileName, aPassword: LPSTR; aFlags: FLAGS; aReserved: ULONG; aSession: LPLHANDLE): ULONG; stdcall;

begin
  if aSession=nil then
    Exit(MAPI_E_FAILURE);
  aSession^:=cSessionHandle;
  Result:=SUCCESS_SUCCESS;
end;


// MAPILogoff: end the test session
function ProvLogoff(aSession: LHANDLE; aUIParam: ULONG_PTR; aFlags: FLAGS; aReserved: ULONG): ULONG; stdcall;

begin
  if aSession<>cSessionHandle then
    Exit(MAPI_E_INVALID_SESSION);
  Result:=SUCCESS_SUCCESS;
end;


// MAPISendMail: write the ANSI message to a file
function ProvSendMail(aSession: LHANDLE; aUIParam: ULONG_PTR; aMessage: lpMapiMessage; aFlags: FLAGS; aReserved: ULONG): ULONG; stdcall;

var
  lLines: TUnicodeStringArray;
  lRecip: lpMapiRecipDesc;
  lFile: lpMapiFileDesc;
  lIdx: Integer;

begin
  if not ValidSession(aSession) then
    Exit(MAPI_E_INVALID_SESSION);
  if aMessage=nil then
    Exit(MAPI_E_INVALID_MESSAGE);
  lLines:=Header('MAPISendMail', aSession, aFlags);
  Add(lLines, 'Subject: '+UnicodeString(AnsiString(aMessage^.lpszSubject)));
  Add(lLines, 'MessageType: '+UnicodeString(AnsiString(aMessage^.lpszMessageType)));
  if aMessage^.lpOriginator<>nil then
    Add(lLines, 'Originator: '+RecipText(UnicodeString(AnsiString(aMessage^.lpOriginator^.lpszName)),
      UnicodeString(AnsiString(aMessage^.lpOriginator^.lpszAddress))));
  lRecip:=aMessage^.lpRecips;
  for lIdx:=0 to Integer(aMessage^.nRecipCount)-1 do
    begin
    Add(lLines, RecipClassText(lRecip^.ulRecipClass)+': '+RecipText(UnicodeString(AnsiString(lRecip^.lpszName)),
      UnicodeString(AnsiString(lRecip^.lpszAddress))));
    Inc(lRecip);
    end;
  lFile:=aMessage^.lpFiles;
  for lIdx:=0 to Integer(aMessage^.nFileCount)-1 do
    begin
    Add(lLines, 'Attachment: '+AttachmentText(UnicodeString(AnsiString(lFile^.lpszPathName)),
      UnicodeString(AnsiString(lFile^.lpszFileName)), lFile^.nPosition));
    Inc(lFile);
    end;
  Add(lLines, 'Body:');
  Add(lLines, UnicodeString(AnsiString(aMessage^.lpszNoteText)));
  Result:=Deliver(aUIParam, lLines, 'Subject: '+UnicodeString(AnsiString(aMessage^.lpszSubject)));
end;


// MAPISendMailW: write the UTF-16 message to a file
function ProvSendMailW(aSession: LHANDLE; aUIParam: ULONG_PTR; aMessage: lpMapiMessageW; aFlags: FLAGS; aReserved: ULONG): ULONG; stdcall;

var
  lLines: TUnicodeStringArray;
  lRecip: lpMapiRecipDescW;
  lFile: lpMapiFileDescW;
  lIdx: Integer;

begin
  if not ValidSession(aSession) then
    Exit(MAPI_E_INVALID_SESSION);
  if aMessage=nil then
    Exit(MAPI_E_INVALID_MESSAGE);
  lLines:=Header('MAPISendMailW', aSession, aFlags);
  Add(lLines, 'Subject: '+UnicodeString(aMessage^.lpszSubject));
  Add(lLines, 'MessageType: '+UnicodeString(aMessage^.lpszMessageType));
  if aMessage^.lpOriginator<>nil then
    Add(lLines, 'Originator: '+RecipText(aMessage^.lpOriginator^.lpszName, aMessage^.lpOriginator^.lpszAddress));
  lRecip:=aMessage^.lpRecips;
  for lIdx:=0 to Integer(aMessage^.nRecipCount)-1 do
    begin
    Add(lLines, RecipClassText(lRecip^.ulRecipClass)+': '+RecipText(lRecip^.lpszName, lRecip^.lpszAddress));
    Inc(lRecip);
    end;
  lFile:=aMessage^.lpFiles;
  for lIdx:=0 to Integer(aMessage^.nFileCount)-1 do
    begin
    Add(lLines, 'Attachment: '+AttachmentText(lFile^.lpszPathName, lFile^.lpszFileName, lFile^.nPosition));
    Inc(lFile);
    end;
  Add(lLines, 'Body:');
  Add(lLines, UnicodeString(aMessage^.lpszNoteText));
  Result:=Deliver(aUIParam, lLines, 'Subject: '+UnicodeString(aMessage^.lpszSubject));
end;


// MAPISendDocuments: write the list of documents to a file
function ProvSendDocuments(aUIParam: ULONG_PTR; aDelimChar, aFilePaths, aFileNames: LPSTR; aReserved: ULONG): ULONG; stdcall;

var
  lLines: TUnicodeStringArray;
  lPaths, lNames: TStringArray;
  lDelim: AnsiChar;
  lName: UnicodeString;
  lIdx: Integer;

begin
  if (aDelimChar=nil) or (aFilePaths=nil) then
    Exit(MAPI_E_ATTACHMENT_NOT_FOUND);
  lDelim:=aDelimChar^;
  lPaths:=String(AnsiString(aFilePaths)).Split([lDelim]);
  lNames:=String(AnsiString(aFileNames)).Split([lDelim]);
  lLines:=Header('MAPISendDocuments', 0, 0);
  for lIdx:=0 to Length(lPaths)-1 do
    begin
    lName:='';
    if lIdx<Length(lNames) then
      lName:=UnicodeString(lNames[lIdx]);
    Add(lLines, 'Attachment: '+AttachmentText(UnicodeString(lPaths[lIdx]), lName, ULONG(-1)));
    end;
  Result:=Deliver(aUIParam, lLines, Format('%d document(s)', [Length(lPaths)]));
end;


// MAPIFreeBuffer: free a block returned by MAPIResolveName
function ProvFreeBuffer(aBuffer: LPVOID): ULONG; stdcall;

begin
  if aBuffer<>nil then
    FreeMem(aBuffer);
  Result:=SUCCESS_SUCCESS;
end;


// MAPIResolveName: resolve any name with an @ to an SMTP recipient
function ProvResolveName(aSession: LHANDLE; aUIParam: ULONG_PTR; aName: LPSTR; aFlags: FLAGS; aReserved: ULONG; aRecip: PlpMapiRecipDesc): ULONG; stdcall;

var
  lName, lAddress: AnsiString;
  lRecip: lpMapiRecipDesc;
  lText: PAnsiChar;

begin
  if not ValidSession(aSession) then
    Exit(MAPI_E_INVALID_SESSION);
  if (aName=nil) or (aRecip=nil) or (aName^=#0) then
    Exit(MAPI_E_UNKNOWN_RECIPIENT);
  lName:=aName;
  if Pos('@', lName)=0 then
    Exit(MAPI_E_UNKNOWN_RECIPIENT);
  lAddress:='SMTP:'+lName;
  { One block: the record, then the name and the address, freed with MAPIFreeBuffer. }
  lRecip:=GetMem(SizeOf(MapiRecipDesc)+Length(lName)+Length(lAddress)+2);
  lRecip^:=Default(MapiRecipDesc);
  lRecip^.ulRecipClass:=MAPI_TO;
  lText:=PAnsiChar(lRecip)+SizeOf(MapiRecipDesc);
  Move(PAnsiChar(lName)^, lText^, Length(lName)+1);
  lRecip^.lpszName:=lText;
  Inc(lText, Length(lName)+1);
  Move(PAnsiChar(lAddress)^, lText^, Length(lAddress)+1);
  lRecip^.lpszAddress:=lText;
  aRecip^:=lRecip;
  Result:=SUCCESS_SUCCESS;
end;


// MAPIFindNext: the test provider stores no messages
function ProvFindNext(aSession: LHANDLE; aUIParam: ULONG_PTR; aMessageType, aSeedMessageID: LPSTR; aFlags: FLAGS; aReserved: ULONG; aMessageID: LPSTR): ULONG; stdcall;

begin
  Result:=MAPI_E_NO_MESSAGES;
end;


// MAPIReadMail: the test provider stores no messages
function ProvReadMail(aSession: LHANDLE; aUIParam: ULONG_PTR; aMessageID: LPSTR; aFlags: FLAGS; aReserved: ULONG; aMessage: PlpMapiMessage): ULONG; stdcall;

begin
  Result:=MAPI_E_INVALID_MESSAGE;
end;


// MAPISaveMail: not supported
function ProvSaveMail(aSession: LHANDLE; aUIParam: ULONG_PTR; aMessage: lpMapiMessage; aFlags: FLAGS; aReserved: ULONG; aMessageID: LPSTR): ULONG; stdcall;

begin
  Result:=MAPI_E_NOT_SUPPORTED;
end;


// MAPIDeleteMail: the test provider stores no messages
function ProvDeleteMail(aSession: LHANDLE; aUIParam: ULONG_PTR; aMessageID: LPSTR; aFlags: FLAGS; aReserved: ULONG): ULONG; stdcall;

begin
  Result:=MAPI_E_INVALID_MESSAGE;
end;


// MAPIAddress: not supported
function ProvAddress(aSession: LHANDLE; aUIParam: ULONG_PTR; aCaption: LPSTR; aEditFields: ULONG; aLabels: LPSTR; aRecipCount: ULONG;
  aRecips: lpMapiRecipDesc; aFlags: FLAGS; aReserved: ULONG; aNewRecipCount: LPULONG; aNewRecips: PlpMapiRecipDesc): ULONG; stdcall;

begin
  Result:=MAPI_E_NOT_SUPPORTED;
end;


// MAPIDetails: not supported
function ProvDetails(aSession: LHANDLE; aUIParam: ULONG_PTR; aRecip: lpMapiRecipDesc; aFlags: FLAGS; aReserved: ULONG): ULONG; stdcall;

begin
  Result:=MAPI_E_NOT_SUPPORTED;
end;


exports
  ProvLogon name 'MAPILogon',
  ProvLogoff name 'MAPILogoff',
  ProvSendMail name 'MAPISendMail',
  ProvSendMailW name 'MAPISendMailW',
  ProvSendDocuments name 'MAPISendDocuments',
  ProvFindNext name 'MAPIFindNext',
  ProvReadMail name 'MAPIReadMail',
  ProvSaveMail name 'MAPISaveMail',
  ProvDeleteMail name 'MAPIDeleteMail',
  ProvFreeBuffer name 'MAPIFreeBuffer',
  ProvAddress name 'MAPIAddress',
  ProvDetails name 'MAPIDetails',
  ProvResolveName name 'MAPIResolveName';

end.
