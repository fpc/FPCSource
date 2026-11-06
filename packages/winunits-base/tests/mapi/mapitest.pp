{ Tests unit mapi. Without arguments: report the MAPI library and its entry points.
  "send ADDRESS [FILE]" or "sendw ADDRESS [FILE]": send a test mail with MAPISendMail or MAPISendMailW, with a dialog
  and FILE attached; "send-nodialog"/"sendw-nodialog" do the same without MAPI_DIALOG. }
program mapitest;

{$mode objfpc}{$H+}

uses
  SysUtils, Windows, mapi;

const
  cEntries: array[0..12] of AnsiString = ('MAPILogon', 'MAPILogoff', 'MAPISendMail', 'MAPISendMailW', 'MAPISendDocuments',
    'MAPIFindNext', 'MAPIReadMail', 'MAPISaveMail', 'MAPIDeleteMail', 'MAPIFreeBuffer', 'MAPIAddress', 'MAPIDetails', 'MAPIResolveName');


// Print the default mail client and the entry points of the loaded MAPI library
procedure Report;

var
  lLib: HMODULE;
  lEntry: AnsiString;

begin
  Writeln('pointer size: ', SizeOf(Pointer), ', SizeOf(MapiMessage)=', SizeOf(MapiMessage), ', SizeOf(LHANDLE)=', SizeOf(LHANDLE));
  Writeln('default mail client: "', DefaultMailClient, '"');
  Writeln('default mail client library: "', DefaultMailClientLibrary, '"');
  Writeln('InitMAPI(', MAPIDefaultLibrary, '): ', InitMAPI, ', MAPILoaded: ', MAPILoaded);
  lLib:=GetModuleHandleW(MAPIDefaultLibrary);
  for lEntry in cEntries do
    Writeln('  ', lEntry, ': ', GetProcAddress(lLib, PAnsiChar(lEntry))<>nil);
  FreeMAPI;
  Writeln('after FreeMAPI, MAPILoaded: ', MAPILoaded);
  Writeln('MAPIFreeBuffer(nil) loads the library again: result ', MAPIFreeBuffer(nil), ', MAPILoaded: ', MAPILoaded);
end;


// Send a test mail to aAddress with MAPISendMail, aFile attached if not empty
procedure SendAnsi(const aAddress, aFile: AnsiString; aFlags: ULONG);

var
  lMsg: MapiMessage;
  lRecip: MapiRecipDesc;
  lAttach: MapiFileDesc;
  lAddr, lName: AnsiString;

begin
  lAddr:='SMTP:'+aAddress;
  lRecip:=Default(MapiRecipDesc);
  lRecip.ulRecipClass:=MAPI_TO;
  lRecip.lpszName:=PAnsiChar(aAddress);
  lRecip.lpszAddress:=PAnsiChar(lAddr);
  lMsg:=Default(MapiMessage);
  lMsg.lpszSubject:='FPC mapi unit test (MAPISendMail)';
  lMsg.lpszNoteText:='Sent by mapitest with MAPISendMail.';
  lMsg.nRecipCount:=1;
  lMsg.lpRecips:=@lRecip;
  if aFile<>'' then
    begin
    lName:=ExtractFileName(aFile);
    lAttach:=Default(MapiFileDesc);
    lAttach.nPosition:=ULONG(-1);
    lAttach.lpszPathName:=PAnsiChar(aFile);
    lAttach.lpszFileName:=PAnsiChar(lName);
    lMsg.nFileCount:=1;
    lMsg.lpFiles:=@lAttach;
    end;
  Writeln('MAPISendMail: ', MAPISendMail(0, 0, @lMsg, aFlags, 0));
end;


// Send a test mail to aAddress with MAPISendMailW, aFile attached if not empty
procedure SendWide(const aAddress, aFile: UnicodeString; aFlags: ULONG);

var
  lMsg: MapiMessageW;
  lRecip: MapiRecipDescW;
  lAttach: MapiFileDescW;
  lAddr, lName: UnicodeString;

begin
  lAddr:='SMTP:'+aAddress;
  lRecip:=Default(MapiRecipDescW);
  lRecip.ulRecipClass:=MAPI_TO;
  lRecip.lpszName:=PWideChar(aAddress);
  lRecip.lpszAddress:=PWideChar(lAddr);
  lMsg:=Default(MapiMessageW);
  lMsg.lpszSubject:='FPC mapi unit test (MAPISendMailW) '#$00E9#$00E8#$20AC;
  lMsg.lpszNoteText:='Sent by mapitest with MAPISendMailW: '#$00E9#$00E8#$20AC;
  lMsg.nRecipCount:=1;
  lMsg.lpRecips:=@lRecip;
  if aFile<>'' then
    begin
    lName:=ExtractFileName(aFile);
    lAttach:=Default(MapiFileDescW);
    lAttach.nPosition:=ULONG(-1);
    lAttach.lpszPathName:=PWideChar(aFile);
    lAttach.lpszFileName:=PWideChar(lName);
    lMsg.nFileCount:=1;
    lMsg.lpFiles:=@lAttach;
    end;
  Writeln('MAPISendMailW: ', MAPISendMailW(0, 0, @lMsg, aFlags, 0));
end;


var
  lCmd, lFile: String;
  lFlags: ULONG;

begin
  lCmd:=ParamStr(1);
  lFile:='';
  if ParamCount>=3 then
    lFile:=ExpandFileName(ParamStr(3));
  lFlags:=MAPI_DIALOG or MAPI_LOGON_UI;
  if (lCmd='send-nodialog') or (lCmd='sendw-nodialog') then
    begin
    lFlags:=0;
    lCmd:=Copy(lCmd, 1, Pos('-', lCmd)-1);
    end;
  if (ParamCount>=2) and (lCmd='send') then
    SendAnsi(ParamStr(2), lFile, lFlags)
  else if (ParamCount>=2) and (lCmd='sendw') then
    SendWide(UnicodeString(ParamStr(2)), UnicodeString(lFile), lFlags)
  else
    Report;
end.
