{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    OS/2 event logging facility.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$R-}

const
  EventLogAvailable: boolean = false;

  No_Handle = cardinal ($FFFFFFFF);

  EventLogHandle: cardinal = No_Handle;

  sis_MMIOAddr = 0;
  sis_MEC_Table = 1;
  sis_Sys_Log = 2;

  lf_LogEnable = 1;     { Logging enabled }
  lf_LogAvailable = 2;  { Logging available }

  ErrLog_Service = 1;
  ErrLog_Version = 1;

  lf_Bit_ProcName = 1;    {used to indicate whether the current error log}
                          {entry packet contains space in which the error}
                          {logging facility can place a long process name}
                          {("on" indicates YES, "off" indicates NO)      }
  lf_Bit_Origin_256 = 2;  {used to indicate whether the current error log }
                          {entry packet contains an 8 byte originator name}
                          {or a 256 byte originator name ("on" indicates  }
                          {a 256 byte originator name, "off" indicates an }
                          {8 byte originator name)                        }
  lf_Bit_DateTime = 4;    {used to indicate that the caller has placed time}
                          {and date values in the Error Log entry packet   }
                          {and does not wish to have those values modified }
                          {during the logging process ("on" indicates that }
                          {the error log entry packet already contains time}
                          {and date values, "off" indicates the packet does}
                          {not already contain time and date values)       }
  lf_Bit_Suspend = 8;
  lf_Bit_Resume = 16;
  lf_Bit_Redirect = 32;
  lf_Bit_GetStatus = 64;
  lf_Bit_Register = 128;
  lf_Bit_Remote_Fail = 256;

  MaxDataSize = 3400;

type
  Str3 = string [3];

  TLogRecord = record
    Len: word;          { length of this record (including the Len field) }
    Rec_ID: word;       { record ID }
    Status: cardinal;   { record status bits (see lf_Bit_* constants) }
    Qualifier: array [1..4] of char;    { qualifier tag }
    Reserved: cardinal;
    Time: cardinal;     { hours, minutes, seconds, hundreds }
    Date: cardinal;     { day, month, year (stored as word) }
    case byte of
     0: (Data: array [1..MaxDataSize] of char);
     1: (Originator256: array [0..255] of char;
         ProcessName_O256: array [1..260] of char;
         FormatDLLName_O256_ProcName: array [1..12] of char;
         Data_O256_ProcName: array [1..MaxDataSize] of char);
     2: (Originator256b: array [0..255] of char;
         FormatDLLName_O256: array [1..12] of char;
         Data_O256: array [1..MaxDataSize] of char);
     3: (Originator8: array [0..7] of char;
         ProcessName_O8: array [1..260] of char;
         FormatDLLName_O8_ProcName: array [1..12] of char;
         Data_O8_ProcName: array [1..MaxDataSize] of char);
     4: (Originator8b: array [0..7] of char;
         FormatDLLName_O8: array [1..12] of char;
         Data_O8: array [1..MaxDataSize] of char);
  end;
  LogRecord = TLogRecord;
  PLogRecord = ^TLogRecord;

  TLogEntryRec = record
    Version: word;                      {this version is 1}
    Count: word;                        {number of log records in this buffer}
    LogRec: array [0..0] of TLogRecord; {repeated count times}
  end;
  LogEntryRec = TLogEntryRec;
  PLogEntryRec = ^TLogEntryRec;


function DosQueryRASInfo (Index: cardinal; var PBuffer: pointer): longint;
                                          cdecl; external 'DOSCALLS' index 112;

function LogOpen (var Handle: cardinal): longint; cdecl;
                                                 external 'DOSCALLS' index 430;

function LogClose (Handle: cardinal): longint; cdecl;
                                                 external 'DOSCALLS' index 431;

function LogAddEntries (Handle: cardinal; Service: cardinal;
      LogEntries: PLogEntryRec): longint; cdecl; external 'DOSCALLS' index 432;

function LogAddEntries (Handle: cardinal; Service: cardinal;
  var LogEntries: TLogEntryRec): longint; cdecl; external 'DOSCALLS' index 432;


function TEventLog.DefaultFileName: string;

begin
  Result := GetEnvironmentVariable ('TEMP');
  if Result = '' then
    begin
      Result := GetEnvironmentVariable ('TMP');
      if Result = '' then Result := ExpandFileName ('.');
    end;
  Result := Result + DirectorySeparator +
                        ChangeFileExt (ExtractFileName (ParamStr (0)), '.log');
end;


Resourcestring
  SErrNoSysLog = 'Could not open system log (error %d)';
  SErrLogFailed = 'Failed to log entry (error %d)';


procedure TEventLog.ActivateSystemLog;

var
  P: PWord;

begin
  CheckIdentification;
  DosQueryRASInfo (sis_Sys_Log, P);
  EventLogAvailable := P^ and (lf_LogAvailable or lf_LogEnable)
                                           = (lf_LogAvailable or lf_LogEnable);
  if not (EventLogAvailable) then
    ActivateFileLog
  else
    if EventLogHandle = No_Handle then
      LogOpen (EventLogHandle);
end;


procedure TEventLog.DeactivateSystemLog;

begin
  if EventLogAvailable then
    if EventLogHandle <> No_Handle then
      begin
        LogClose (EventLogHandle);
        EventLogHandle := No_Handle;
      end
  else
    DeactivateFileLog;
end;


procedure TEventLog.WriteSystemLog (EventType: TEventType; Msg: string);

const
  WinET: array [TEventType] of Str3 = ('USR', 'INF', 'WRN', 'ERR', 'DBG');

var
  P: PLogEntryRec;
  S: string;
  Cnt, TSize, DSize: cardinal;
  W: word;

begin
  if not (EventLogAvailable) then
    WriteFileLog (EventType, Msg)
  else
    begin
      S := Copy (Identification, 1, 256);
      TSize := Length (Msg);
      Cnt := Succ (Pred (TSize) div MaxDataSize);
      if Cnt > high (word) then
        begin
          Cnt := high (word);
          TSize := Cnt * MaxDataSize;
        end;
      DSize := TSize + 4 + Cnt * (24 + 256 + 260 + 12);
      GetMem (P, DSize);
      FillChar (P^, DSize, #0);
      with P^ do
        begin
          Version := ErrLog_Version;
          Count := Cnt;
          for W := 0 to Pred (Cnt) do
            with LogRec [W] do
              begin
                if (W = Pred (Cnt)) and (TSize mod MaxDataSize <> 0) then
                  begin
                    Len := 24 + 256 + 260 + 12 + TSize mod MaxDataSize;
                    Move (Msg [Succ (W * MaxDataSize)],
                                Data_O256_ProcName [1], TSize mod MaxDataSize);
                  end
                else
                  begin
                    Len := 24 + 256 + 260 + 12 + MaxDataSize;
                    Move (Msg [Succ (W * MaxDataSize)],
                                          Data_O256_ProcName [1], MaxDataSize);
                  end;
                Rec_ID := $4650; { FP }
                Status := lf_Bit_ProcName or lf_Bit_Origin_256;
                Move (WinET [EventType] [1], Qualifier,
                                                   Length (WinET [EventType]));
                Move (S [1], Originator256 [0], Length (S));
              end;
        end;
      LogAddEntries (EventLogHandle, ErrLog_Service, P);
      FreeMem (P, DSize);
    end;
end;

Function TEventLog.RegisterMessageFile(AFileName : String) : Boolean;

begin
  Result:=True;
end;

function TEventLog.MapTypeToCategory(EventType: TEventType): Word;
begin
  Result:=0;
  If (EventType=ETCustom) then
    DoGetCustomEventCategory(Result);
end;

function TEventLog.MapTypeToEventID(EventType: TEventType): DWord;

begin
  Result:=0;
  If (EventType=ETCustom) then
    DoGetCustomEventID(Result);
end;

function TEventLog.MapTypeToEvent(EventType: TEventType): DWord;

begin
  If EventType=etCustom Then
    begin
    Result:=CustomLogType;
    DoGetCustomEvent(Result);
    end
  else
    Result := ord (EventType);
end;

