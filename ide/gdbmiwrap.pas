{
    Copyright (c) 2015 by Nikolay Nikolov

    This unit provides a wrapper around GDB and implements parsing of
    the GDB/MI command result records.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit gdbmiwrap;

{$MODE objfpc}{$H+}
{$ASSERTIONS on}

{$I globdir.inc}

interface

uses
  SysUtils, Classes, GDBMIProc;

type
{$ifdef TARGET_IS_64BIT}
  { force 64bit if target compilation CPU is 64-bit address CPU }
  CORE_ADDR = Qword;
{$else}
  CORE_ADDR = PtrInt;
{$endif}

  TGDBMI_TupleValue = class;
  TGDBMI_ListValue = class;
  TGDBMI_Value = class
    function AsString: string;
    function AsLongInt: LongInt;
    function AsPtrInt: PtrInt;
    function AsTuple: TGDBMI_TupleValue;
    function AsList: TGDBMI_ListValue;
  end;

  { "C string\n" }
  TGDBMI_StringValue = class(TGDBMI_Value)
    FStringValue: string;
  public
    constructor Create(const S: string);
    property StringValue: string read FStringValue;
  end;

  (* {...} or [...] *)
  TGDBMI_TupleOrListValue = class(TGDBMI_Value)
  private
    FNames: array of string;
    FValues: array of TGDBMI_Value;
    function GetValue(const AName: string): TGDBMI_Value;
  public
    destructor Destroy; override;
    procedure Clear;
    procedure Add(AName: string; AValue: TGDBMI_Value);
    function HasNames: Boolean;
    function IsEmpty: Boolean;
    property Values [const AName: string]: TGDBMI_Value read GetValue; default;
  end;

  (* {} or {variable=value,variable=value,variable=value} *)
  TGDBMI_TupleValue = class(TGDBMI_TupleOrListValue)
  end;

  { [] or [value,value,value] or [variable=value,variable=value,variable=value] }
  TGDBMI_ListValue = class(TGDBMI_TupleOrListValue)
  private
    function GetCount: LongInt;
    function GetValueAt(AIndex: LongInt): TGDBMI_Value;
  public
    property Count: LongInt read GetCount;
    property ValueAt [AIndex: LongInt]: TGDBMI_Value read GetValueAt;
  end;

  TGDBMI_AsyncOutput = class
    FAsyncClass: string;
    FParameters: TGDBMI_TupleValue;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    property AsyncClass: string read FAsyncClass write FAsyncClass;
    property Parameters: TGDBMI_TupleValue read FParameters;
  end;

  TGDBMI_ResultRecord = class(TGDBMI_AsyncOutput)
  public
    function Success: Boolean;
  end;

  TGDBMI_AsyncOutput_List = array of TGDBMI_AsyncOutput;

  TGDBWrapper = class
  private
    FProcess: TGDBProcess;
    FRawResponse: TStringList;
    FConsoleStream: TStringList;
    FExecAsyncOutput: TGDBMI_AsyncOutput;
    FResultRecord: TGDBMI_ResultRecord;

    function IsAlive: Boolean;
    procedure ReadResponse;
  public
    NotifyAsyncOutput: TGDBMI_AsyncOutput_List;

    constructor Create;
    destructor Destroy; override;
    procedure Command(S: string);
    procedure WaitForProgramStop;
    property RawResponse: TStringList read FRawResponse;
    property ConsoleStream: TStringList read FConsoleStream;
    property ExecAsyncOutput: TGDBMI_AsyncOutput read FExecAsyncOutput;
    property ResultRecord: TGDBMI_ResultRecord read FResultRecord write FResultRecord;
    property Alive: Boolean read IsAlive;
  end;

function QuoteString(S: string): string;

implementation

function QuoteString(S: string): string;
var
  I: LongInt;
begin
  I := 1;
  Result := '';
  while I <= Length(S) do
  begin
    case S[I] of
      '''': Result := Result + '\''';
      '"':  Result := Result + '\"';
      #10:  Result := Result + '\n';
      #13:  Result := Result + '\r';
      #9:   Result := Result + '\t';
      #11:  Result := Result + '\v';
      #8:   Result := Result + '\b';
      #12:  Result := Result + '\f';
      #7:   Result := Result + '\a';
      '\':  Result := Result + '\\';
      '?':  Result := Result + '\?';
      else
        Result := Result + S[I];
    end;
    Inc(I);
  end;
  Result := '"' + Result + '"';
end;

function TGDBMI_Value.AsString: string;
begin
  Result := (self as TGDBMI_StringValue).StringValue;
end;

function TGDBMI_Value.AsLongInt: LongInt;
begin
  Result := StrToInt(AsString);
end;

function TGDBMI_Value.AsPtrInt: PtrInt;
begin
{$ifdef CPU64}
  Result := StrToInt64(AsString);
{$else}
  Result := StrToInt(AsString);
{$endif}
end;

function TGDBMI_Value.AsTuple: TGDBMI_TupleValue;
begin
  Result := self as TGDBMI_TupleValue;
end;

function TGDBMI_Value.AsList: TGDBMI_ListValue;
begin
  Result := self as TGDBMI_ListValue;
end;

constructor TGDBMI_StringValue.Create(const S: string);
begin
  FStringValue := S;
end;

destructor TGDBMI_TupleOrListValue.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TGDBMI_TupleOrListValue.Clear;
var
  I: LongInt;
begin
  SetLength(FNames, 0);
  for I := Low(FValues) to High(FValues) do
    FreeAndNil(FValues[I]);
  SetLength(FValues, 0);
end;

procedure TGDBMI_TupleOrListValue.Add(AName: string; AValue: TGDBMI_Value);
begin
  Assert(AValue <> nil);
  Assert(IsEmpty or (HasNames = (AName <> '')));
  if AName <> '' then
  begin
    SetLength(FNames, Length(FNames) + 1);
    FNames[Length(FNames) - 1] := AName;
  end;
  SetLength(FValues, Length(FValues) + 1);
  FValues[Length(FValues) - 1] := AValue;
end;

function TGDBMI_TupleOrListValue.HasNames: Boolean;
begin
  Result := Length(FNames) > 0;
end;

function TGDBMI_TupleOrListValue.IsEmpty: Boolean;
begin
  Result := Length(FValues) = 0;
end;

function TGDBMI_TupleOrListValue.GetValue(const AName: string): TGDBMI_Value;
var
  I: LongInt;
begin
  for I := Low(FNames) to High(FNames) do
    if FNames[I] = AName then
    begin
      Result := FValues[I];
      exit;
    end;
  Result := nil;
end;

function TGDBMI_ListValue.GetCount: LongInt;
begin
  Result := Length(FValues);
end;

function TGDBMI_ListValue.GetValueAt(AIndex: LongInt): TGDBMI_Value;
begin
  Assert((AIndex >= Low(FValues)) and (AIndex <= High(FValues)));
  Result := FValues[AIndex];
end;

constructor TGDBMI_AsyncOutput.Create;
begin
  FParameters := TGDBMI_TupleValue.Create;
end;

destructor TGDBMI_AsyncOutput.Destroy;
begin
  FParameters.Free;
  inherited Destroy;
end;

procedure TGDBMI_AsyncOutput.Clear;
begin
  AsyncClass := '';
  Parameters.Clear;
end;

function TGDBMI_ResultRecord.Success: Boolean;
begin
  { according to the GDB docs, 'done' and 'running' should be treated identically by clients }
  Result := (AsyncClass='done') or (AsyncClass='running');
end;

function ParseCString(const CStr: string; var NextCharPos: LongInt): string;
begin
  if (NextCharPos <= Length(CStr)) and (CStr[NextCharPos] = '"') then
    Inc(NextCharPos);
  Result := '';
  while NextCharPos <= Length(CStr) do
  begin
    if CStr[NextCharPos] = '"' then
    begin
      Inc(NextCharPos);
      exit;
    end
    else if CStr[NextCharPos] = '\' then
    begin
      Inc(NextCharPos);
      if NextCharPos <= Length(CStr) then
        case CStr[NextCharPos] of
          '''': Result := Result + '''';
          '"': Result := Result + '"';
          'n': Result := Result + #10;
          'r': Result := Result + #13;
          't': Result := Result + #9;
          'v': Result := Result + #11;
          'b': Result := Result + #8;
          'f': Result := Result + #12;
          'a': Result := Result + #7;
          '\': Result := Result + '\';
          '?': Result := Result + '?';
          {\0, \000, \xhhh}
        end;
    end
    else
      Result := Result + CStr[NextCharPos];
    Inc(NextCharPos);
  end;
end;

function ParseIdentifier(const S: string; var NextCharPos: LongInt): string;
begin
  Result := '';
  while (NextCharPos <= Length(S)) and (S[NextCharPos] in ['A'..'Z', 'a'..'z', '0'..'9', '-']) do
  begin
    Result := Result + S[NextCharPos];
    Inc(NextCharPos);
  end;
end;

function ParseValue(const S: string; var NextCharPos: LongInt): TGDBMI_Value;
var
  CStr: string;
  Tuple: TGDBMI_TupleValue;
  List: TGDBMI_ListValue;

  Name: string;
  Value: TGDBMI_Value;
begin
  Assert(NextCharPos <= Length(S));
  case S[NextCharPos] of
    '"':
      begin
        CStr := ParseCString(S, NextCharPos);
        Result := TGDBMI_StringValue.Create(CStr);
      end;
    '{':
      begin
        Inc(NextCharPos);
        Assert(NextCharPos <= Length(S));
        Tuple := TGDBMI_TupleValue.Create;
        Result := Tuple;
        while (NextCharPos <= Length(S)) and (S[NextCharPos] <> '}') do
        begin
          Name := ParseIdentifier(S, NextCharPos);
          Assert(NextCharPos <= Length(S));
          Assert(S[NextCharPos] = '=');
          Inc(NextCharPos);
          Value := ParseValue(S, NextCharPos);
          Tuple.Add(Name, Value);
          Assert(NextCharPos <= Length(S));
          Assert(S[NextCharPos] in [',', '}']);
          if S[NextCharPos] = ',' then
            Inc(NextCharPos);
        end;
        if (NextCharPos <= Length(S)) and (S[NextCharPos] = '}') then
          Inc(NextCharPos);
      end;
    '[':
      begin
        Inc(NextCharPos);
        Assert(NextCharPos <= Length(S));
        List := TGDBMI_ListValue.Create;
        Result := List;
        if S[NextCharPos] in ['"', '{', '['] then
        begin
          { list of values, no names }
          while (NextCharPos <= Length(S)) and (S[NextCharPos] <> ']') do
          begin
            Value := ParseValue(S, NextCharPos);
            List.Add('', Value);
            Assert(NextCharPos <= Length(S));
            Assert(S[NextCharPos] in [',', ']']);
            if S[NextCharPos] = ',' then
              Inc(NextCharPos);
          end;
        end
        else
        begin
          { list of name=value pairs (like a tuple) }
          while (NextCharPos <= Length(S)) and (S[NextCharPos] <> ']') do
          begin
            Name := ParseIdentifier(S, NextCharPos);
            Assert(NextCharPos <= Length(S));
            Assert(S[NextCharPos] = '=');
            Inc(NextCharPos);
            Value := ParseValue(S, NextCharPos);
            List.Add(Name, Value);
            Assert(NextCharPos <= Length(S));
            Assert(S[NextCharPos] in [',', ']']);
            if S[NextCharPos] = ',' then
              Inc(NextCharPos);
          end;
        end;
        if (NextCharPos <= Length(S)) and (S[NextCharPos] = ']') then
          Inc(NextCharPos);
      end;
    else
      Assert(False);
  end;
end;

procedure ParseAsyncOutput(const S: string; AsyncOutput: TGDBMI_AsyncOutput; var NextCharPos: LongInt);
var
  Name: string;
  Value: TGDBMI_Value;
begin
  AsyncOutput.Clear;
  AsyncOutput.AsyncClass := ParseIdentifier(S, NextCharPos);
  while NextCharPos <= Length(S) do
  begin
    Assert(S[NextCharPos] = ',');
    Inc(NextCharPos);
    Name := ParseIdentifier(S, NextCharPos);
    Assert(NextCharPos <= Length(S));
    Assert(S[NextCharPos] = '=');
    Inc(NextCharPos);
    Value := ParseValue(S, NextCharPos);
    AsyncOutput.Parameters.Add(Name, Value);
  end;
end;

function TGDBWrapper.IsAlive: Boolean;
begin
  Result := Assigned(FProcess) and FProcess.Alive;
end;

procedure TGDBWrapper.ReadResponse;
var
  S: string;
  I: LongInt;
  NextCharPos: LongInt;
  NAO: TGDBMI_AsyncOutput;
begin
  FRawResponse.Clear;
  FConsoleStream.Clear;
  ExecAsyncOutput.Clear;
  for I := Low(NotifyAsyncOutput) to High(NotifyAsyncOutput) do
    FreeAndNil(NotifyAsyncOutput[I]);
  SetLength(NotifyAsyncOutput, 0);
  if not FProcess.Alive then
    exit;
  repeat
    S := FProcess.GDBReadLn;
    FRawResponse.Add(S);
    if Length(S) >= 1 then
      case S[1] of
        '~':
          begin
            NextCharPos := 2;
            FConsoleStream.Add(ParseCString(S, NextCharPos));
          end;
        '*':
          begin
            NextCharPos := 2;
            ParseAsyncOutput(S, ExecAsyncOutput, NextCharPos);
          end;
        '^':
          begin
            NextCharPos := 2;
            ParseAsyncOutput(S, ResultRecord, NextCharPos);
          end;
        '=':
          begin
            NextCharPos := 2;
            NAO := TGDBMI_AsyncOutput.Create;
            try
              ParseAsyncOutput(S, NAO, NextCharPos);
              SetLength(NotifyAsyncOutput, Length(NotifyAsyncOutput) + 1);
              NotifyAsyncOutput[Length(NotifyAsyncOutput) - 1] := NAO;
              NAO := nil;
            finally
              NAO.Free;
            end;
          end;
      end;
  until (S = '(gdb) ') or (S = '(gdb)') or not FProcess.Alive;
end;

constructor TGDBWrapper.Create;
begin
  FRawResponse := TStringList.Create;
  FConsoleStream := TStringList.Create;
  FProcess := TGDBProcess.Create;
  FExecAsyncOutput := TGDBMI_AsyncOutput.Create;
  FResultRecord := TGDBMI_ResultRecord.Create;
  ReadResponse;
end;

destructor TGDBWrapper.Destroy;
begin
  if Alive then
    Command('-gdb-exit');
  FProcess.Free;
  FResultRecord.Free;
  FExecAsyncOutput.Free;
  FConsoleStream.Free;
  FRawResponse.Free;
end;

procedure TGDBWrapper.Command(S: string);
begin
  FProcess.GDBWriteLn(S);
  ReadResponse;
end;

procedure TGDBWrapper.WaitForProgramStop;
begin
  repeat
    ReadResponse;
  until (ExecAsyncOutput.AsyncClass = 'stopped') or not FProcess.Alive;
end;

end.
