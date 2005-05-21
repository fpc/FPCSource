{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    Windows message compiler unit.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$ifdef fpc}
{$mode objfpc}
{$h+}
{$endif}
unit msgcomp;

interface

Uses Classes;

Type
  TMessagehandler = Procedure(Sender : Tobject; Msg : String) Of Object;

  TMessageCompiler = Class
  Private
    FErrors : Integer;
    FEscapeNeeded : Boolean;
    FOnVerbose,
    FOnError : TMessageHandler;
    FCompiling : Boolean;
    FMC: TStream;
    FPas: TStream;
    FRC: TStream;
    FMsg: TStream;
    FLocaleID: Integer;
    FSubLocaleID: Integer;
    FMsgFileName: String;
    FUnitName: String;
    Procedure SWriteLn(S : String; Stream : TStream);
    Procedure CompileError(EMsg : String;Line : Integer; Value : String);
    Procedure ProcessMessages(Lines : TStrings; MsgList : Tlist);
    Procedure WriteMsgFile(MsgList : TList; Stream : TStream);
    Procedure WriteRCFile(MsgList : TList; Stream : TStream);
    Procedure WritePasFile(MsgList : TList; Stream : TStream);
    Procedure ClearList(MsgList : Tlist);
    procedure SetStream(const Index: Integer; const Value: TStream);
    Function  GetStream(const Index: Integer) : TStream;
    Procedure Verbose(Fmt : String;Args : Array of const);
  Public
    Constructor Create;
    Function Compile : Boolean;
    Property MC  : TStream index 1 Read GetStream Write SetStream;
    Property Msg : TStream index 2 Read GetStream Write SetStream;
    Property Pas : TStream index 3 Read GetStream Write SetStream;
    Property RC  : TStream Index 4 Read GetStream Write SetStream;
    Property LocaleID : Integer Read FLocaleID Write FlocaleID;
    Property SubLocaleID : Integer Read FSubLocaleID Write FSublocaleID;
    Property MessageFileName : String Read FMsgFileName Write FMsgFileName;
    Property UnitName : String Read FUnitName Write FUnitName;
    Property EscapeNeeded : Boolean Read FEscapeNeeded Write FEscapeNeeded;
    Property OnVerbose : TMessageHandler Read FOnVerbose Write FOnVerbose;
    Property OnError : TMessageHandler Read FOnError Write FOnError;
    Property Errors : Integer Read FErrors;
  End;

  TMessageEntry = Class(TObject)
    MessageID    : Cardinal;
    TotalMessageOfs  : Cardinal;
    MessageAlias,
    Language,
    MessageText  : String;
    Function OffsetToNext : Cardinal;
  End;

implementation

Uses SysUtils ;

Const
  SC = SizeOF(Cardinal);

Resourcestring
  SErrSetStreamNotAllowed = 'Setting stream during compile is not allowed.';
  SUnknownLine = 'Unknown error: "%s"';
  SNoMessage = 'Message starts without MessageID : "%s"';
  SErrUnknownDirective = 'Unknown directive "%s"';
  SErrInLine = 'Error: line %d: %s';
  SFoundMessageID = 'Found messageID : %s';
  SStartCompiling = 'Start compiling: %d lines.';
  SErrUnexpected = 'Unexpected error : %s';
  SWritingMessageFile = 'Writing %d entries to message file.';
  SWritingPasFile = 'Writing to unit "%s"';
  SWrotePasFile = 'Wrote %d constants to unit "%s"';
  SWritingRCFile = 'Writing rc file.';
  SErrNoNumber = 'Not a valid integer: %s';

procedure TMessageCompiler.ClearList(MsgList: Tlist);

Var
  I : Integer;

begin
  For I:=0 to MsgList.Count-1 do
    TMessageEntry(MsgList[I]).Free;
  MsgList.Clear;
end;

Function TMessageCompiler.Compile : Boolean;

Var
  Lines : TStrings;
  MsgList : TList;

Begin
  FErrors:=0;
  MsgList:=TList.Create;
  try
    Lines:=TStringList.Create;
    Try
      Lines.LoadFromStream(MC);
      ProcessMessages(Lines,MSgList);
      If (FErrors=0) then
        begin
        If Assigned(Msg) then
          WriteMsgFile(MsgList,Msg);
        if Assigned(Pas) then
          WritePasFile(MsgList,Pas);
        if Assigned(RC) then
          WriteRCFile(MsgList,RC);
        end;
    Finally
      Lines.Free;
    end;
  Finally
    ClearList(MsgList);
  end;
  Result:=(FErrors=0);
End;

Procedure TMessageCompiler.Verbose(Fmt : String;Args : Array of const);

begin
  if Assigned(FOnverbose) then
    FOnVerBose(Self,Format(Fmt,Args));
end;


Function HexToInt(Hex : String) : Integer;

Const HexSymbols : String = '0123456789ABCDEF';

Var I,J : Integer;

Begin
  Hex := UpperCase(Hex);
  Result := 0;
  J := Length(Hex);
  For I := 1 to J do
    Result := Result+((Pos(Hex[J-I+1],HexSymbols)-1) shl ((I-1)*4));
End;

Constructor TMessageCompiler.Create;

begin
  // English
  LocaleID:=9;
  SubLocaleID:=1;
end;


Procedure TMessageCompiler.CompileError(EMsg : String;Line : Integer; Value : String);

begin
  Inc(FErrors);
  EMsg:=Format(EMsg,[Value]);
  If Assigned(FOnError) then
    FOnError(Self,Format(SErrInLine,[Line,EMsg]));
end;

Procedure TMessageCompiler.ProcessMessages(Lines : TStrings; MsgList : TList);

Var
  Line : Integer;
  Me : TMessageEntry;

  Function Pad(S : String; Len : Integer) : String;
  Var I : Integer;
  Begin
    For I := Length(S) to Len-1 do S := S+' ';
    Result := S;
  End;

  Function SkipLine(Var S : String) : Boolean;

  Var
    I : Integer;

  begin
    I:=Pos(';',S);
    If (I<>0) then
      S:=Copy(S,1,I-1);
    Result:=Length(S)=0;
  end;

  Procedure DoDirective(S : String; I : Integer);

  Var
    MsgID : Integer;
    T : String;

  begin
    T := UpperCase(Copy(S,1,I-1));
    Delete(S,1,I);
    S:=Trim(S);
    If (T='MESSAGEID') Then
      begin
      Verbose(SFoundMessageID,[S]);
      S:=Uppercase(S);
      If Pos('0X',S)<>0 then
        begin
        Delete(S,1,2);
        MsgId:=HexToInt(S);
        end
      else
        MsgID:=StrToIntDef(S,-1);
      If (MsgID=-1) then
        CompileError(Format(SErrNoNumber,[S]),Line,T)
      else
        begin
        Me:=TMessageENtry.Create;
        Me.MessageID:=MsgID;
        end;
      End
    Else If (T = 'SYMBOLICNAME') Then
      Begin
      If Assigned(me) then
        Me.MessageAlias:=S;
      End
    Else If (T = 'LANGUAGE') Then
      begin
      If assigned(ME) then
        Me.Language:=S;
      end
    else
      CompileError(SErrUnknownDirective,Line,T);
   End;

Var
  I,Count : Integer;
  S : String;

Begin
  Count := Lines.Count-1;
  Verbose(SStartCOmpiling,[Count]);
  Line:=0;
  Me:=Nil;
  While Line<=Count do
    Begin
    Try
      S:=Lines[Line];
      If Not SkipLine(S) then
        begin
        I:=Pos('=',S);
        If (I<>0) then
          DoDirective(S,I)
        else
          If (Me=Nil) Then
            CompileError(SNoMessage,Line,S)
          else
            begin
            // Message starts.
            While (S<>'.') do
              begin
              If Length(Me.MessageText)>0 then
                Me.MessageText:=Me.MessageText+#13#10+S
              else
                Me.MessageText:=S;
              Inc(Line);
              If Line<=Count then
                S:=Lines[Line]
              end;
            MsgList.Add(Me);
            Me:=Nil;
            end;
        End;
    Except
      On E : Exception do
        CompileError(SErrUnexpected,Line,E.Message);
    End;
    Inc(Line);
    end;
End;

procedure TMessageCompiler.SetStream(const Index: Integer;
  const Value: TStream);
begin
  If FCompiling then
    Raise Exception.Create(SErrSetStreamNotAllowed);
  Case index of
   1 : FMC := Value;
   2 : FMsg := Value;
   3 : FPas := Value;
   4 : FRC := Value;
  end;
end;

Function TMessageCompiler.GetStream(const Index: Integer) : TStream;

begin
  Case index of
   1 : Result:=FMC;
   2 : Result:=FMsg;
   3 : Result:=FPas;
   4 : Result:=FRC;
  end;
end;

procedure TMessageCompiler.SWriteLn(S: String; Stream: TStream);
begin
  S:=S+#13#10;
  Stream.Write(S[1],Length(S));
end;

Procedure TMessageCompiler.WriteMSGFile(MsgList : Tlist; Stream : TStream);


Var
  I,Count : Integer;
  HeaderEntry,NullEntry: Array[1..3] of cardinal;
  O,BO  : Cardinal;
  M : TMessageEntry;
  S : String;

Begin
  Verbose(SWritingMessageFile,[MsgList.Count]);
  NullEntry[1]:=0;
  NullEntry[2]:=0;
  NullEntry[3]:=0;
  Count:=MsgList.Count;
  Stream.Write(Count,SC);
  BO:=((SC*3)*Count)+SC; // Header size...
  // Loop 1: Header entries.
  For I:=0 to Count-1 do
    begin
    M:=TMessageEntry(MsgList[I]);
    HeaderEntry[1]:=M.MessageID;
    HeaderEntry[2]:=M.MessageID;
    HeaderEntry[3]:=BO;
    BO:=BO+M.OffsetToNext;
    Stream.Write(HeaderEntry,SizeOf(HeaderEntry));
    end;
  For I:=0 to Count-1 do
    begin
    M:=TMessageEntry(MsgList[I]);
    O:=M.OffsetToNext;
    Stream.Write(O,SizeOf(O));
    Dec(O,SC);
    S:=M.MessageText;
    Stream.Write(S[1],Length(S));
    If (Length(S)<O) then
      Stream.Write(NullEntry,O-Length(S));
    end;
End;

procedure TMessageCompiler.WritePasFile(MsgList: TList; Stream: TStream);

Var
  I,Count : Integer;
  ME : TMessageEntry;

begin
  Verbose(SWritingPasFile,[UnitName]);
  SWriteln(Format('Unit %s;',[UnitName]),Stream);
  SWriteln('',Stream);
  SWriteln('Interface',Stream);
  SWriteln('',Stream);
  SWriteln('Const',Stream);
  Count:=0;
  For I:=0 to MsgList.Count-1 do
    begin
    Me:=TMessageEntry(MsgList[I]);
    With Me do
      If (MessageAlias<>'') then
        begin
        Swriteln(Format('  %s = %d; ',[MessageAlias,MessageID]),Stream);
        Inc(Count);
        end;
    end;
  SWriteln('',Stream);
  SWriteln('Implementation',Stream);
  SWriteln('',Stream);
  SWriteln('end.',Stream);
  Verbose(SWrotePasFile,[Count,UnitName]);
end;

procedure TMessageCompiler.WriteRCFile(MsgList: TList; Stream: TStream);

Const
  LangLine = 'LANGUAGE 0x%s,0x%s';
  FileLine = '1 11 "%s"';

Var
  S : String;

begin
  Verbose(SWritingRCFile,[]);
  S:=Format(LangLine,[IntToHex(LocaleID,1),IntToHex(SubLocaleID,1)]);
  SWriteLn(S,Stream);
  S:=MessageFileName;
  If EscapeNeeded Then
    S:=StringReplace(S,'\','\\',[rfReplaceAll]);
  SWriteLn(Format(FileLine,[S]),Stream);
end;

{ TMessageEntry }

function TMessageEntry.OffsetToNext: Cardinal;
begin
  Result:=((Length(MessageText) div SC) +2) * SC;
end;

end.
