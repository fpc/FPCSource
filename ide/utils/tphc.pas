{
 !!! Someone please fix DRIVERS.PAS, so it doesn't clears the screen on exit
     when we didn't use any of it's functions, just had it in 'uses'

     Then we can delete GetDosTicks() from WHelp...
}

uses Objects,WUtils,WHelp,WTPHWriter;

const
     SrcExt          = '.txt';
     HelpExt         = '.fph';
     TokenPrefix     = '.';
     CommentPrefix   = ';';
     TokenIndex      = 'INDEX';
     TokenTopic      = 'TOPIC';
     TokenCode       = 'CODE';

     FirstTempTopic  = 1000000;

     CR              = #$0D;
     LF              = #$0A;

type
     THCIndexEntry = record
       Tag      : PString;
       TopicName: PString;
     end;

     THCTopic = record
       Name     : PString;
       Topic    : PTopic;
     end;

     PHCIndexEntryCollection = ^THCIndexEntryCollection;
     THCIndexEntryCollection = object(T

var SrcName, DestName: string;
    HelpFile        : THelpFileWriter;

procedure Print(const S: string);
begin
  writeln(S);
end;

procedure Abort; forward;

procedure Help;
begin
  Print('Syntax : TPHC <helpsource>[.TXT] <helpfile>[.FPH]');
  Abort;
end;

procedure Fatal(const S: string);
begin
  Print('Fatal: '+S);
  Abort;
end;

procedure Warning(const S: string);
begin
  Print('Warning: '+S);
end;

procedure ProcessParams;
begin
  if (ParamCount<1) or (ParamCount>2) then Help;
  SrcName:=ParamStr(1);
  if ExtOf(SrcName)='' then SrcName:=SrcName+SrcExt;
  if ParamCount=1 then
    DestName:=DirAndNameOf(SrcName)+HelpExt
  else
    begin
      DestName:=ParamStr(2);
      if ExtOf(DestName)='' then DestName:=DestName+HelpExt;
    end;
end;

procedure Compile(SrcS, DestS: PStream);
var CurLine: string;
    CurLineNo: longint;
    CurTopic : PTopic;
    HelpFile: PHelpFileWriter;
    InCode: boolean;
    NextTempTopic: longint;
procedure AddLine(const S: string);
begin
  if CurTopic<>nil then
    HelpFile^.AddLineToTopic(CurTopic,S);
end;
procedure ProcessToken(S: string);
var P: byte;
    Token: string;
    TopicName: string;
    TopicContext: THelpCtx;
    Text: string;
begin
  S:=Trim(S);
  P:=Pos(' ',S); if P=0 then P:=length(S)+1;
  Token:=UpcaseStr(copy(S,1,P-1)); Delete(S,1,P); S:=Trim(S);
  if Token=TokenIndex then
    begin
      if InCode then AddLine(hscCode);
      if copy(S,1,1)<>'{' then
        Fatal('"{" expected at line '+IntToStr(CurLineNo));
      if copy(S,length(S),1)<>'}' then
        Fatal('"}" expected at line '+IntToStr(CurLineNo));
      S:=copy(S,2,length(S)-2);
      P:=Pos(':',S); if P=0 then P:=length(S)+1;
      Text:=copy(S,1,!!
    end else
  if Token=TokenTopic then
    begin
      if InCode then AddLine(hscCode);
      P:=Pos(' ',S); if P=0 then P:=length(S)+1;
      TopicName:=UpcaseStr(copy(S,1,P-1)); Delete(S,1,P); S:=Trim(S);
      if TopicName='' then
        Fatal('Topic name missing at line '+IntToStr(CurLineNo));
      if S='' then
        TopicContext:=0
      else
        if copy(S,1,1)<>'=' then
          begin
            Fatal('"=" expected at line '+IntToStr(CurLineNo));
            TopicContext:=0;
          end
        else
          begin
            S:=Trim(copy(S,2,255));
            TopicContext:=StrToInt(S);
            if LastStrToIntResult<>0 then
              Fatal('Error interpreting context number at line '+IntToStr(CurLineNo));
          end;
      if TopicContext=0 then
        begin
          TopicContext:=NextTempTopic;
          Inc(NextTempTopic);
        end;
      CurTopic:=HelpFile^.CreateTopic(TopicContext);
    end else
  if Token=TokenCode then
    begin
      AddLine(hscCode);
      InCode:=not InCode;
    end else
  Warning('Uknown token "'+Token+'" encountered at line '+IntToStr(CurLineNo));
end;
procedure ProcessLine(const S: string);
begin
  AddLine(S);
end;
function ReadNextLine: boolean;
var C: char;
begin
  Inc(CurLineNo);
  CurLine:='';
  repeat
    SrcS^.Read(C,1);
    if (C in[CR,LF])=false then
      CurLine:=CurLine+C;
  until (C=LF) or (SrcS^.Status<>stOK);
  ReadNextLine:=(SrcS^.Status=stOK);
end;
var OK: boolean;
begin
  New(HelpFile, InitStream(DestS,0));
  CurTopic:=nil; CurLineNo:=0;
  NextTempTopic:=FirstTempTopic;
  InCode:=false;
  repeat
    OK:=ReadNextLine;
    if OK then
    if copy(CurLine,1,length(CommentPrefix))=CommentPrefix then
      { comment }
    else
    if copy(CurLine,1,length(TokenPrefix))=TokenPrefix then
      ProcessToken(copy(CurLine,2,255))
    else
    { normal help-text }
    begin
      ProcessLine(CurLine);
    end;
  until OK=false;
  if HelpFile^.WriteFile=false then
    Fatal('Error writing help file.');
  Dispose(HelpFile, Done);
end;

const SrcS  : PBufStream = nil;
      DestS : PBufStream = nil;

procedure Abort;
begin
  if SrcS<>nil then Dispose(SrcS, Done); SrcS:=nil;
  if DestS<>nil then Dispose(DestS, Done); DestS:=nil;
end;

BEGIN
  Print('þ Help Compiler  Version 0.9  Copyright (c) 1999 by B‚rczi G bor');
  ProcessParams;
  New(SrcS, Init(SrcName, stOpenRead, 4096));
  if (SrcS=nil) or (SrcS^.Status<>stOK) then
    Fatal('Error opening source file.');
  New(DestS, Init(DestName, stCreate, 4096));
  if (DestS=nil) or (DestS^.Status<>stOK) then
    Fatal('Error creating destination file.');
  Compile(SrcS,DestS);
END.
