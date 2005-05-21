{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    Free Pascal Message Compiler (command-line version)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$h+}
program fpmc;

uses msgcomp,getopts,sysutils,classes;


Type
  TFPMC = Class (TObject)
    CmdUnitName,
    InputFileName,
    MsgFileName,
    PascalFileName,
    RCFileName : String;
    LanguageID,
    SubLanguageID : Integer;
    CmdVerbose,
    CmdUseEscape : Boolean;
    Procedure DoError(Sender : TObject; Msg : String);
    Procedure DoVerbose(Sender : TObject; Msg : String);
    Procedure Usage(WithError : Boolean);
    Procedure HandleOptions;
    Procedure CompileFile;
  end;

Procedure TFPMC.DoError(Sender : TObject; Msg : String);

begin
  Writeln(StdErr,Msg);
end;

Procedure TFPMC.DoVerbose(Sender : TObject; Msg : String);

begin
  Writeln(Msg);
end;


Procedure TFPMC.Usage(WithError : Boolean);

begin
  Writeln('Usage : ',ExtractFileName(Paramstr(0)),' <option>');
  Writeln('Where options is one or more of :');
  Writeln(' -a           Produce all files (-m -p -r)');
  Writeln(' -e           Do not escape backslashes in filenames');
  Writeln(' -h           Usage screen');
  Writeln(' -i filename  Input filename');
  Writeln(' -l ID        Set locale (language ID)');
  Writeln(' -m [msgfile] Create message file.');
  Writeln(' -p [pasfile] Create pascal unit from message aliases.');
  Writeln(' -r [rcfile]  Create .RC file for use with message file');
  Writeln(' -s ID        Set sublocale (sublanguage)');
  Writeln(' -u name      Set unitname');
  Writeln(' -v           Be verbose');
  Writeln('Names of output files are deduced from input filename if needed.');
  Halt(Ord(WithError));
end;

Procedure TFPMC.Handleoptions;

Var
  C : Char;
  NeedPasFileName,
  NeedMsgFileName,
  NeedRCFileName : Boolean;

begin
  NeedPasFileName:=False;
  NeedMsgFileName:=False;
  NeedRCFileName :=False;
  CmdUnitName:='';
  LanguageID:=-1;
  SubLanguageID:=-1;
  CmdUseEscape:=True;
  repeat
    C:=GetOpt('vahei:m::l:s:u:r::p::');
    Case C of
      'a' : begin
            NeedMsgFilename:=(MsgFileName='');
            NeedPasFilename:=(PascalFileName='');
            NeedRCFilename:=(RCFileName='');
            end;
      'e' : CmdUseEscape:=False;
      'h','?' : Usage(false);
      'i' : InputFileName:=OptArg;
      'm' : begin
            MsgFileName:=OptArg;
            NeedMsgFilename:=(MsgFileName='');
            end;
      'l' : LanguageID:=StrToIntDef(OptArg,-1);
      'p' : begin
            PascalFileName:=OptArg;
            NeedPasFilename:=(PascalFileName='');
            end;
      's' : SubLanguageID:=StrToIntDef(OptArg,-1);
      'u' : CmdUnitName:=OptArg;
      'r' : begin
            RCFileName:=OptArg;
            NeedRCFilename:=(RCFileName='');
            end;
      'v' : CmdVerbose:=True;
    end;
  Until (C=EndOfOptions);
  If (InputFileName='') Then
    Usage(true);
  If NeedMsgFileName then
    MsgFileName:=ChangeFileExt(InputFilename,'.msg');
  If NeedPasFileName then
    PascalFileName:=ChangeFileExt(InputFilename,'.pp');
  If NeedRCFileName then
    RCFileName:=ChangeFileExt(InputFilename,'.rc');
  If (PascalFileName<>'') and (CmdUnitName='')  then
    CmdUnitName:=ChangeFileExt(ExtractFileName(PascalFileName),'');
end;

Procedure TFPMC.CompileFile;

Var
  M,P,R,I : TStream;

  Procedure SetupStreams;

  begin
    I:=TFileStream.Create(InputFileName,fmOpenRead);
    If (PascalFileName<>'') then
      P:=TFileStream.Create(PascalFileName,fmCreate);
    If (MsgFileName<>'') then
      M:=TFileStream.Create(MsgFileName,fmCreate);
    If (RCFileName<>'') then
      R:=TFileStream.Create(RCFileName,fmCreate);
  end;

  Procedure CloseStreams;

  begin
    M.Free;
    P.Free;
    R.Free;
    I.Free;
  end;

begin
  SetupStreams;
  Try
  With TMessageCompiler.Create do
    Try
      Msg:=M;
      MC:=I;
      RC:=R;
      Pas:=P;
      OnError:=@DoError;
      If CmdVerbose then
        OnVerbose:=@DoVerbose;
      UnitName:=CmdUnitName;
      MessageFileName:=MsgFileName;
      EscapeNeeded:=CmdUseEscape;
      If (LanguageID<>-1) then
        LocaleID:=LanguageID;
      If (SubLanguageID<>-1) then
        SubLocaleID:=SubLanguageID;
      Compile;
    Finally
      Free;
    end;
  Finally
    CloseStreams;
  end;
end;

begin
  With TFPMC.Create do
    Try
      HandleOptions;
      CompileFile;
    Finally
      Free;
    end;
end.
