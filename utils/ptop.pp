
Program PtoP;
{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Michael Van Canneyt, member of 
    the Free Pascal development team

    Pascal pretty print program
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Uses PtoPu,Objects,getopts;

Var 
  Infilename,OutFileName,ConfigFile : String;
  BeVerbose : Boolean;
  TheIndent,TheBufSize,TheLineSize : Integer;
  
Function StrToInt(Const S : String) : Integer;

Var Code : integer;

begin
  Val(S,StrToInt,Code);
  If Code<>0 then StrToInt:=0;
end;
  
Procedure Usage;

begin
  Writeln ('ptop : Usage : ');
  Writeln ('ptop [-v] [-i indent] [-b bufsize ][-c optsfile][-l linesize] infile outfile');
  Writeln ('     converts infile to outfile.');
  Writeln ('     -c : read options from optsfile');
  Writeln ('     -i : Set number of indent spaces.');
  Writeln ('     -l : Set maximum output linesize.');
  Writeln ('     -b : Use buffers of size bufsize');
  Writeln ('     -v : be verbose');
  writeln ('ptop -g ofile');
  writeln ('     generate default options file');
  Writeln ('ptop -h : This help');
  halt(0);
end;

Procedure Genopts;

Var S : PBufStream;

begin
  S:=New(PBufStream,Init(ConfigFile,stCreate,255));
  GeneratecfgFile(S);
  S^.Close;
  S^.Done;
end;

Procedure ProcessOpts;

Var c : char;

begin
  { Set defaults }
  Infilename:='';
  OutFileName:='';
  ConfigFile:=''; 
  TheIndent:=2;
  TheBufSize:=255;
  TheLineSize:=MaxLineSize;
  BeVerbose:=False;
  Repeat
    c:=getopt('i:c:g:b:hv');
    case c of 
      'i' : begin
            TheIndent:=StrToInt(OptArg);
            If TheIndent=0 then TheIndent:=2;
            end;
      'b' : begin
            TheBufSize:=StrToInt(OptArg);
            If TheBufSize=0 then TheBufSize:=255;
            end;
      'c' : ConfigFile:=OptArg;
      'l' : begin
            TheLineSize:=StrToInt(OptArg);
            If TheLineSIze=0 Then TheLineSize:=MaxLineSize;
            end;
      'g' : begin
            ConfigFIle:=OptArg;
            GenOpts;
            halt(0);
            end;
      'h' : usage;
      'v' : BeVerbose:=True;
    else
    end;
  until c=endofoptions;
  If optind<=paramcount then
    begin
    InFileName:=paramstr(OptInd);
    Inc(optind);
    If OptInd<=paramcount then
      OutFilename:=Paramstr(OptInd);
    end;
end; { Of ProcessOpts }

Var DiagS : PMemoryStream;
    InS,OutS,cfgS : PBufSTream;
    PPrinter : TPrettyPrinter;
    P : Pchar;
    i : longint;
    
begin
  ProcessOpts;
  If (Length(InfileName)=0) or (Length(OutFileName)=0) Then
    Usage;
  Ins:=New(PBufStream,Init(InFileName,StopenRead,TheBufSize));
  OutS:=New(PBufStream,Init(OutFileName,StCreate,TheBufSize));
  If BeVerbose then
    diagS:=New(PMemoryStream,Init(1000,255))
  else
    DiagS:=Nil;
  If ConfigFile<>'' then
    CfgS:=New(PBufStream,Init(ConfigFile,StOpenRead,TheBufSize))
  else 
    CfgS:=Nil;
  PPrinter.Create;
  PPrinter.Indent:=TheIndent;
  PPrinter.LineSize:=TheLineSize;
  PPrinter.Ins:=Ins;
  PPrinter.outS:=OutS;
  PPrinter.cfgS:=CfgS;
  PPrinter.DiagS:=DiagS;
  PPrinter.PrettyPrint;
  If Assigned(DiagS) then
    begin
    I:=DiagS^.GetSize;
    DiagS^.Seek(0);
    getmem (P,I+1);
    DiagS^.Read(P[0],I);
    P[I]:=#0;
    Writeln (stderr,P);
    Flush(stderr);
    DiagS^.Done;
    end;
  If Assigned(CfgS) then 
    CfgS^.Done;
  Ins^.Done;
  OutS^.Done;
end.

{
  $Log$
  Revision 1.3  2000-01-07 16:46:04  daniel
    * copyright 2000

  Revision 1.2  1999/07/08 21:17:10  michael
  + Made output linesize variable

  Revision 1.1  1999/05/12 16:11:39  peter
    * moved

  Revision 1.3  1999/03/25 16:52:29  michael
  + Implemented Delphi keywords and delphi comments

  Revision 1.2  1999/03/23 13:47:47  michael
  Added GPL and log

}
