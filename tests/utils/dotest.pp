{
  $Id$
    This file is part of the Free Pascal test suite.
    Copyright (c) 1999-2002 by the Free Pascal development team.

    This program makes the compilation and
    execution of individual test sources.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

program dotest;
uses
  dos,
  teststr,
  redir;

const
{$ifdef UNIX}
  ExeExt='';
{$else UNIX}
  ExeExt='exe';
{$endif UNIX}

type
  TVerboseLevel=(V_Abort,V_Error,V_Warning,V_Normal,V_Debug);

  TConfig = record
    NeedOptions,
    NeedCPU,
    NeedVersion,
    KnownRunNote  : string;
    ResultCode    : longint;
    KnownRunError : longint;
    NeedRecompile : boolean;
    IsInteractive : boolean;
    IsKnown       : boolean;
    NoRun         : boolean;
    UsesGraph     : boolean;
    ShouldFail    : boolean;
    Category      : string;
  end;

var
  Config : TConfig;
  CompilerBin : string;
  CompilerCPU : string;
  CompilerVersion : string;
  PPFile : string;
  PPFileInfo : string;
  TestName : string;
  Note : string;

const
  LongLogfile : string[32] = 'longlog';
  FailLogfile : string[32] = 'faillist';
  DoVerbose : boolean = false;
  DoGraph : boolean = false;
  DoInteractive : boolean = false;
  DoExecute : boolean = false;
  DoKnown : boolean = false;
  DoAll : boolean = false;
  DoUsual : boolean = true;

procedure Verbose(lvl:TVerboseLevel;const s:string);
begin
  case lvl of
    V_Normal :
      writeln(s);
    V_Debug :
      if DoVerbose then
       writeln('Debug: ',s);
    V_Warning :
      writeln('Warning: ',s);
    V_Error :
      begin
        writeln('Error: ',s);
        halt(1);
      end;
    V_Abort :
      begin
        writeln('Abort: ',s);
        halt(0);
      end;
  end;
end;


Function FileExists (Const F : String) : Boolean;
{
  Returns True if the file exists, False if not.
}
Var
  info : searchrec;
begin
  FindFirst (F,anyfile,Info);
  FileExists:=DosError=0;
  FindClose (Info);
end;


function ToStr(l:longint):string;
var
  s : string;
begin
  Str(l,s);
  ToStr:=s;
end;

function ToStrZero(l:longint;nbzero : byte):string;
var
  s : string;
begin
  Str(l,s);
  while length(s)<nbzero do
    s:='0'+s;
  ToStrZero:=s;
end;


procedure SetPPFileInfo;
Var
  info : searchrec;
  dt : DateTime;
begin
  FindFirst (PPFile,anyfile,Info);
  If DosError=0 then
    begin
      UnpackTime(info.time,dt);
      PPFileInfo:=PPFile+' '+ToStr(dt.year)+'/'+ToStrZero(dt.month,2)+'/'+
        ToStrZero(dt.day,2)+' '+ToStrZero(dt.Hour,2)+':'+ToStrZero(dt.min,2)+':'+ToStrZero(dt.sec,2);
    end
  else
    PPFileInfo:=PPfile;
  FindClose (Info);
end;



procedure TrimB(var s:string);
begin
  while (s<>'') and (s[1] in [' ',#9]) do
   delete(s,1,1);
end;


procedure TrimE(var s:string);
begin
  while (s<>'') and (s[length(s)] in [' ',#9]) do
   delete(s,length(s),1);
end;


function upper(const s : string) : string;
var
  i  : longint;
begin
  for i:=1 to length(s) do
   if s[i] in ['a'..'z'] then
    upper[i]:=char(byte(s[i])-32)
   else
    upper[i]:=s[i];
  upper[0]:=s[0];
end;


function SplitPath(const s:string):string;
var
  i : longint;
begin
  i:=Length(s);
  while (i>0) and not(s[i] in ['/','\']) do
   dec(i);
  SplitPath:=Copy(s,1,i);
end;


function ForceExtension(Const HStr,ext:String):String;
{
  Return a filename which certainly has the extension ext
}
var
  j : longint;
begin
  j:=length(Hstr);
  while (j>0) and (Hstr[j]<>'.') do
   dec(j);
  if j=0 then
   j:=255;
  if Ext<>'' then
   ForceExtension:=Copy(Hstr,1,j-1)+'.'+Ext
  else
   ForceExtension:=Copy(Hstr,1,j-1);
end;


procedure Copyfile(const fn1,fn2:string;append:boolean);
const
  bufsize = 16384;
var
  f,g : file;
  i   : longint;
  buf : pointer;
begin
  if Append then
   Verbose(V_Debug,'Appending '+fn1+' to '+fn2)
  else
   Verbose(V_Debug,'Copying '+fn1+' to '+fn2);
  assign(f,fn1);
  assign(g,fn2);
  {$I-}
   reset(f,1);
  {$I+}
  if ioresult<>0 then
   Verbose(V_Error,'Can''t open '+fn1);
  if append then
   begin
     {$I-}
      reset(g,1);
     {$I+}
     if ioresult<>0 then
      append:=false
     else
      seek(g,filesize(g));
   end;
  if not append then
   begin
     {$I-}
      rewrite(g,1);
     {$I+}
     if ioresult<>0 then
      Verbose(V_Error,'Can''t open '+fn2+' for output');
   end;
  getmem(buf,bufsize);
  repeat
    blockread(f,buf^,bufsize,i);
    blockwrite(g,buf^,i);
  until i<bufsize;
  freemem(buf,bufsize);
  close(f);
  close(g);
end;


procedure AddLog(const logfile,s:string);
var
  t : text;
begin
  assign(t,logfile);
  {$I-}
   append(t);
  {$I+}
  if ioresult<>0 then
   begin
     {$I-}
      rewrite(t);
     {$I+}
     if ioresult<>0 then
      Verbose(V_Abort,'Can''t append to '+logfile);
   end;
  writeln(t,s);
  close(t);
end;


function GetConfig(const fn:string;var r:TConfig):boolean;
var
  t : text;
  part,code : integer;
  l : longint;
  s,res : string;

  function GetEntry(const entry:string):boolean;
  var
    i : longint;
  begin
    Getentry:=false;
    Res:='';
    if Upper(Copy(s,1,length(entry)))=Upper(entry) then
     begin
       Delete(s,1,length(entry));
       TrimB(s);
       if (s<>'') then
        begin
          if (s[1]='=') then
           begin
             delete(s,1,1);
             i:=pos('}',s);
             if i=0 then
              i:=255
             else
              dec(i);
             res:=Copy(s,1,i);
             TrimB(res);
             TrimE(res);
           end;
          Verbose(V_Debug,'Config: '+Entry+' = "'+Res+'"');
          GetEntry:=true;
        end;
     end;
  end;

begin
  FillChar(r,sizeof(r),0);
  GetConfig:=false;
  Verbose(V_Debug,'Reading '+fn);
  assign(t,fn);
  {$I-}
   reset(t);
  {$I+}
  if ioresult<>0 then
   begin
     Verbose(V_Error,'Can''t open '+fn);
     exit;
   end;
  Note:='';
  while not eof(t) do
   begin
     readln(t,s);
     if s<>'' then
      begin
        if s[1]='{' then
         begin
           delete(s,1,1);
           TrimB(s);
           if (s<>'') and (s[1]='%') then
            begin
              delete(s,1,1);
              if GetEntry('OPT') then
               r.NeedOptions:=res
              else
               if GetEntry('CPU') then
                r.NeedCPU:=res
              else
               if GetEntry('VERSION') then
                r.NeedVersion:=res
              else
               if GetEntry('RESULT') then
                Val(res,r.ResultCode,code)
              else
               if GetEntry('GRAPH') then
                r.UsesGraph:=true
              else
               if GetEntry('FAIL') then
                r.ShouldFail:=true
              else
               if GetEntry('RECOMPILE') then
                r.NeedRecompile:=true
              else
               if GetEntry('NORUN') then
                r.NoRun:=true
              else
               if GetEntry('KNOWNRUNERROR') then
                begin
                  if res<>'' then
                    begin
                      val(res,l,code);
                      if code>1 then
                        begin
                          part:=code;
                          val(copy(res,1,code-1),l,code);
                          delete(res,1,part);
                        end;
                      if code=0 then
                        r.KnownRunError:=l;
                      if res<>'' then
                        r.KnownRunNote:=res;
                    end;
                end
              else
               if GetEntry('KNOWN') then
                 r.IsKnown:=true
              else
               if GetEntry('INTERACTIVE') then
                r.IsInteractive:=true
              else
               if GetEntry('NOTE') then
                begin
                  Note:='Note: '+res;
                  Verbose(V_Normal,Note);
                end
              else
               Verbose(V_Error,'Unknown entry: '+s);
            end;
         end
        else
         break;
      end;
   end;
  close(t);
  GetConfig:=true;
end;


function GetCompilerVersion:boolean;
var
  t : text;
begin
  GetCompilerVersion:=false;
  ExecuteRedir(CompilerBin,'-iV','','out','');
  assign(t,'out');
  {$I-}
   reset(t);
   readln(t,CompilerVersion);
   close(t);
   erase(t);
  {$I+}
  if ioresult<>0 then
   Verbose(V_Error,'Can''t get Compiler Version')
  else
   begin
     Verbose(V_Debug,'Current Compiler Version: '+CompilerVersion);
     GetCompilerVersion:=true;
   end;
end;


function GetCompilerCPU:boolean;
var
  t : text;
begin
  GetCompilerCPU:=false;
  ExecuteRedir(CompilerBin,'-iTP','','out','');
  assign(t,'out');
  {$I-}
   reset(t);
   readln(t,CompilerCPU);
   close(t);
   erase(t);
  {$I+}
  if ioresult<>0 then
   Verbose(V_Error,'Can''t get Compiler CPU Target')
  else
   begin
     Verbose(V_Debug,'Current Compiler CPU Target: '+CompilerCPU);
     GetCompilerCPU:=true;
   end;
end;


function ExitWithInternalError(const OutName:string):boolean;
var
  t : text;
  s : string;
begin
  ExitWithInternalError:=false;
  { open logfile }
  assign(t,Outname);
  {$I-}
   reset(t);
  {$I+}
  if ioresult<>0 then
   exit;
  while not eof(t) do
   begin
     readln(t,s);
     if pos('Fatal: Internal error ',s)>0 then
      begin
        ExitWithInternalError:=true;
        break;
      end;
   end;
  close(t);
end;


function RunCompiler:boolean;
var
  outname,
  args : string;
begin
  RunCompiler:=false;
  OutName:=ForceExtension(PPFile,'log');
  args:='-n -Fuunits';
  if Config.NeedOptions<>'' then
   args:=args+' '+Config.NeedOptions;
  args:=args+' '+ppfile;
  Verbose(V_Debug,'Executing '+compilerbin+' '+args);
  { also get the output from as and ld that writes to stderr sometimes }
  ExecuteRedir(CompilerBin,args,'',OutName,OutName);
  Verbose(V_Debug,'Exitcode '+ToStr(ExecuteResult));

  { Check for internal error }
  if ExitWithInternalError(OutName) then
   begin
     AddLog(FailLogFile,TestName);
     if Note<>'' then
      AddLog(FailLogFile,Note);
     AddLog(ResLogFile,failed_to_compile+PPFileInfo+' internalerror generated');
     AddLog(LongLogFile,line_separation);
     AddLog(LongLogFile,failed_to_compile+PPFileInfo);
     if Note<>'' then
      AddLog(LongLogFile,Note);
     CopyFile(OutName,LongLogFile,true);
     { avoid to try again }
     AddLog(ForceExtension(PPFile,'elg'),'Failed to compile '++PPFileInfo);
     Verbose(V_Abort,'Internal error in compiler');
     exit;
   end;

  { Shoud the compile fail ? }
  if Config.ShouldFail then
   begin
     if ExecuteResult<>0 then
      begin
        AddLog(ResLogFile,success_compilation_failed+PPFileInfo);
        { avoid to try again }
        AddLog(ForceExtension(PPFile,'elg'),success_compilation_failed+PPFileInfo);
        RunCompiler:=true;
      end
     else
      begin
        AddLog(FailLogFile,TestName);
        if Note<>'' then
          AddLog(FailLogFile,Note);
        AddLog(ResLogFile,failed_compilation_successful+PPFileInfo);
        AddLog(LongLogFile,line_separation);
        AddLog(LongLogFile,failed_compilation_successful+PPFileInfo);
        { avoid to try again }
        AddLog(ForceExtension(PPFile,'elg'),failed_compilation_successful+PPFileInfo);
        if Note<>'' then
          AddLog(LongLogFile,Note);
        CopyFile(OutName,LongLogFile,true);
      end;
   end
  else
   begin
     if ExecuteResult<>0 then
      begin
        AddLog(FailLogFile,TestName);
        if Note<>'' then
          AddLog(FailLogFile,Note);
        AddLog(ResLogFile,failed_to_compile+PPFileInfo);
        AddLog(LongLogFile,line_separation);
        AddLog(LongLogFile,failed_to_compile+PPFileInfo);
        if Note<>'' then
          AddLog(LongLogFile,Note);
        CopyFile(OutName,LongLogFile,true);
        { avoid to try again }
        AddLog(ForceExtension(PPFile,'elg'),failed_to_compile+PPFileInfo);
        Verbose(V_Abort,'Exitcode: '+ToStr(ExecuteResult)+' (expected 0)');
      end
     else
      begin
        AddLog(ResLogFile,successfully_compiled+PPFileInfo);
        RunCompiler:=true;
      end;
   end;
end;


function RunExecutable:boolean;
var
  outname,
  TestExe : string;
begin
  RunExecutable:=false;
  TestExe:=ForceExtension(PPFile,ExeExt);
  OutName:=ForceExtension(PPFile,'elg');
  Verbose(V_Debug,'Executing '+TestExe);
  { don't redirect interactive and graph programs .. }
  if Config.IsInteractive or Config.UsesGraph then
    ExecuteRedir(TestExe,'','','','')
  else
    ExecuteRedir(TestExe,'','',OutName,'');
  Verbose(V_Debug,'Exitcode '+ToStr(ExecuteResult));
  if ExecuteResult<>Config.ResultCode then
   begin
     if (ExecuteResult<>0) and
        (ExecuteResult=Config.KnownRunError) then
       begin
         AddLog(FailLogFile,TestName+known_problem+Config.KnownRunNote);
         AddLog(ResLogFile,failed_to_run+PPFileInfo+known_problem+Config.KnownRunNote);
         AddLog(LongLogFile,line_separation);
         AddLog(LongLogFile,known_problem+Config.KnownRunNote);
         AddLog(LongLogFile,failed_to_run+PPFileInfo+' ('+ToStr(ExecuteResult)+')');
         Copyfile(OutName,LongLogFile,true);
         Verbose(V_Abort,known_problem+'exitcode: '+ToStr(ExecuteResult)+' (expected '+ToStr(Config.ResultCode)+')');
       end
     else
       begin
         AddLog(FailLogFile,TestName);
         AddLog(ResLogFile,failed_to_run+PPFileInfo);
         AddLog(LongLogFile,line_separation);
         AddLog(LongLogFile,failed_to_run+PPFileInfo+' ('+ToStr(ExecuteResult)+')');
         Copyfile(OutName,LongLogFile,true);
         Verbose(V_Abort,'Exitcode: '+ToStr(ExecuteResult)+' (expected '+ToStr(Config.ResultCode)+')');
       end
   end
  else
   begin
     AddLog(ResLogFile,successfully_run+PPFileInfo);
     RunExecutable:=true;
   end;
end;


procedure getargs;
var
  ch   : char;
  para : string;
  i    : longint;

  procedure helpscreen;
  begin
    writeln('dotest [Options] <File>');
    writeln;
    writeln('Options can be:');
    writeln('  -C<compiler>  set compiler to use');
    writeln('  -V            verbose');
    writeln('  -E            execute test also');
    writeln('  -A            include ALL tests');
    writeln('  -G            include graph tests');
    writeln('  -K            include known bug tests');
    writeln('  -I            include interactive tests');
    halt(1);
  end;

begin
  PPFile:='';
  if exeext<>'' then
    CompilerBin:='ppc386.'+exeext
  else
    CompilerBin:='ppc386';
  for i:=1 to paramcount do
   begin
     para:=Paramstr(i);
     if (para[1]='-') then
      begin
        ch:=Upcase(para[2]);
        delete(para,1,2);
        case ch of
         'A' :
           begin
             DoGraph:=true;
             DoInteractive:=true;
             DoKnown:=true;
             DoAll:=true;
           end;
         'C' : CompilerBin:=Para;
         'E' : DoExecute:=true;
         'G' : begin
                 DoGraph:=true;
                 if para='-' then
                   DoUsual:=false;
               end;
         'I' : begin
                 DoInteractive:=true;
                 if para='-' then
                   DoUsual:=false;
               end;
         'V' : DoVerbose:=true;
         'K' : begin
                 DoKnown:=true;
                 if para='-' then
                   DoUsual:=false;
               end;
        end;
     end
    else
     begin
       If PPFile<>'' then
         HelpScreen;
       PPFile:=ForceExtension(Para,'pp');
     end;
    end;
  if (PPFile='') then
   HelpScreen;
  SetPPFileInfo;
  TestName:=Copy(PPFile,1,Pos('.pp',PPFile)-1);
  Verbose(V_Debug,'Running test '+TestName+', file '+PPFile);
end;


procedure RunTest;
var
  Res : boolean;
  OutName : string;
begin
  Res:=GetConfig(ppfile,Config);
  OutName:=ForceExtension(PPFile,'elg');

  if Res then
   begin
     if Config.UsesGraph and (not DoGraph) then
      begin
        AddLog(ResLogFile,skipping_graph_test+PPFileInfo);
        { avoid a second attempt by writing to elg file }
        AddLog(OutName,skipping_graph_test+PPFileInfo);
        Verbose(V_Abort,skipping_graph_test);
        Res:=false;
      end;
   end;

  if Res then
   begin
     if Config.IsInteractive and (not DoInteractive) then
      begin
        { avoid a second attempt by writing to elg file }
        AddLog(OutName,skipping_interactive_test+PPFileInfo);
        AddLog(ResLogFile,skipping_interactive_test+PPFileInfo);
        Verbose(V_Abort,skipping_interactive_test);
        Res:=false;
      end;
   end;

  if Res then
   begin
     if Config.IsKnown and (not DoKnown) then
      begin
        { avoid a second attempt by writing to elg file }
        AddLog(OutName,skipping_known_bug+PPFileInfo);
        AddLog(ResLogFile,skipping_known_bug+PPFileInfo);
        Verbose(V_Abort,skipping_known_bug);
        Res:=false;
      end;
   end;

  if Res and not DoUsual then
    res:=(Config.IsInteractive and DoInteractive) or
         (Config.IsKnown and DoKnown) or
         (Config.UsesGraph and DoGraph);

  if Res then
   begin
     if (Config.NeedVersion<>'') and not DoAll then
      begin
        Verbose(V_Debug,'Required compiler version: '+Config.NeedVersion);
        Res:=GetCompilerVersion;
        if CompilerVersion<Config.NeedVersion then
         begin
           { avoid a second attempt by writing to elg file }
           AddLog(OutName,skipping_compiler_version_too_low+PPFileInfo);
           AddLog(ResLogFile,skipping_compiler_version_too_low+PPFileInfo);
           Verbose(V_Abort,'Compiler version too low '+CompilerVersion+' < '+Config.NeedVersion);
           Res:=false;
         end;
      end;
   end;

  if Res then
   begin
     if Config.NeedCPU<>'' then
      begin
        Verbose(V_Debug,'Required compiler cpu: '+Config.NeedCPU);
        Res:=GetCompilerCPU;
        if Upper(Config.NeedCPU)<>Upper(CompilerCPU) then
         begin
           { avoid a second attempt by writing to elg file }
           AddLog(OutName,skipping_other_cpu+PPFileInfo);
           AddLog(ResLogFile,skipping_other_cpu+PPFileInfo);
           Verbose(V_Abort,'Compiler cpu wrong '+CompilerCPU+' <> '+Config.NeedCPU);
           Res:=false;
         end;
      end;
   end;

  if Res then
   begin
     Res:=RunCompiler;
     if Res and Config.NeedRecompile then
      Res:=RunCompiler;
   end;

  if Res then
   begin
     if (Config.NoRun) then
      begin
        { avoid a second attempt by writing to elg file }
        AddLog(OutName,skipping_run_test+PPFileInfo);
        AddLog(ResLogFile,skipping_run_test+PPFileInfo);
        Verbose(V_Debug,skipping_run_test);
      end
     else
      begin
        if (not Config.ShouldFail) and DoExecute then
         begin
           if FileExists(ForceExtension(PPFile,'ppu')) or
              FileExists(ForceExtension(PPFile,'ppo')) or
              FileExists(ForceExtension(PPFile,'ppw')) then
             begin
               AddLog(ForceExtension(PPFile,'elg'),skipping_run_unit+PPFileInfo);
               AddLog(ResLogFile,skipping_run_unit+PPFileInfo);
               Verbose(V_Debug,'Unit found, skipping run test')
             end
           else
            Res:=RunExecutable;
         end;
      end;
   end;
end;


begin
  GetArgs;
  RunTest;
end.
{
  $Log$
  Revision 1.20  2002-11-18 16:42:43  pierre
   + KNOWNRUNERROR added

  Revision 1.19  2002/11/18 01:31:07  pierre
   + use -n option
   + use -G- for only graph
   + use -I- for only interactive
   + use -K- for only known bugs.

  Revision 1.18  2002/11/14 10:36:12  pierre
   * add internalerror info to log file

  Revision 1.17  2002/11/13 15:26:24  pierre
   + digest program added

  Revision 1.16  2002/11/13 15:19:44  pierre
   log strings moved to teststr unit

  Revision 1.15  2002/09/07 15:40:56  peter
    * old logs removed and tabs fixed

  Revision 1.14  2002/04/21 18:15:32  peter
    * Check for internal errors

  Revision 1.13  2002/03/03 13:27:28  hajny
    + added support for OS/2 units (.ppo)

  Revision 1.12  2002/01/29 13:24:16  pierre
   + also generate .elg file for units

  Revision 1.11  2002/01/29 12:51:08  pierre
    + PPFileInfo to also display time stamp of test file
    * generate .elg file in several cases
      to avoid trying to recompute the same test
      over and over again.

}
