{
  $Id$
}
program dotest;
uses
  dos,
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
    NeedVersion   : string;
    ResultCode    : longint;
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
  ResLogfile  : string[32] = 'log';
  LongLogfile : string[32] = 'longlog';
  FailLogfile : string[32] = 'faillist';
  DoVerbose : boolean = false;
  DoGraph : boolean = false;
  DoInteractive : boolean = false;
  DoExecute : boolean = false;
  DoKnown : boolean = false;

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
  code : integer;
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


function RunCompiler:boolean;
var
  outname,
  args : string;
begin
  RunCompiler:=false;
  OutName:=ForceExtension(PPFile,'log');
  args:='-Fuunits';
  if Config.NeedOptions<>'' then
   args:=args+' '+Config.NeedOptions;
  args:=args+' '+ppfile;
  Verbose(V_Debug,'Executing '+compilerbin+' '+args);
  { also get the output from as and ld that writes to stderr sometimes }
  ExecuteRedir(CompilerBin,args,'',OutName,OutName);
  Verbose(V_Debug,'Exitcode '+ToStr(ExecuteResult));
  { Shoud the compile fail ? }
  if Config.ShouldFail then
   begin
     if ExecuteResult<>0 then
      begin
        AddLog(ResLogFile,'Success, compilation failed '+PPFileInfo);
        { avoid to try again }
        AddLog(ForceExtension(PPFile,'elg'),'Success, compilation failed '+PPFileInfo);
        RunCompiler:=true;
      end
     else
      begin
        AddLog(FailLogFile,TestName);
        if Note<>'' then
          AddLog(FailLogFile,Note);
        AddLog(ResLogFile,'Failed, compilation successful '+PPFileInfo);
        AddLog(LongLogFile,'>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>');
        AddLog(LongLogFile,'Failed, compilation successful '+PPFileInfo);
        { avoid to try again }
        AddLog(ForceExtension(PPFile,'elg'),'Failed, compilation successful '+PPFileInfo);
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
        AddLog(ResLogFile,'Failed to compile '+PPFileInfo);
        AddLog(LongLogFile,'>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>');
        AddLog(LongLogFile,'Failed to compile '+PPFileInfo);
        if Note<>'' then
          AddLog(LongLogFile,Note);
        CopyFile(OutName,LongLogFile,true);
        { avoid to try again }
        AddLog(ForceExtension(PPFile,'elg'),'Failed to compile '++PPFileInfo);
        Verbose(V_Abort,'Exitcode: '+ToStr(ExecuteResult)+' (expected 0)');
      end
     else
      begin
        AddLog(ResLogFile,'Successfully compiled '+PPFileInfo);
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
  ExecuteRedir(TestExe,'','',OutName,'');
  Verbose(V_Debug,'Exitcode '+ToStr(ExecuteResult));
  if ExecuteResult<>Config.ResultCode then
   begin
     AddLog(FailLogFile,TestName);
     AddLog(ResLogFile,'Failed to run '+PPFileInfo);
     AddLog(LongLogFile,'>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>');
     AddLog(LongLogFile,'Failed to run '+PPFileInfo+' ('+ToStr(ExecuteResult)+')');
     Copyfile(OutName,LongLogFile,true);
     Verbose(V_Abort,'Exitcode: '+ToStr(ExecuteResult)+' (expected '+ToStr(Config.ResultCode)+')');
   end
  else
   begin
     AddLog(ResLogFile,'Successfully run '+PPFileInfo);
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
           end;
         'C' : CompilerBin:=Para;
         'E' : DoExecute:=true;
         'G' : DoGraph:=true;
         'I' : DoInteractive:=true;
         'V' : DoVerbose:=true;
         'K' : DoKnown:=true;
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
        AddLog(ResLogFile,'Skipping test because it uses graph '+PPFileInfo);
        { avoid a second attempt by writing to elg file }
        AddLog(OutName,'Skipping test because it uses graph '+PPFileInfo);
        Verbose(V_Abort,'Skipping test because it uses graph ');
        Res:=false;
      end;
   end;

  if Res then
   begin
     if Config.IsInteractive and (not DoInteractive) then
      begin
        { avoid a second attempt by writing to elg file }
        AddLog(OutName,'Skipping test because it is interactive '+PPFileInfo);
        AddLog(ResLogFile,'Skipping test because it is interactive '+PPFileInfo);
        Verbose(V_Abort,'Skipping test because it is interactive ');
        Res:=false;
      end;
   end;

  if Res then
   begin
     if Config.IsKnown and (not DoKnown) then
      begin
        { avoid a second attempt by writing to elg file }
        AddLog(OutName,'Skipping test because it is a known bug '+PPFileInfo);
        AddLog(ResLogFile,'Skipping test because it is a known bug '+PPFileInfo);
        Verbose(V_Abort,'Skipping test because it is a known bug ');
        Res:=false;
      end;
   end;

  if Res then
   begin
     if Config.NeedVersion<>'' then
      begin
        Verbose(V_Debug,'Required compiler version: '+Config.NeedVersion);
        Res:=GetCompilerVersion;
        if CompilerVersion<Config.NeedVersion then
         begin
           { avoid a second attempt by writing to elg file }
           AddLog(OutName,'Skipping test because compiler version too low '+PPFileInfo);
           AddLog(ResLogFile,'Skipping test because compiler version too low '+PPFileInfo);
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
           AddLog(OutName,'Skipping test because for other cpu '+PPFileInfo);
           AddLog(ResLogFile,'Skipping test because for other cpu '+PPFileInfo);
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
        AddLog(OutName,'Skipping run test '+PPFileInfo);
        AddLog(ResLogFile,'Skipping run test '+PPFileInfo);
        Verbose(V_Debug,'Skipping run test ');
      end
     else
      begin
        if (not Config.ShouldFail) and DoExecute then
         begin
           if FileExists(ForceExtension(PPFile,'ppu')) or
              FileExists(ForceExtension(PPFile,'ppw')) then
             begin
               AddLog(ForceExtension(PPFile,'elg'),'Skipping test run because it is a unit '+PPFileInfo);
               AddLog(ResLogFile,'Skipping test run because it is a unit '+PPFileInfo);
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
  Revision 1.12  2002-01-29 13:24:16  pierre
   + also generate .elg file for units

  Revision 1.11  2002/01/29 12:51:08  pierre
    + PPFileInfo to also display time stamp of test file
    * generate .elg file in several cases
      to avoid trying to recompute the same test
      over and over again.

  Revision 1.10  2001/07/31 09:00:16  pierre
   + %Note= comment added

  Revision 1.9  2001/07/04 11:23:39  florian
    * spelling mistake fixed

  Revision 1.8  2001/06/02 00:41:36  peter
    * write exitcode for all executed programs in debug mode

  Revision 1.7  2000/12/09 16:01:10  peter
    + known bug flag
    + norun flag
    + recompile flag

  Revision 1.6  2000/12/04 22:06:25  peter
    * fixed stupid c&p bug for CPU check

  Revision 1.5  2000/12/03 22:59:10  florian
    * some problems for go32v2 fixed
}
