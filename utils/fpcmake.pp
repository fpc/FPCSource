{
    $Id$
    Copyright (c) 1999 by Peter Vreman

    Convert Makefile.fpc to Makefile

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$ifdef fpc}{$mode objfpc}{$endif}
{$H+}
program fpcmake;
uses
{$ifdef go32v2}
  dpmiexcp,
{$endif}
  dos,
  sysutils,classes,inifiles;

const
  Version='v0.99.13';
  Title='fpcmake '+Version;

  EnvVar='FPCMAKEINI'; { should be FPCMAKE in the future }
  TimeFormat='yyyy/mm/dd hh:nn';

  targets=4;
  targetstr : array[1..targets] of string=(
    'linux','go32v2','win32','os2'
  );

{ Sections in Makefile.fpc }
  sec_dirs='dirs';
  sec_libs='libs';
  sec_targets='targets';
  sec_info='info';
  sec_defaults='defaults';
  sec_tools='tools';

type
  TFpcMake=record
    TargetUnits,
    TargetPrograms : array[0..targets] of string;
    DefaultUnits   : boolean;
    DefaultRule,
    DefaultTarget,
    DefaultCPU,
    DefaultOptions : string;
    DirFpc,
    DirUnit,
    DirLib,
    DirObj,
    DirTarget,
    DirUnitTarget,
    DirInc,
    DirProcInc,
    DirOSInc       : string;
    LibGCC,
    LibOther       : boolean;
    InfoCfg,
    InfoDirs,
    InfoTools,
    InfoInstall,
    InfoObjects,
    InfoFiles      : boolean;
    ToolsSed,
    ToolsDiff,
    ToolsCmp,
    ToolsUpx,
    ToolsDate,
    ToolsZip       : boolean;
    PreSettings,
    PostSettings,
    Rules          : TStringList;
  end;

var
  userini : TFpcMake;
  fpcini  : TIniFile;

{*****************************************************************************
                                     Helpers
*****************************************************************************}

procedure Verbose(s:string);
begin
  writeln(s);
end;


procedure Error(s:string);
begin
  Writeln(s);
  Halt(1);
end;


{*****************************************************************************
                               Makefile.fpc reading
*****************************************************************************}

function ReadMakefilefpc:boolean;
var
  fn  : string;
  ini : TIniFile;
  i   : integer;
begin
  ReadMakefilefpc:=false;
  if FileExists('Makefile.fpc') then
   fn:='Makefile.fpc'
  else
   if FileExists('makefile.fpc') then
    fn:='makefile.fpc'
  else
   exit;

  Verbose('Reading '+fn);
  ini:=TIniFile.Create(fn);

  with userini,ini do
   begin
   { targets }
     TargetUnits[0]:=ReadString(sec_targets,'units','');
     TargetPrograms[0]:=ReadString(sec_targets,'programs','');
     for i:=1 to targets do
      begin
        TargetUnits[i]:=ReadString(sec_targets,'units_'+targetstr[i],'');
        TargetPrograms[i]:=ReadString(sec_targets,'programs_'+targetstr[i],'');
      end;
   { defaults }
     DefaultUnits:=ReadBool(sec_defaults,'defaultunits',false);
     DefaultRule:=ReadString(sec_defaults,'defaultrule','all');
     DefaultTarget:=ReadString(sec_defaults,'defaulttarget','');
     DefaultCPU:=ReadString(sec_defaults,'defaultcpu','');
     DefaultOptions:=ReadString(sec_defaults,'defaultoptions','');
   { dirs }
     DirFpc:=ReadString(sec_dirs,'fpcdir','');
     DirUnit:=ReadString(sec_dirs,'unitdir','');
     DirLib:=ReadString(sec_dirs,'libdir','');
     DirObj:=ReadString(sec_dirs,'objdir','');
     DirTarget:=ReadString(sec_dirs,'targetdir','');
     DirUnitTarget:=ReadString(sec_dirs,'unittargetdir','');
     DirInc:=ReadString(sec_dirs,'incdir','');
     DirProcInc:=ReadString(sec_dirs,'procincdir','');
     DirOSInc:=ReadString(sec_dirs,'osincdir','');
   { libs }
     LibGcc:=ReadBool(sec_libs,'libgcc',false);
     LibOther:=ReadBool(sec_libs,'libother',false);
   { tools }
     ToolsSed:=ReadBool(sec_tools,'toolsed',false);
     ToolsDiff:=ReadBool(sec_tools,'tooldiff',false);
     ToolsCmp:=ReadBool(sec_tools,'toolcmp',false);
     ToolsUpx:=ReadBool(sec_tools,'toolupx',true);
     ToolsDate:=ReadBool(sec_tools,'tooldate',true);
     ToolsZip:=ReadBool(sec_tools,'toolzip',true);
   { info }
     InfoCfg:=ReadBool(sec_info,'infoconfig',true);
     InfoDirs:=ReadBool(sec_info,'infodirs',false);
     InfoTools:=ReadBool(sec_info,'infotools',false);
     InfoInstall:=ReadBool(sec_info,'infoinstall',true);
     InfoObjects:=ReadBool(sec_info,'infoobjects',true);
     InfoFiles:=ReadBool(sec_info,'infofiles',false);
   { rules }
     PreSettings:=TStringList.Create;
     ReadSectionRaw('presettings',PreSettings);
   { rules }
     PostSettings:=TStringList.Create;
     ReadSectionRaw('postsettings',PostSettings);
   { rules }
     rules:=TStringList.Create;
     ReadSectionRaw('rules',rules);
   end;

  ini.Destroy;
  ReadMakefilefpc:=true;
end;


{*****************************************************************************
                               userini.ini loading
*****************************************************************************}

function ReadFpcMakeIni:TIniFile;
var
  fn : string;
begin
  ReadFpcMakeIni:=nil;
  if FileExists('fpcmake.ini') then
   fn:='fpcmake.ini'
  else
   if (FileExists(GetEnv('FPCMAKEINI'))) then
    fn:=GetEnv('FPCMAKEINI')
  else
{$ifdef linux}
   if FileExists('/usr/lib/fpc/fpcmake.ini') then
    fn:='/usr/lib/fpc/fpcmake.ini'
{$else}
   if FileExists(ChangeFileExt(paramstr(0),'.ini')) then
    fn:=ChangeFileExt(paramstr(0),'.ini')
{$endif}
  else
   exit;

  Verbose('Opening '+fn);
  result:=TIniFile.Create(fn);
end;


{*****************************************************************************
                               Makefile writing
*****************************************************************************}

function WriteMakefile:boolean;
var
  mf : TStringList;
  ss : TStringList;

  procedure FixTab(sl:TStringList);
  var
    i,j,k : integer;
    s,s2  : string;
  begin
    i:=0;
    while (i<sl.Count) do
     begin
       if (sl[i]<>'') and (sl[i][1] in [' ',#9]) then
        begin
          s:=sl[i];
          k:=0;
          j:=0;
          repeat
            inc(j);
            case s[j] of
              ' ' :
                inc(k);
              #9 :
                k:=(k+7) and not(7);
              else
                break;
            end;
          until (j=length(s));
          if k>7 then
           begin
             s2:='';
             Delete(s,1,j-1);
             while (k>7) do
              begin
                s2:=s2+#9;
                dec(k,8);
              end;
             while (k>0) do
              begin
                s2:=s2+' ';
                dec(k);
              end;
             sl[i]:=s2+s;
           end;
        end;
       inc(i);
     end;
  end;

  procedure AddSection(b:boolean;s:string);
  begin
    if b then
     begin
       ss.Clear;
       fpcini.ReadSectionRaw(s,ss);
       mf.AddStrings(ss);
       mf.Add('');
     end;
  end;

  procedure AddRule(s:string);
  var
    i : integer;
  begin
    i:=0;
    while (i<userini.rules.Count) do
     begin
       if (userini.rules[i]<>'') and
          (userini.rules[i][1]=s[1]) and
          (Copy(userini.rules[i],1,length(s))=s) then
         exit;
       inc(i);
     end;
    mf.Add(s+': fpc_'+s);
    mf.Add('');
  end;

var
  hs : string;
  i  : integer;
begin
{ Open the Makefile }
  Verbose('Creating Makefile');
  mf:=TStringList.Create;
{ Buffer for reading and writing the sections }
  ss:=TStringList.Create;

  with mf do
   begin
   { write header & autodetection }
     Add('#');
     Add('# Makefile generated from Makefile.fpc on '+FormatDateTime(TimeFormat,Now));
     Add('#');
     Add('');
     Add('defaultrule: Makefile '+userini.defaultrule);
     Add('');
     AddSection(true,'makefilerule');
     AddSection(true,'osdetect');

   { set the forced target os/cpu }
     if (userini.defaulttarget<>'') or (userini.defaultcpu<>'') then
      begin
        AddSection(true,'defaulttarget');
        if userini.defaulttarget<>'' then
         Add('override OS_TARGET:='+userini.defaulttarget);
        if userini.defaultcpu<>'' then
         Add('override CPU_TARGET:='+userini.defaultcpu);
        Add('');
      end;

   { fpc detection }
     AddSection(true,'fpcdetect');

   { write the default & user settings }
     AddSection(true,'defaultsettings');
     AddSection(true,'usersettings');

   { Pre Settings }
     if userini.PreSettings.count>0 then
      AddStrings(userini.PreSettings);

   { Targets }
     Add('');
     Add('UNITOBJECTS='+userini.targetunits[0]);
     Add('EXEOBJECTS='+userini.targetprograms[0]);
     for i:=1to targets do
      if (userini.targetunits[i]<>'') or
         (userini.targetprograms[i]<>'') then
      begin
        Add('ifeq ($(OS_TARGET),'+targetstr[i]+')');
        if userini.targetunits[i]<>'' then
         Add('UNITOBJECTS+='+userini.targetunits[i]);
        if userini.targetprograms[i]<>'' then
         Add('EXEOBJECTS+='+userini.targetprograms[i]);
        Add('endif');
      end;

   { Defaults }
     Add('');
     if userini.defaultunits then
      Add('DEFAULTUNITS=1');
     if userini.defaultoptions<>'' then
      Add('override NEEDOPT='+userini.defaultoptions);

   { Dirs }
     Add('');
     if userini.dirfpc<>'' then
      begin
        { this dir can be set in the environment, it's more a default }
        Add('ifndef FPCDIR');
        Add('FPCDIR='+userini.dirfpc);
        Add('endif');
      end;
     if userini.dirunit<>'' then
      Add('override NEEDUNITDIR='+userini.dirunit);
     if userini.dirlib<>'' then
      Add('override NEEDLIBDIR='+userini.dirlib);
     if userini.dirobj<>'' then
      Add('override NEEDOBJDIR='+userini.dirobj);
     if userini.dirinc<>'' then
      Add('override NEEDINCDIR='+userini.dirinc);
     if userini.dirtarget<>'' then
      begin
        Add('ifndef TARGETDIR');
        Add('TARGETDIR='+userini.dirtarget);
        Add('endif');
      end;
     if userini.dirunittarget<>'' then
      begin
        Add('ifndef UNITTARGETDIR');
        Add('UNITTARGETDIR='+userini.dirunittarget);
        Add('endif');
      end;

   { Libs }
     Add('');
     if userini.libgcc then
      Add('override NEEDGCCLIB=1');
     if userini.libother then
      Add('override NEEDOTHERLIB=1');

   { Info }
     Add('');
     hs:='';
     if userini.infocfg then
      hs:=hs+'fpc_infocfg ';
     if userini.infodirs then
      hs:=hs+'fpc_infodirs ';
     if userini.infotools then
      hs:=hs+'fpc_infotools ';
     if userini.infoobjects then
      hs:=hs+'fpc_infoobjects ';
     if userini.infoinstall then
      hs:=hs+'fpc_infoinstall ';
     if userini.infofiles then
      hs:=hs+'fpc_infofiles ';
     Add('FPCINFO='+hs);

   { Post Settings }
     if userini.PostSettings.count>0 then
      AddStrings(userini.PostSettings);

   { commandline }
     Add('');
     AddSection(true,'command_begin');
     AddSection(true,'command_rtl');
     AddSection(true,'command_needopt');
     AddSection((userini.dirfpc<>''),'command_fpcdir');
     AddSection((userini.dirunit<>''),'command_needunit');
     AddSection((userini.dirlib<>''),'command_needlib');
     AddSection((userini.dirobj<>''),'command_needobj');
     AddSection((userini.dirinc<>''),'command_needinc');
     AddSection(userini.libgcc,'command_gcclib');
     AddSection(userini.libother,'command_otherlib');
     AddSection((userini.dirinc<>''),'command_inc');
     AddSection((userini.dirprocinc<>''),'command_procinc');
     AddSection((userini.dirosinc<>''),'command_osinc');
     AddSection((userini.dirtarget<>''),'command_target');
     AddSection((userini.dirunittarget<>''),'command_unittarget');
     AddSection(true,'command_smartlink');
     AddSection(true,'command_end');

   { write tools }
     AddSection(true,'shelltools');
     AddSection(true,'tool_default');
     AddSection(userini.toolsupx,'tool_upx');
     AddSection(userini.toolssed,'tool_sed');
     AddSection(userini.toolsdate,'tool_date');
     AddSection(userini.toolszip,'tool_zip');
     AddSection(userini.toolscmp,'tool_cmp');
     AddSection(userini.toolsdiff,'tool_diff');

   { write dirs }
     AddSection(true,'dir_default');
     AddSection(true,'dir_rtl');
     AddSection(true,'dir_units');
     AddSection(true,'dir_gcclib');
     AddSection(true,'dir_otherlib');
     AddSection(true,'dir_install');

   { extensions }
     AddSection(true,'extensions');

   { add default rules }
     AddSection(true,'defaultrules');
     AddRule('all');
     AddRule('staticlib');
     AddRule('sharedlib');
     AddRule('showinstall');
     AddRule('install');
     AddRule('staticinstall');
     AddRule('sharedinstall');
     AddRule('libinstall');
     AddRule('zipinstall');
     AddRule('zipinstalladd');
     AddRule('clean');
     AddRule('clean_all');
     AddRule('depend');
     AddRule('info');

   { default fpc_ rules }
     AddSection(true,'compilerules');
     AddSection(true,'libraryrules');
     AddSection(true,'installrules');
     AddSection(true,'zipinstallrules');
     AddSection(true,'cleanrules');
     AddSection(true,'dependrules');
     AddSection(true,'inforules');
     AddSection(userini.infocfg,'info_cfg');
     AddSection(userini.infodirs,'info_dirs');
     AddSection(userini.infotools,'info_tools');
     AddSection(userini.infoobjects,'info_objects');
     AddSection(userini.infoinstall,'info_install');
     AddSection(userini.infofiles,'info_files');

   { insert users rules }
     if userini.rules.count>0 then
      begin
        AddSection(true,'userrules');
        AddStrings(userini.rules);
      end;
   end;

{ Write the Makefile and cleanup }
  Verbose('Writing Makefile');
  FixTab(mf);
  mf.SaveToFile('Makefile');
  mf.Destroy;
  ss.Destroy;
  WriteMakefile:=true;
end;


begin
{ Open userini.ini }
  fpcini:=ReadFpcMakeIni;
  if not assigned(fpcini) then
   Error('Can''t read fpcmake.ini');

{ Open Makefile.fpc }
  if not ReadMakefilefpc then
   Error('Can''t read Makefile.fpc');

{ Write Makefile }
  if not WriteMakefile then
   Error('Can''t write Makefile');

  fpcini.destroy;
end.
{
  $Log$
  Revision 1.2  1999-11-03 23:39:53  peter
    * lot of updates

  Revision 1.1  1999/11/02 23:57:40  peter
    * initial version

}
