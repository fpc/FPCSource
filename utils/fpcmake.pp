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
  sec_sections='sections';
  sec_install='install';
  sec_clean='clean';
  sec_dirs='dirs';
  sec_libs='libs';
  sec_targets='targets';
  sec_info='info';
  sec_defaults='defaults';
  sec_tools='tools';

type
  TTargetsString=array[0..targets] of string;
  TFpcMake=record
    TargetLoaders,
    TargetUnits,
    TargetPrograms,
    InstallUnits,
    InstallFiles,
    CleanUnits,
    CleanFiles     : TTargetsString;
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
    DirInc         : string;
    LibName,
    LibUnits       : string;
    LibGCC,
    LibOther       : boolean;
    InfoCfg,
    InfoDirs,
    InfoTools,
    InfoInstall,
    InfoObjects,
    InfoFiles      : boolean;
    SectionNone,
    SectionCompile,
    SectionDepend,
    SectionInstall,
    SectionZipInstall,
    SectionClean,
    SectionLibs,
    SectionCommand,
    SectionExts,
    SectionDirs,
    SectionTools,
    SectionInfo    : boolean;
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

  procedure ReadTargetsString(var t:Ttargetsstring;const sec,name,def:string);
  var
    i : integer;
  begin
    t[0]:=ini.ReadString(sec,name,def);
    for i:=1 to targets do
     t[i]:=ini.ReadString(sec,name+'_'+targetstr[i],'');
  end;

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
     ReadTargetsString(TargetLoaders,sec_targets,'loaders','');
     ReadTargetsString(TargetUnits,sec_targets,'units','');
     ReadTargetsString(TargetPrograms,sec_targets,'programs','');
   { clean }
     ReadTargetsString(CleanUnits,sec_clean,'units','');
     ReadTargetsString(CleanFiles,sec_clean,'files','');
   { install }
     ReadTargetsString(InstallUnits,sec_install,'units','');
     ReadTargetsString(InstallFiles,sec_install,'files','');
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
   { libs }
     LibName:=ReadString(sec_libs,'libname','');
     LibUnits:=ReadString(sec_libs,'libunits','');
     LibGcc:=ReadBool(sec_libs,'libgcc',false);
     LibOther:=ReadBool(sec_libs,'libother',false);
   { tools }
     ToolsSed:=ReadBool(sec_tools,'toolsed',false);
     ToolsDiff:=ReadBool(sec_tools,'tooldiff',false);
     ToolsCmp:=ReadBool(sec_tools,'toolcmp',false);
     ToolsUpx:=ReadBool(sec_tools,'toolupx',true);
     ToolsDate:=ReadBool(sec_tools,'tooldate',true);
     ToolsZip:=ReadBool(sec_tools,'toolzip',true);
   { sections }
     SectionInstall:=ReadBool(sec_sections,'install',true);
     SectionZipInstall:=ReadBool(sec_sections,'zipinstall',true);
     SectionClean:=ReadBool(sec_sections,'clean',true);
     SectionInfo:=ReadBool(sec_sections,'info',true);
     SectionTools:=ReadBool(sec_sections,'tools',true);
     SectionLibs:=ReadBool(sec_sections,'libs',true);
     SectionExts:=ReadBool(sec_sections,'exts',true);
     SectionDirs:=ReadBool(sec_sections,'dirs',true);
     SectionCompile:=ReadBool(sec_sections,'compile',true);
     SectionDepend:=ReadBool(sec_sections,'depend',true);
     SectionCommand:=ReadBool(sec_sections,'command',true);
     SectionNone:=ReadBool(sec_sections,'none',false);
     if SectionNone then
      begin
        SectionInstall:=false;
        SectionZipInstall:=false;
        SectionClean:=false;
        SectionInfo:=false;
        SectionTools:=false;
        SectionLibs:=false;
        SectionExts:=false;
        SectionDirs:=false;
        SectionCompile:=false;
        SectionDepend:=false;
        SectionCommand:=false;
      end;
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
   if (FileExists(GetEnv(envvar))) then
    fn:=GetEnv(envvar)
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

  procedure AddTargets(const pre:string;var t:TTargetsString);
  var
    i : integer;
  begin
    if t[0]<>'' then
     mf.Add(pre+'='+t[0]);
    for i:=1to targets do
     if (t[i]<>'') then
      begin
        mf.Add('ifeq ($(OS_TARGET),'+targetstr[i]+')');
        if t[i]<>'' then
         mf.Add(pre+'+='+t[i]);
        mf.Add('endif');
      end;
  end;

  procedure AddHead(const s:string);
  begin
    mf.Add('');
    mf.Add('# '+s);
    mf.Add('');
  end;

var
  hs : string;
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
     Add('# Makefile generated by '+title+' on '+FormatDateTime(TimeFormat,Now));
     Add('#');
     Add('');
     Add('defaultrule: '+userini.defaultrule);
     Add('');
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
      begin
        AddHead('Pre Settings');
        AddStrings(userini.PreSettings);
      end;

   { Targets }
     AddHead('Targets');
     AddTargets('LOADEROBJECTS',userini.targetloaders);
     AddTargets('UNITOBJECTS',userini.targetunits);
     AddTargets('EXEOBJECTS',userini.targetprograms);

   { Clean }
     AddHead('Clean');
     AddTargets('EXTRACLEANUNITS',userini.cleanunits);
     AddTargets('EXTRACLEANFILES',userini.cleanfiles);

   { Install }
     AddHead('Install');
     AddTargets('EXTRAINSTALLUNITS',userini.installunits);
     AddTargets('EXTRAINSTALLFILES',userini.installfiles);

   { Defaults }
     AddHead('Defaults');
     if userini.defaultunits then
      Add('DEFAULTUNITS=1');
     if userini.defaultoptions<>'' then
      Add('override NEEDOPT='+userini.defaultoptions);

   { Dirs }
     AddHead('Directories');
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
     AddHead('Libraries');
     if userini.libname<>'' then
      Add('LIBNAME='+userini.libname);
     if userini.libunits<>'' then
      Add('SHAREDLIBOBJECTUNITS='+userini.libunits);
     if userini.libgcc then
      Add('override NEEDGCCLIB=1');
     if userini.libother then
      Add('override NEEDOTHERLIB=1');

   { Info }
     if userini.SectionInfo then
      begin
        AddHead('Info');
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
      end;

   { Post Settings }
     if userini.PostSettings.count>0 then
      begin
        AddHead('Post Settings');
        AddStrings(userini.PostSettings);
      end;

     Add('');

   { write dirs }
     if userini.sectiondirs then
      begin
        AddSection(true,'dir_default');
        AddSection(true,'dir_rtl');
        AddSection(true,'dir_units');
        AddSection(true,'dir_gcclib');
        AddSection(true,'dir_otherlib');
        AddSection(true,'dir_install');
      end;

   { commandline }
     if userini.sectioncommand then
      begin
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
        AddSection((userini.dirtarget<>''),'command_target');
        AddSection((userini.dirunittarget<>''),'command_unittarget');
        AddSection(true,'command_smartlink');
        AddSection(true,'command_end');
      end;

   { write tools }
     if userini.sectiontools then
      begin
        AddSection(true,'shelltools');
        AddSection(true,'tool_default');
        AddSection(userini.toolsupx,'tool_upx');
        AddSection(userini.toolssed,'tool_sed');
        AddSection(userini.toolsdate,'tool_date');
        AddSection(userini.toolszip,'tool_zip');
        AddSection(userini.toolscmp,'tool_cmp');
        AddSection(userini.toolsdiff,'tool_diff');
      end;

   { extensions }
     if userini.sectionexts then
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
     if userini.SectionCompile then
      AddSection(true,'compilerules');
     if userini.SectionLibs then
      AddSection(true,'libraryrules');
     if userini.SectionInstall then
      AddSection(true,'installrules');
     if userini.SectionZipInstall then
      AddSection(true,'zipinstallrules');
     if userini.SectionClean then
      AddSection(true,'cleanrules');
     if userini.SectionDepend then
      AddSection(true,'dependrules');
     if userini.SectionInfo then
      begin
        AddSection(true,'inforules');
        AddSection(userini.infocfg,'info_cfg');
        AddSection(userini.infodirs,'info_dirs');
        AddSection(userini.infotools,'info_tools');
        AddSection(userini.infoobjects,'info_objects');
        AddSection(userini.infoinstall,'info_install');
        AddSection(userini.infofiles,'info_files');
      end;

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
  Revision 1.6  1999-11-10 22:10:49  peter
    * fpcmake updated

  Revision 1.5  1999/11/09 14:38:32  peter
    * sections section to leave out whole info/tools

  Revision 1.4  1999/11/08 15:01:39  peter
    * fpcmake support

  Revision 1.3  1999/11/04 12:07:13  michael
  + Now envvar is used

  Revision 1.2  1999/11/03 23:39:53  peter
    * lot of updates

  Revision 1.1  1999/11/02 23:57:40  peter
    * initial version

}
