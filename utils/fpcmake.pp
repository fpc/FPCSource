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
  TitleDate=Title+' ['+{$i %DATE}+']';

{ Include default fpcmake.ini }
{$i fpcmake.inc}

type
  tsections=(sec_none,
    sec_units,sec_exes,sec_loaders,sec_examples,sec_package,
    sec_compile,sec_require,sec_install,sec_sourceinstall,sec_zipinstall,
    sec_clean,sec_libs,sec_command,sec_exts,sec_dirs,sec_tools,sec_info
  );

  tbic=(bic_none,bic_build,bic_install,bic_clean);

  tspecialdir=record
    dir,unitdir,packdir : string;
  end;


const
  EnvVar='FPCMAKEINI';

  sectionstr : array[tsections] of string=('none',
    'units','exes','loaders','examples','package',
    'compile','require','install','sourceinstall','zipinstall',
    'clean','libs','command','exts','dirs','tools','info'
  );

  sectiondef : array[tsections] of boolean=(false,
    true,true,false,false,false,
    true,false,true,true,true,
    true,true,true,true,true,true,true
  );

  targets=4;
  targetstr : array[1..targets] of string=(
    'linux','go32v2','win32','os2'
  );

  rules=15;
  rulestr : array[1..rules] of string=(
    'all','debug',
    'examples','test',
    'smart','shared',
    'showinstall','install','sourceinstall','zipinstall','zipinstalladd',
    'clean','cleanall',
    'require','info'
  );

  rule2sec : array[1..rules] of tsections=(
    sec_compile,sec_compile,
    sec_examples,sec_examples,
    sec_libs,sec_libs,
    sec_install,sec_install,sec_sourceinstall,sec_zipinstall,sec_zipinstall,
    sec_clean,sec_clean,
    sec_require,sec_info
  );

  rule2bic : array[1..rules] of tbic=(
    bic_build,bic_build,
    bic_build,bic_build,
    bic_build,bic_build,
    bic_install,bic_install,bic_install,bic_install,bic_install,
    bic_clean,bic_clean,
    bic_none,bic_none
  );

  rulediralso : array[1..rules] of boolean=(
    true,true,
    true,true,
    true,true,
    true,true,true,false,false,
    true,true,
    true,false
  );

  specialdirs = 4;
  specialdir : array[1..specialdirs] of tspecialdir=(
    (dir: 'rtl';  unitdir: '$(UNITSDIR)/rtl';  packdir: '$(FPCDIR)/rtl'),
    (dir: 'fcl';  unitdir: '$(UNITSDIR)/fcl';  packdir: '$(FPCDIR)/fcl'),
    (dir: 'api';  unitdir: '$(UNITSDIR)/api';  packdir: '$(FPCDIR)/api'),
    (dir: 'fv';   unitdir: '$(UNITSDIR)/fv';   packdir: '$(FPCDIR)/fv')
  );

{ Sections in Makefile.fpc }
  ini_sections='sections';
  ini_install='install';
  ini_zip='zip';
  ini_clean='clean';
  ini_dirs='dirs';
  ini_require='require';
  ini_libs='libs';
  ini_targets='targets';
  ini_info='info';
  ini_defaults='defaults';
  ini_tools='tools';

type
  TTargetsString=array[0..targets] of string;
  TFpcMake=record
    TargetDirs,
    TargetLoaders,
    TargetUnits,
    TargetPrograms,
    TargetExamples,
    TargetRST      : TTargetsString;
    InstallUnitSubDir,
    InstallPrefixDir,
    InstallDataDir,
    InstallBaseDir : string;
    InstallUnits,
    InstallFiles   : TTargetsString;
    CleanUnits,
    CleanFiles     : TTargetsString;
    ZipName,
    ZipTarget      : string;
    DefaultRule,
    DefaultBuildDir,
    DefaultInstallDir,
    DefaultCleanDir,
    DefaultDir,
    DefaultTarget,
    DefaultCPU     : string;
    DirFpc,
    DirPackage,
    DirComponent,
    DirUnit,
    DirLib,
    DirObj,
    DirTarget,
    DirUnitTarget,
    DirSources,
    DirInc         : string;
    RequireRTL      : boolean;
    RequireOptions  : string;
    RequirePackages,
    RequireComponents : TTargetsString;
    LibName,
    LibUnits       : string;
    LibGCC,
    LibOther       : boolean;
    InfoCfg,
    InfoDirs,
    InfoTools,
    InfoInstall,
    InfoObjects    : boolean;
    Section        : array[tsections] of boolean;
    ToolsPPDep,
    ToolsPPUMove,
    ToolsPPUFiles,
    ToolsData2Inc,
    ToolsSed,
    ToolsDiff,
    ToolsCmp,
    ToolsUpx,
    ToolsDate,
    ToolsZip       : boolean;
    MultiPacks,
    PreSettings,
    PostSettings,
    Rules          : TStringList;
  end;

  TMyMemoryStream=class(TMemoryStream)
  public
    constructor Create(p:pointer;mysize:integer);
  end;

var
  userini   : TFpcMake;
  fpcini    : TIniFile;
  IniStream : TMyMemoryStream;


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


function TargetStringEmpty(const t:TTargetsString):boolean;
var
  i : integer;
begin
  for i:=0 to targets do
   if t[i]<>'' then
    begin
      TargetStringEmpty:=false;
      exit;
    end;
  TargetStringEmpty:=true;
end;


procedure AddStrNoDup(var s:string;const s2:string);
var
  i : longint;
  add : boolean;
begin
  add:=false;
  i:=pos(s2,s);
  if (i=0) then
   add:=true
  else
   if (i=1) then
    begin
      if (length(s)>length(s2)) and
         (s[length(s2)+1]<>' ') then
       add:=true;
    end
  else
   if (i>1) and
      ((s[i-1]<>' ') or
       ((length(s)>=i+length(s2)) and (s[i+length(s2)]<>' '))) then
    add:=true;
  if add then
   begin
     if s='' then
      s:=s2
     else
      s:=s+' '+s2;
   end;
end;


{*****************************************************************************
                                  TMyMemoryStream
*****************************************************************************}

constructor TMyMemoryStream.Create(p:pointer;mysize:integer);
begin
  inherited Create;
  SetPointer(p,mysize);
end;


{*****************************************************************************
                               Makefile.fpc reading
*****************************************************************************}

function ReadMakefilefpc(const fn:string):boolean;
var
  ini : TIniFile;

  procedure ReadTargetsString(var t:Ttargetsstring;const sec,name,def:string);
  var
    i : integer;
  begin
    t[0]:=ini.ReadString(sec,name,def);
    for i:=1 to targets do
     t[i]:=ini.ReadString(sec,name+'_'+targetstr[i],'');
  end;

var
  sec : tsections;
begin
  ReadMakefilefpc:=false;

  Verbose('Reading '+fn);
  ini:=TIniFile.Create(fn);

  with userini,ini do
   begin
   { targets }
     ReadTargetsString(TargetDirs,ini_targets,'dirs','');
     ReadTargetsString(TargetLoaders,ini_targets,'loaders','');
     ReadTargetsString(TargetUnits,ini_targets,'units','');
     ReadTargetsString(TargetPrograms,ini_targets,'programs','');
     ReadTargetsString(TargetExamples,ini_targets,'examples','');
     ReadTargetsString(TargetRST,ini_targets,'rst','');
   { clean }
     ReadTargetsString(CleanUnits,ini_clean,'units','');
     ReadTargetsString(CleanFiles,ini_clean,'files','');
   { install }
     InstallPrefixDir:=ReadString(ini_install,'dirprefix','');
     InstallBaseDir:=ReadString(ini_install,'basedir','');
     InstallDataDir:=ReadString(ini_install,'datadir','');
     InstallUnitSubDir:=ReadString(ini_install,'unitsubdir','');
     ReadTargetsString(InstallUnits,ini_install,'units','');
     ReadTargetsString(InstallFiles,ini_install,'files','');
   { zip }
     ZipName:=ReadString(ini_zip,'zipname','');
     ZipTarget:=ReadString(ini_zip,'ziptarget','install');
   { defaults }
     DefaultDir:=ReadString(ini_defaults,'defaultdir','');
     DefaultBuildDir:=ReadString(ini_defaults,'defaultbuilddir','');
     DefaultInstallDir:=ReadString(ini_defaults,'defaultinstalldir','');
     DefaultCleanDir:=ReadString(ini_defaults,'defaultcleandir','');
     DefaultRule:=ReadString(ini_defaults,'defaultrule','all');
     DefaultTarget:=ReadString(ini_defaults,'defaulttarget','');
     DefaultCPU:=ReadString(ini_defaults,'defaultcpu','');
   { require }
     RequireRTL:=ReadBool(ini_require,'rtl',true);
     RequireOptions:=ReadString(ini_require,'options','');
     ReadTargetsString(requireComponents,ini_require,'components','');
     ReadTargetsString(requirePackages,ini_require,'packages','');
   { dirs }
     DirFpc:=ReadString(ini_dirs,'fpcdir','');
     DirPackage:=ReadString(ini_dirs,'packagedir','$(FPCDIR)/packages');
     DirComponent:=ReadString(ini_dirs,'componentdir','$(FPCDIR)/components');
     DirUnit:=ReadString(ini_dirs,'unitdir','');
     DirLib:=ReadString(ini_dirs,'libdir','');
     DirObj:=ReadString(ini_dirs,'objdir','');
     DirTarget:=ReadString(ini_dirs,'targetdir','');
     DirSources:=ReadString(ini_dirs,'sourcesdir','');
     DirUnitTarget:=ReadString(ini_dirs,'unittargetdir','');
     DirInc:=ReadString(ini_dirs,'incdir','');
   { libs }
     LibName:=ReadString(ini_libs,'libname','');
     LibUnits:=ReadString(ini_libs,'libunits','');
     LibGcc:=ReadBool(ini_libs,'libgcc',false);
     LibOther:=ReadBool(ini_libs,'libother',false);
   { tools }
     ToolsPPDep:=ReadBool(ini_tools,'toolppdep',true);
     ToolsPPUMove:=ReadBool(ini_tools,'toolppumove',true);
     ToolsPPUFiles:=ReadBool(ini_tools,'toolppufiles',true);
     ToolsSed:=ReadBool(ini_tools,'toolsed',false);
     ToolsData2Inc:=ReadBool(ini_tools,'tooldata2inc',false);
     ToolsDiff:=ReadBool(ini_tools,'tooldiff',false);
     ToolsCmp:=ReadBool(ini_tools,'toolcmp',false);
     ToolsUpx:=ReadBool(ini_tools,'toolupx',true);
     ToolsDate:=ReadBool(ini_tools,'tooldate',true);
     ToolsZip:=ReadBool(ini_tools,'toolzip',true);
   { sections, but allow overriding the 'none' option to include a few sects only }
     for sec:=low(tsections) to high(tsections) do
      Section[sec]:=sectiondef[sec];
     Section[sec_None]:=ReadBool(ini_sections,sectionstr[sec_none],sectiondef[sec_none]);
     if Section[sec_None] then
      FillChar(Section,sizeof(Section),0);
     for sec:=low(tsections) to high(tsections) do
      Section[sec]:=ReadBool(ini_sections,sectionstr[sec],section[sec]);
     { turn on needed sections }
     if not TargetStringEmpty(TargetLoaders) then
      section[sec_loaders]:=true;
     if not TargetStringEmpty(TargetUnits) then
      section[sec_units]:=true;
     if not TargetStringEmpty(TargetPrograms) then
      section[sec_exes]:=true;
     if not TargetStringEmpty(TargetExamples) then
      section[sec_examples]:=true;
   { info }
     InfoCfg:=ReadBool(ini_info,'infoconfig',true);
     InfoDirs:=ReadBool(ini_info,'infodirs',false);
     InfoTools:=ReadBool(ini_info,'infotools',false);
     InfoInstall:=ReadBool(ini_info,'infoinstall',true);
     InfoObjects:=ReadBool(ini_info,'infoobjects',true);
   { multipacks }
     MultiPacks:=TStringList.Create;
     ReadSectionRaw('multipack',MultiPacks);
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
   fn:='';
  if fn='' then
   begin
     Verbose('Opening internal ini');
     IniStream:=TMyMemoryStream.Create(@fpcmakeini,sizeof(fpcmakeini));
     result:=TIniFile.Create(IniStream);
   end
  else
   begin
     Verbose('Opening '+fn);
     result:=TIniFile.Create(fn);
   end;
end;


{*****************************************************************************
                               Makefile writing
*****************************************************************************}

function WriteMakefile(const fn:string):boolean;
var
  mf : TStringList;
  ss : TStringList;
  Phony : string;

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

  procedure AddHead(const s:string);
  begin
    mf.Add('');
    mf.Add('# '+s);
    mf.Add('');
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

  procedure AddRule(rule:integer);
  var
    i : integer;
    hs : string;
  begin
    i:=0;
    while (i<userini.rules.Count) do
     begin
       if (length(userini.rules[i])>length(rulestr[rule])) and
          (userini.rules[i][1]=rulestr[rule][1]) and
          ((userini.rules[i][length(rulestr[rule])+1]=':') or
           ((length(userini.rules[i])>length(rulestr[rule])+1) and
            (userini.rules[i][length(rulestr[rule])+2]=':'))) and
          (Copy(userini.rules[i],1,length(rulestr[rule]))=rulestr[rule]) then
         exit;
       inc(i);
     end;
    hs:='';
    if userini.section[rule2sec[rule]] then
     hs:=hs+' fpc_'+rulestr[rule];
    if userini.DefaultDir<>'' then
     hs:=hs+' $(addsuffix _'+rulestr[rule]+','+userini.defaultdir+')'
    else
     if (userini.DefaultBuildDir<>'') and (rule2bic[rule]=bic_build) then
      hs:=hs+' $(addsuffix _'+rulestr[rule]+','+userini.defaultbuilddir+')'
    else
     if (userini.DefaultInstallDir<>'') and (rule2bic[rule]=bic_install) then
      hs:=hs+' $(addsuffix _'+rulestr[rule]+','+userini.defaultinstalldir+')'
    else
     if (userini.DefaultCleanDir<>'') and (rule2bic[rule]=bic_clean) then
      hs:=hs+' $(addsuffix _'+rulestr[rule]+','+userini.defaultcleandir+')'
    else
     if rulediralso[rule] and (not TargetStringEmpty(userini.targetdirs)) then
      hs:=hs+' $(addsuffix _'+rulestr[rule]+',$(DIROBJECTS))';
    if hs<>'' then
     begin
       Phony:=Phony+' '+rulestr[rule];
       mf.Add(rulestr[rule]+':'+hs);
       mf.Add('');
     end;
  end;

  procedure AddTargets(const pre:string;var t:TTargetsString;wildcard:boolean);
  var
    i : integer;
  begin
    for i:=0 to targets do
     if (t[i]<>'') then
      begin
        if i<>0 then
         mf.Add('ifeq ($(OS_TARGET),'+targetstr[i]+')');
        if t[i]<>'' then
         begin
           if wildcard then
            mf.Add('override '+pre+'+=$(wildcard '+t[i]+')')
           else
            mf.Add('override '+pre+'+='+t[i]);
         end;
        if i<>0 then
         mf.Add('endif');
      end;
  end;

  procedure AddTargetsUnitDir(const packpre:string;var t:TTargetsString);
  var
    i,j,k : integer;
    hs,pack,packdirvar,unitdirvar,unitdir,packdir : string;
  begin
    for i:=0 to targets do
     if (t[i]<>'') then
      begin
        hs:=t[i];
        repeat
          j:=pos(' ',hs);
          if j=0 then
           j:=length(hs)+1;
          pack:=Copy(hs,1,j-1);
          packdirvar:='PACKAGEDIR_'+Uppercase(pack);
          unitdirvar:='UNITDIR_'+Uppercase(pack);
          packdir:=packpre+'/'+pack;
          unitdir:='$(UNITSDIR)/'+pack;
          for k:=1to specialdirs do
           begin
             if specialdir[k].dir=pack then
              begin
                packdir:=specialdir[k].packdir;
                unitdir:=specialdir[k].unitdir;
                break;
              end;
           end;
          mf.Add('ifneq ($(wildcard '+packdir+'),)');
          mf.Add('ifneq ($(wildcard '+packdir+'/$(OS_TARGET)),)');
          mf.Add(packdirvar+'='+packdir+'/$(OS_TARGET)');
          mf.Add('else');
          mf.Add(packdirvar+'='+packdir);
          mf.Add('endif');
          mf.Add(unitdirvar+'=$('+packdirvar+')');
          mf.Add('else');
          mf.Add(packdirvar+'=');
          mf.Add('ifneq ($(wildcard '+unitdir+'),)');
          mf.Add('ifneq ($(wildcard '+unitdir+'/$(OS_TARGET)),)');
          mf.Add(unitdirvar+'='+unitdir+'/$(OS_TARGET)');
          mf.Add('else');
          mf.Add(unitdirvar+'='+unitdir);
          mf.Add('endif');
          mf.Add('else');
          mf.Add(unitdirvar+'=');
          mf.Add('endif');
          mf.Add('endif');
          mf.Add('ifdef '+unitdirvar);
          mf.Add('override NEEDUNITDIR+=$('+unitdirvar+')');
          mf.Add('endif');
          system.delete(hs,1,j);
        until hs='';
      end;
  end;

  procedure AddTargetDir(const s:string);
  var
    j  : integer;
    hs : string;
  begin
    AddHead('Dir '+s);
    mf.Add('ifdef OBJECTDIR'+Uppercase(s));
    hs:='.PHONY: ';
    for j:=1to rules do
     hs:=hs+' '+s+'_'+rulestr[j];
    mf.Add(hs);
    mf.Add('');
    for j:=1to rules do
     begin
       mf.Add(s+'_'+rulestr[j]+':');
       mf.Add(#9+'$(MAKE) -C '+s+' '+rulestr[j]);
       if j<rules then
        mf.Add('');
     end;
    mf.Add('endif');
  end;

  procedure AddPackageDep(const s:string);
  var
    packagedir : string;
  begin
    packagedir:='$(PACKAGEDIR_'+Uppercase(s)+')';
    mf.Add('ifdef PACKAGE'+Uppercase(s));
    mf.Add('ifneq ($(wildcard '+packagedir+'),)');
    mf.Add('ifeq ($(wildcard '+packagedir+'/$(FPCMADE)),)');
    mf.Add('override COMPILEPACKAGES+='+s);
    mf.Add(s+'_package:');
    mf.Add(#9'$(MAKE) -C '+packagedir+' all');
    mf.Add('endif');
    mf.Add('endif');
    mf.Add('endif');
    Phony:=Phony+' '+s+'_package';
  end;

  function AddTargetDefines(const ts:TTargetsString;const prefix:string):string;
  var
    j,i : integer;
    hs,hs2 : string;
  begin
    hs2:='';
    for j:=0 to targets do
     if (ts[j]<>'') then
      begin
        if j<>0 then
         mf.Add('ifeq ($(OS_TARGET),'+targetstr[j]+')');
        hs:=ts[j];
        repeat
          i:=pos(' ',hs);
          if i=0 then
           i:=length(hs)+1;
          mf.Add(prefix+Uppercase(Copy(hs,1,i-1))+'=1');
          { add to the list of dirs without duplicates }
          AddStrNoDup(hs2,Copy(hs,1,i-1));
          system.delete(hs,1,i);
        until hs='';
        if j<>0 then
         mf.Add('endif');
      end;
     AddTargetDefines:=hs2;
  end;

var
  hs  : string;
  i : integer;
begin
{ Open the Makefile }
  mf:=TStringList.Create;
{ Buffer for reading and writing the sections }
  ss:=TStringList.Create;

  with mf do
   begin
   { write header & autodetection }
     Add('#');
     Add('# Makefile generated by '+titledate);
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

   { fpc dir }
     AddSection(true,'fpcdircheckenv');
     if userini.dirfpc<>'' then
      begin
        Add('# Default FPCDIR');
        Add('ifeq ($(FPCDIR),wrong)');
        Add('override FPCDIR='+userini.dirfpc);
        Add('ifeq ($(wildcard $(FPCDIR)/rtl),)');
        Add('override FPCDIR=wrong');
        Add('endif');
        Add('endif');
        Add('');
      end;
     AddSection(true,'fpcdirdetect');

   { fpcdir subdirs }
     Add('ifndef PACKAGEDIR');
     Add('PACKAGEDIR='+userini.dirpackage);
     Add('endif');
     Add('ifndef COMPONENTDIR');
     Add('COMPONENTDIR='+userini.dircomponent);
     Add('endif');
     AddSection(true,'fpcdirsubs');

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
     AddTargets('DIROBJECTS',userini.targetdirs,true);
     AddTargets('LOADEROBJECTS',userini.targetloaders,false);
     AddTargets('UNITOBJECTS',userini.targetunits,false);
     AddTargets('EXEOBJECTS',userini.targetprograms,false);
     AddTargets('EXAMPLEOBJECTS',userini.targetexamples,false);
     AddTargets('RSTOBJECTS',userini.targetrst,false);

   { Clean }
     AddHead('Clean');
     AddTargets('EXTRACLEANUNITS',userini.cleanunits,false);
     AddTargets('EXTRACLEANFILES',userini.cleanfiles,false);

   { Install }
     AddHead('Install');
     AddTargets('EXTRAINSTALLUNITS',userini.installunits,false);
     AddTargets('EXTRAINSTALLFILES',userini.installfiles,false);
     if userini.installprefixdir<>'' then
      Add('PREFIXINSTALLDIR='+userini.installprefixdir);
     if userini.installbasedir<>'' then
      Add('BASEINSTALLDIR='+userini.installbasedir);
     if userini.installdatadir<>'' then
      Add('DATAINSTALLDIR='+userini.installdatadir);
     if userini.InstallUnitSubDir<>'' then
      Add('UNITSUBDIR='+userini.InstallUnitSubDir);

   { Zip }
     if userini.zipname<>'' then
      Add('ZIPNAME='+userini.zipname);
     if userini.ziptarget<>'' then
      Add('ZIPTARGET='+userini.ziptarget);

   { Defaults }
     AddHead('Defaults');
     if userini.Requireoptions<>'' then
      Add('override NEEDOPT='+userini.Requireoptions);

   { Dirs }
     AddHead('Directories');
     if userini.dirsources<>'' then
      Add('vpath %$(PASEXT) '+userini.dirsources);
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

   { Packages }
     AddHead('Packages');
     if userini.RequireRTL then
      begin
        Add('override PACKAGES=rtl');
        Add('PACKAGEDIR_RTL=$(FPCDIR)/rtl/$(OS_TARGET)');
      end;
     AddTargets('PACKAGES',userini.Requirepackages,false);
     AddTargets('COMPONENTS',userini.Requirecomponents,false);
     AddTargetsUnitDir('$(PACKAGEDIR)',userini.Requirepackages);
     AddTargetsUnitDir('$(COMPONENTDIR)',userini.Requirecomponents);

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
     if userini.Section[sec_Info] then
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
        Add('INFOTARGET='+hs);
      end;

   { Post Settings }
     if userini.PostSettings.count>0 then
      begin
        AddHead('Post Settings');
        AddStrings(userini.PostSettings);
      end;
     Add('');

   { write dirs }
     if userini.section[sec_dirs] then
      begin
        AddSection(true,'dir_default');
        AddSection(userini.libgcc,'dir_gcclib');
        AddSection(userini.libother,'dir_otherlib');
        AddSection(true,'dir_install');
      end;

   { commandline }
     if userini.section[sec_command] then
      begin
        Add('');
        AddSection(true,'command_begin');
        AddSection((userini.Requireoptions<>''),'command_needopt');
        AddSection((userini.dirfpc<>''),'command_unitsdir');
        AddSection((userini.dirfpc<>''),'command_rtldir');
        AddSection((userini.dirunit<>'') or
                   (not TargetStringEmpty(userini.Requirepackages)) or
                   (not TargetStringEmpty(userini.Requirecomponents))
                   ,'command_needunit');
        AddSection((userini.dirlib<>''),'command_needlib');
        AddSection((userini.dirobj<>''),'command_needobj');
        AddSection((userini.dirinc<>''),'command_needinc');
        AddSection(userini.libgcc,'command_gcclib');
        AddSection(userini.libother,'command_otherlib');
        AddSection((userini.dirinc<>''),'command_inc');
        AddSection((userini.dirtarget<>''),'command_target');
        AddSection((userini.dirunittarget<>''),'command_unittarget');
        AddSection(true,'command_end');
      end;

   { write tools }
     if userini.section[sec_tools] then
      begin
        AddSection(true,'shelltools');
        AddSection(true,'tool_default');
        AddSection(userini.toolsppdep,'tool_ppdep');
        AddSection(userini.toolsppumove,'tool_ppumove');
        AddSection(userini.toolsppufiles,'tool_ppufiles');
        AddSection(userini.toolsdata2inc,'tool_data2inc');
        AddSection(userini.toolsupx,'tool_upx');
        AddSection(userini.toolssed,'tool_sed');
        AddSection(userini.toolsdate,'tool_date');
        AddSection(userini.toolszip,'tool_zip');
        AddSection(userini.toolscmp,'tool_cmp');
        AddSection(userini.toolsdiff,'tool_diff');
      end;

   { extensions }
     if userini.section[sec_exts] then
      AddSection(true,'extensions');

   { add default rules }
     AddSection(true,'standardrules');
     Phony:='';
     for i:=1 to rules do
      AddRule(i);
     if Phony<>'' then
      begin
        Add('.PHONY: '+Phony);
        Add('');
      end;

   { Package requirements, must be before the other rules so it's done first }
     AddSection(true,'packagerequirerules');
     Phony:='';
     if userini.RequireRTL then
      AddPackageDep('rtl');
     Add('');
     if not TargetStringEmpty(userini.RequirePackages) then
      begin
        hs:=AddTargetDefines(userini.RequirePackages,'PACKAGE');
        repeat
          i:=pos(' ',hs);
          if i=0 then
           i:=length(hs)+1;
          AddPackageDep(Copy(hs,1,i-1));
          system.delete(hs,1,i);
        until hs='';
        Add('');
      end;
     if Phony<>'' then
      begin
        Add('.PHONY: '+Phony);
        Add('');
      end;

   { Components }
     Phony:='';
     if not TargetStringEmpty(userini.RequireComponents) then
      begin
        hs:=AddTargetDefines(userini.RequireComponents,'COMPONENT');
        repeat
          i:=pos(' ',hs);
          if i=0 then
           i:=length(hs)+1;
          AddPackageDep(Copy(hs,1,i-1));
          system.delete(hs,1,i);
        until hs='';
        Add('');
      end;
     if Phony<>'' then
      begin
        Add('.PHONY: '+Phony);
        Add('');
      end;

   { compile rules for making loaders/units/exes/examples }
     AddSection(not TargetStringEmpty(userini.targetloaders),'loaderrules');
     AddSection(not TargetStringEmpty(userini.targetunits),'unitrules');
     AddSection(not TargetStringEmpty(userini.targetprograms),'exerules');
     AddSection(not TargetStringEmpty(userini.targetexamples),'examplerules');
     AddSection(not TargetStringEmpty(userini.targetrst),'rstrules');

   { default fpc_ rules }
     AddSection(userini.Section[sec_Compile],'compilerules');
     AddSection(userini.Section[sec_Libs],'libraryrules');
     AddSection(userini.Section[sec_Install],'installrules');
     AddSection(userini.Section[sec_SourceInstall],'sourceinstallrules');
     AddSection(userini.Section[sec_ZipInstall],'zipinstallrules');
     AddSection(userini.Section[sec_Clean],'cleanrules');
     AddSection(userini.Section[sec_require],'requirerules');
     if userini.Section[sec_Info] then
      begin
        AddSection(true,'inforules');
        AddSection(userini.infocfg,'info_cfg');
        AddSection(userini.infodirs,'info_dirs');
        AddSection(userini.infotools,'info_tools');
        AddSection(userini.infoobjects,'info_objects');
        AddSection(userini.infoinstall,'info_install');
      end;

   { Target dirs }
     if not TargetStringEmpty(userini.targetdirs) then
      begin
        AddHead('Target Dirs');
        hs:=AddTargetDefines(userini.targetdirs,'OBJECTDIR');
        repeat
          i:=pos(' ',hs);
          if i=0 then
           i:=length(hs)+1;
          AddTargetDir(Copy(hs,1,i-1));
          system.delete(hs,1,i);
        until hs='';
        Add('');
      end;

   { insert users rules }
     if userini.rules.count>0 then
      begin
        AddSection(true,'userrules');
        AddStrings(userini.rules);
      end;
   end;

{ Write the Makefile and cleanup }
  Verbose('Writing '+fn);
  FixTab(mf);
  mf.SaveToFile(fn);
  mf.Destroy;
  ss.Destroy;
  WriteMakefile:=true;
end;


procedure UseMakefilefpc;
var
  fn : string;
begin
  if FileExists('Makefile.fpc') then
   fn:='Makefile.fpc'
  else
   fn:='makefile.fpc';
{ Open Makefile.fpc }
  if not ReadMakefilefpc(fn) then
   Error('Can''t read '+fn);
{ Write Makefile }
  if not WriteMakefile('Makefile') then
   Error('Can''t write Makefile');
end;


procedure UseParameters;
var
  i  : integer;
  fn : string;
begin
  for i:=1 to ParamCount do
   begin
     fn:=ParamStr(i);
     { Open Makefile.fpc }
     if not ReadMakefilefpc(fn) then
      Error('Can''t read '+fn);
     { Write Makefile }
     if not WriteMakefile(ExtractFilePath(fn)+'Makefile') then
      Error('Can''t write '+ExtractFilePath(fn)+'Makefile');
   end;
end;


begin
{ Open userini.ini }
  fpcini:=ReadFpcMakeIni;
  if not assigned(fpcini) then
   Error('Can''t read fpcmake.ini');

  if ParamCount=0 then
   UseMakefilefpc
  else
   UseParameters;

  fpcini.destroy;
end.
{
  $Log$
  Revision 1.18  2000-01-06 01:29:59  peter
    * FPCDIR setting/detect
    * lot of other updates to create .deb files correctly

  Revision 1.17  2000/01/04 00:00:23  peter
    * Makefile updates again

  Revision 1.16  2000/01/03 19:42:41  peter
    * regenerated

  Revision 1.15  1999/12/23 19:32:28  peter
    * automatic support for package/target dir structure

  Revision 1.14  1999/12/23 13:52:23  peter
    + default[install,build,clean]dir

  Revision 1.13  1999/12/21 16:06:47  peter
    * don't call dirobjects for zipisntall,info

  Revision 1.12  1999/12/19 15:15:04  peter
    * fpcmade.<TARGET> added
    * parameter support. So it can be using with "find -name 'Makefile.fpc'"

  Revision 1.11  1999/12/02 11:30:24  peter
    * better dup checking

  Revision 1.10  1999/11/26 00:20:15  peter
    * fpcmake updated

  Revision 1.9  1999/11/25 20:23:01  peter
    * package requireencies

  Revision 1.8  1999/11/24 23:53:00  peter
    * packages
    * lot of other changes

  Revision 1.7  1999/11/23 09:43:35  peter
    + internal .ini file
    + packages support
    * ppufiles,data2inc support

  Revision 1.6  1999/11/10 22:10:49  peter
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
