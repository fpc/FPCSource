{
    $Id$
    Copyright (c) 1999-2000 by Peter Vreman

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
    sec_compile,sec_require,sec_install,sec_sourceinstall,sec_zipinstall,sec_zipsourceinstall,
    sec_clean,sec_libs,sec_command,sec_exts,sec_dirs,sec_tools,sec_info
  );

  tbic=(bic_none,bic_build,bic_install,bic_zipinstall,bic_clean);

  tspecialdir=record
    dir,unitdir,packdir : string;
  end;


const
  EnvVar='FPCMAKEINI';

  sectionstr : array[tsections] of string=('none',
    'units','exes','loaders','examples','package',
    'compile','require','install','sourceinstall','zipinstall','zipsourceinstall',
    'clean','libs','command','exts','dirs','tools','info'
  );

  sectiondef : array[tsections] of boolean=(false,
    true,true,false,false,false,
    true,false,true,true,true,true,
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
    'showinstall','install','sourceinstall','zipinstall','zipsourceinstall',
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
    bic_install,bic_install,bic_install,bic_zipinstall,bic_zipinstall,
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
    TargetPkgs,
    TargetRST      : TTargetsString;
    InstallSourceSubdirs : boolean;
    InstallPackageName,
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
    DefaultZipInstallDir,
    DefaultCleanDir,
    DefaultDir,
    DefaultTarget,
    DefaultCPU     : string;
    DirFpc,
    DirPackage,
    DirToolkit,
    DirComponent,
    DirUnit,
    DirLib,
    DirObj,
    DirTarget,
    DirUnitTarget,
    DirSources,
    DirInc          : string;
    RequireRTL      : boolean;
    RequireOptions  : string;
    RequireToolkits,
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
    ToolsZip,
    ToolsTar       : boolean;
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


function posidx(const substr,s : string;idx:integer):integer;
var
  i,j : integer;
  e   : boolean;
begin
  i := idx;
  j := 0;
  e:=(length(SubStr)>0);
  while e and (i<=Length(s)-Length(SubStr)) do
   begin
     inc(i);
     if (SubStr[1]=s[i]) and (Substr=Copy(s,i,Length(SubStr))) then
      begin
        j:=i;
        e:=false;
      end;
   end;
  PosIdx:=j;
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
  i,idx : longint;
  again,add : boolean;
begin
  add:=false;
  idx:=0;
  repeat
    again:=false;
    i:=posidx(s2,s,idx);
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
      begin
        idx:=i+length(s2);
        again:=true;
      end;
  until not again;
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
  b   : boolean;
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
     ReadTargetsString(TargetPkgs,ini_targets,'pkgs','');
     if not TargetStringEmpty(TargetPkgs) then
      TargetPkgs[0]:='all '+TargetPkgs[0];
   { clean }
     ReadTargetsString(CleanUnits,ini_clean,'units','');
     ReadTargetsString(CleanFiles,ini_clean,'files','');
   { install }
     InstallPackageName:=ReadString(ini_install,'packagename','');
     InstallPrefixDir:=ReadString(ini_install,'dirprefix','');
     InstallBaseDir:=ReadString(ini_install,'basedir','');
     InstallDataDir:=ReadString(ini_install,'datadir','');
     InstallUnitSubDir:=ReadString(ini_install,'unitsubdir','');
     InstallSourceSubdirs:=ReadBool(ini_install,'sourcesubdirs',true);
     ReadTargetsString(InstallUnits,ini_install,'units','');
     ReadTargetsString(InstallFiles,ini_install,'files','');
   { zip }
     ZipName:=ReadString(ini_zip,'zipname','');
     ZipTarget:=ReadString(ini_zip,'ziptarget','install');
   { defaults }
     DefaultDir:=ReadString(ini_defaults,'defaultdir','');
     DefaultBuildDir:=ReadString(ini_defaults,'defaultbuilddir','');
     DefaultInstallDir:=ReadString(ini_defaults,'defaultinstalldir','');
     DefaultZipInstallDir:=ReadString(ini_defaults,'defaultzipinstalldir','');
     DefaultCleanDir:=ReadString(ini_defaults,'defaultcleandir','');
     DefaultRule:=ReadString(ini_defaults,'defaultrule','all');
     DefaultTarget:=ReadString(ini_defaults,'defaulttarget','');
     DefaultCPU:=ReadString(ini_defaults,'defaultcpu','');
   { require }
     b:=not(TargetStringEmpty(targetunits) and TargetStringEmpty(targetprograms) and TargetStringEmpty(targetexamples));
     RequireRTL:=ReadBool(ini_require,'rtl',b);
     RequireOptions:=ReadString(ini_require,'options','');
     ReadTargetsString(requireToolkits,ini_require,'toolkit','');
     ReadTargetsString(requirePackages,ini_require,'packages','');
     ReadTargetsString(requireComponents,ini_require,'components','');
     if userini.RequireRTL then
      begin
        if userini.Requirepackages[0]<>'' then
         userini.Requirepackages[0]:='rtl '+userini.Requirepackages[0]
        else
         userini.Requirepackages[0]:='rtl';
      end;
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
     { dependencies }
     if section[sec_zipsourceinstall] then
      begin
        section[sec_zipinstall]:=true;
        section[sec_sourceinstall]:=true;
      end;
     if section[sec_sourceinstall] then
      begin
        section[sec_tools]:=true;
        section[sec_dirs]:=true;
      end;
   { dirs }
     DirFpc:=ReadString(ini_dirs,'fpcdir','');
     DirPackage:=ReadString(ini_dirs,'packagedir','$(FPCDIR)/packages');
     DirToolkit:=ReadString(ini_dirs,'toolkitdir','');
     DirComponent:=ReadString(ini_dirs,'componentdir','');
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
     ToolsPPUMove:=ReadBool(ini_tools,'toolppumove',section[sec_compile]);
     ToolsPPUFiles:=ReadBool(ini_tools,'toolppufiles',section[sec_install] or section[sec_clean]);
     ToolsUpx:=ReadBool(ini_tools,'toolupx',section[sec_install]);
     ToolsZip:=ReadBool(ini_tools,'toolzip',section[sec_zipinstall]);
     ToolsTar:=ReadBool(ini_tools,'tooltar',section[sec_zipinstall]);
     ToolsPPDep:=ReadBool(ini_tools,'toolppdep',false);
     ToolsSed:=ReadBool(ini_tools,'toolsed',false);
     ToolsData2Inc:=ReadBool(ini_tools,'tooldata2inc',false);
     ToolsDiff:=ReadBool(ini_tools,'tooldiff',false);
     ToolsCmp:=ReadBool(ini_tools,'toolcmp',false);
     ToolsDate:=ReadBool(ini_tools,'tooldate',false);
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

function VarName(const s:string):string;
var
  i,j : longint;
begin
  i:=0;
  result:=s;
  while i<length(result) do
   begin
     inc(i);
     case result[i] of
       '{' :
         begin
           { this are pkgs which are hold the dirs between the accolades }
           j:=PosIdx('}',result,i);
           if j>0 then
            Delete(result,i,j-i+1)
           else
            Delete(result,i,1);
           dec(i);
         end;
       '$','(',')' :
         begin
           Delete(result,i,1);
           dec(i);
         end;
       'a'..'z' :
         result[i]:=chr(ord(result[i])-32);
     end;
   end;
end;

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
    { zipinstall is special, it allows packages }
    if (not userini.installsourcesubdirs) and
       ((rulestr[rule]='zipsourceinstall') or (rulestr[rule]='sourceinstall')) then
     begin
       if userini.section[rule2sec[rule]] then
        hs:=hs+' fpc_'+rulestr[rule];
     end
    else
     if ((rulestr[rule]='zipinstall') or
         (rulestr[rule]='zipsourceinstall')) and
        (not TargetStringEmpty(userini.targetpkgs)) then
      hs:=hs+' $(addsuffix _'+rulestr[rule]+',$(PKGOBJECTS))'
    else
     begin
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
        if (userini.DefaultZipInstallDir<>'') and (rule2bic[rule]=bic_zipinstall) then
         hs:=hs+' $(addsuffix _'+rulestr[rule]+','+userini.defaultzipinstalldir+')'
       else
        if (userini.DefaultCleanDir<>'') and (rule2bic[rule]=bic_clean) then
         hs:=hs+' $(addsuffix _'+rulestr[rule]+','+userini.defaultcleandir+')'
       else
        if rulediralso[rule] and (not TargetStringEmpty(userini.targetdirs)) then
         hs:=hs+' $(addsuffix _'+rulestr[rule]+',$(DIROBJECTS))';
     end;
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

  procedure AddTargetDir(const s:string);
  var
    j  : integer;
    hs : string;
  begin
    AddHead('Dir '+s);
    mf.Add('ifdef OBJECTDIR'+VarName(s));
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

  function AddTargetDefines(const ts:TTargetsString;const prefix:string):string;
  var
    k1,k2,k3,j,i : integer;
    name,hs,hs2,hs3 : string;
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
          name:=Copy(hs,1,i-1);
          k1:=pos('{',name);
          if k1>0 then
           begin
             k2:=PosIdx('}',name,k1);
             if k2=0 then
              begin
                Writeln('Error: missing closing } for "',name,'"');
                k2:=length(name)+1;
              end;
             hs3:=Copy(name,k1+1,k2-k1-1);
             for k3:=1to length(hs3) do
              if hs3[k3]=',' then
               hs3[k3]:=' ';
             name:=Copy(name,1,k1-1);
             mf.Add(prefix+VarName(name)+'='+hs3);
           end
          else
           mf.Add(prefix+VarName(name)+'=1');
          { add to the list of dirs without duplicates }
          AddStrNoDup(hs2,name);
          system.delete(hs,1,i);
        until hs='';
        if j<>0 then
         mf.Add('endif');
      end;
     AddTargetDefines:=hs2;
  end;

  procedure AddPackage(const path,pack:string);
  var
    k : integer;
    packdirvar,unitdirvar,unitdir,packdir : string;
  begin
    mf.Add('ifdef PACKAGE'+VarName(pack));
    { create needed variables }
    packdirvar:='PACKAGEDIR_'+VarName(pack);
    unitdirvar:='UNITDIR_'+VarName(pack);
    packdir:=path+'/'+pack;
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
    { Use Package dir, add build rules }
    mf.Add('ifneq ($(wildcard '+packdir+'/$(OS_TARGET)),)');
    mf.Add(packdirvar+'='+packdir+'/$(OS_TARGET)');
    mf.Add('else');
    mf.Add(packdirvar+'='+packdir);
    mf.Add('endif');
    mf.Add('ifeq ($(wildcard $('+packdirvar+')/$(FPCMADE)),)');
    mf.Add('override COMPILEPACKAGES+=package_'+pack);
    Phony:=Phony+'package_'+pack;
    mf.Add('package_'+pack+':');
    mf.Add(#9'$(MAKE) -C $('+packdirvar+') all');
    mf.Add('endif');
    mf.Add(unitdirvar+'=$('+packdirvar+')');
    mf.Add('else');
    { Package dir doesn''t exists, create unit dir }
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
    { Add Unit dir to the command line -Fu }
    mf.Add('ifdef '+unitdirvar);
    mf.Add('override NEEDUNITDIR+=$('+unitdirvar+')');
    mf.Add('endif');
    mf.Add('endif');
  end;

  procedure AddTargetsPackages(const path:string;const ts:TTargetsString);
  var
    Phony,hs : string;
    i  : integer;
  begin
   { Components }
     Phony:='';
     if not TargetStringEmpty(ts) then
      begin
        AddHead(VarName(path)+' packages');
        hs:=AddTargetDefines(ts,'PACKAGE');
        mf.Add('');
        repeat
          i:=pos(' ',hs);
          if i=0 then
           i:=length(hs)+1;
          AddPackage(path,Copy(hs,1,i-1));
          system.delete(hs,1,i);
        until hs='';
        mf.Add('');
      end;
     if Phony<>'' then
      begin
        mf.Add('.PHONY: '+Phony);
        mf.Add('');
      end;
  end;

  procedure AddPkg(const pack:string);
  var
    j  : integer;
    packname,pkgdirsvar,hs : string;
  begin
    packname:=pack;
    AddHead('Pkg '+packname);
    mf.Add('ifdef PKG'+VarName(packname));
    hs:='.PHONY: ';
    for j:=1 to rules do
     hs:=hs+' pkg'+packname+'_'+rulestr[j];
    mf.Add(hs);
    mf.Add('');
    { pkgall is special which processes all directories }
    if pack='all' then
     pkgdirsvar:='DIROBJECTS'
    else
     begin
       mf.Add('override PKGOBJECTS+=pkg'+packname);
       pkgdirsvar:='PKG'+VarName(packname);
     end;
    for j:=1to rules do
     if rulediralso[j] then
      begin
        mf.Add('pkg'+packname+'_'+rulestr[j]+': $(addsuffix _'+rulestr[j]+',$('+pkgdirsvar+'))');
        if j<rules then
         mf.Add('');
      end
     else
      begin
        { zipinstall is special for pkgs }
        if (rulestr[j]='zipinstall') then
         begin
           mf.Add('pkg'+packname+'_'+rulestr[j]+':');
           mf.Add(#9'$(MAKE) fpc_zipinstall PACKAGENAME='+packname+
                  ' ZIPTARGET=pkg'+packname+'_install');
           if j<rules then
            mf.Add('');
         end
        else
         if (rulestr[j]='zipsourceinstall') then
          begin
            mf.Add('pkg'+packname+'_'+rulestr[j]+':');
            mf.Add(#9'$(MAKE) fpc_zipinstall PACKAGENAME='+packname+
                   ' PACKAGESUFFIX=src ZIPTARGET=pkg'+packname+'_sourceinstall');
            if j<rules then
             mf.Add('');
          end;
      end;
    mf.Add('endif');
  end;

  procedure AddTargetsPkgs(const ts:TTargetsString);
  var
    Phony,hs : string;
    i  : integer;
  begin
   { Components }
     Phony:='';
     if not TargetStringEmpty(ts) then
      begin
        hs:=AddTargetDefines(ts,'PKG');
        mf.Add('');
        repeat
          i:=pos(' ',hs);
          if i=0 then
           i:=length(hs)+1;
          AddPkg(Copy(hs,1,i-1));
          system.delete(hs,1,i);
        until hs='';
        mf.Add('');
      end;
     if Phony<>'' then
      begin
        mf.Add('.PHONY: '+Phony);
        mf.Add('');
      end;
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

   { Pre Settings }
     if userini.PreSettings.count>0 then
      begin
        AddSection(true,'presettings');
        AddStrings(userini.PreSettings);
      end;

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
     if userini.RequireRTL then
      begin
        Add('ifndef PACKAGESDIR');
        Add('PACKAGESDIR='+userini.dirpackage);
        Add('endif');
        Add('ifndef TOOLKITSDIR');
        Add('TOOLKITSDIR='+userini.dirtoolkit);
        Add('endif');
        Add('ifndef COMPONENTSDIR');
        Add('COMPONENTSDIR='+userini.dircomponent);
        Add('endif');
        Add('');
        AddSection(true,'fpcdirsubs');
      end;

   { write the default & user settings }
     AddSection(true,'usersettings');

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
     if userini.installpackagename<>'' then
      Add('PACKAGENAME='+userini.installpackagename);

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
     Phony:='';
     AddTargets('PACKAGES',userini.Requirepackages,false);
     AddTargets('TOOLKITS',userini.Requiretoolkits,false);
     AddTargets('COMPONENTS',userini.Requirecomponents,false);
     if Phony<>'' then
      begin
        Add('.PHONY: '+Phony);
        Add('');
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
     Add('');

   { Post Settings }
     if userini.PostSettings.count>0 then
      begin
        AddSection(true,'postsettings');
        AddStrings(userini.PostSettings);
        Add('');
      end;

   { shell tools like copy,del,echo }
     AddSection(userini.section[sec_command] or userini.section[sec_tools],'shelltools');

   { write tools }
     AddSection(userini.section[sec_tools],'tool_default');
     AddSection(userini.toolsppdep,'tool_ppdep');
     AddSection(userini.toolsppumove,'tool_ppumove');
     AddSection(userini.toolsppufiles,'tool_ppufiles');
     AddSection(userini.toolsdata2inc,'tool_data2inc');
     AddSection(userini.toolsupx,'tool_upx');
     AddSection(userini.toolssed,'tool_sed');
     AddSection(userini.toolscmp,'tool_cmp');
     AddSection(userini.toolsdiff,'tool_diff');
     AddSection(userini.toolsdate,'tool_date');
     AddSection(userini.toolszip,'tool_zip');
     AddSection(userini.toolstar,'tool_tar');

   { extensions }
     if userini.section[sec_exts] then
      AddSection(true,'extensions');

   { package/component dirs }
     AddSection(true,'packagerequirerules');
     AddSection(userini.requirertl,'checkfpcdirsubs');
     AddTargetsPackages('$(PACKAGESDIR)',userini.Requirepackages);
     AddTargetsPackages('$(TOOLKITSDIR)',userini.Requiretoolkits);
     AddTargetsPackages('$(COMPONENTSDIR)',userini.Requirecomponents);
     Add('');

   { Pkgs }
     AddTargetsPkgs(userini.targetpkgs);

   { write dirs }
     AddSection(true,'dir_default');
     if userini.section[sec_dirs] then
      begin
        AddSection(userini.libgcc,'dir_gcclib');
        AddSection(userini.libother,'dir_otherlib');
        AddSection(true,'dir_install');
      end;

   { redirection }
     AddSection(true,'redir');

   { commandline }
     if userini.section[sec_command] then
      begin
        Add('');
        AddSection(true,'command_begin');
        AddSection((userini.Requireoptions<>''),'command_needopt');
        AddSection((userini.dirunit<>'') or
                   (not TargetStringEmpty(userini.Requiretoolkits)) or
                   (not TargetStringEmpty(userini.Requirepackages)) or
                   (not TargetStringEmpty(userini.Requirecomponents))
                   ,'command_needunit');
        AddSection(true,'command_unitsdir');
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
     AddSection(userini.Section[sec_ZipSourceInstall],'zipsourceinstallrules');
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
        AddSection(true,'directorytargets');
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

   { local makefile }
     AddSection(true,'localmakefile');

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
  Revision 1.26  2000-01-14 12:14:41  peter
    * sourceinstall updates

  Revision 1.25  2000/01/13 21:08:46  peter
    * zipsourcesinstall
    * zip fixes, bzip2 support with USETAR=bz2
    * multi pkg's support to include several dirs

  Revision 1.24  2000/01/13 11:34:26  peter
    * better package dep creation

  Revision 1.23  2000/01/12 23:20:37  peter
    * gecho support
    * use foreach to write fpcext.cmd
    * add fpcext.cmd to clean targets

  Revision 1.22  2000/01/10 22:55:49  peter
    * zipname creation from packagename

  Revision 1.21  2000/01/08 16:31:04  peter
    * support variable in packagenames
    * fpcmake.loc support
    * fixed place of presettings which must be before FPCDIR is set

  Revision 1.20  2000/01/07 16:46:02  daniel
    * copyright 2000

  Revision 1.19  2000/01/06 15:49:23  peter
    * rtldir removed, it's now handled like any other package

  Revision 1.18  2000/01/06 01:29:59  peter
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
