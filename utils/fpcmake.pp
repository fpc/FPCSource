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
  sysutils,classes,inifiles;

const
  Version='v0.99.13';
  Title='fpcmake '+Version;

const
  sec_dirs='dirs';
  sec_libs='libs';
  sec_targets='targets';
  sec_info='info';
  sec_misc='misc';
  sec_rules='rules';

type
  TFpcMake=record
    DefaultUnits   : boolean;
    TargetUnits,
    TargetPrograms : string;
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
    InfoDir,
    InfoTools,
    InfoInstall,
    InfoObjects,
    InfoFiles      : boolean;
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
     DefaultUnits:=ReadBool(sec_targets,'defaultunits',false);
     TargetUnits:=ReadString(sec_targets,'units','');
     TargetPrograms:=ReadString(sec_targets,'programs','');
   { dirs }
     DirUnit:=ReadString(sec_dirs,'unit','');
     DirLib:=ReadString(sec_dirs,'lib','');
     DirObj:=ReadString(sec_dirs,'obj','');
     DirTarget:=ReadString(sec_dirs,'target','');
     DirUnitTarget:=ReadString(sec_dirs,'unittarget','');
     DirInc:=ReadString(sec_dirs,'inc','');
     DirProcInc:=ReadString(sec_dirs,'procinc','');
     DirOSInc:=ReadString(sec_dirs,'osinc','');
   { libs }
     LibGcc:=ReadBool(sec_libs,'gcc',false);
     LibOther:=ReadBool(sec_libs,'other',false);
   { info }
     InfoCfg:=ReadBool(sec_info,'config',true);
     InfoDir:=ReadBool(sec_info,'dir',false);
     InfoTools:=ReadBool(sec_info,'tools',false);
     InfoInstall:=ReadBool(sec_info,'install',true);
     InfoObjects:=ReadBool(sec_info,'objects',true);
     InfoFiles:=ReadBool(sec_info,'files',false);
   { rules }
     rules:=TStringList.Create;
     ReadSectionRaw(sec_rules,rules);
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
  if FileExists('userini.ini') then
   fn:='userini.ini'
  else
{$ifdef linux}
   if FileExists('/usr/lib/fpc/userini.ini') then
    fn:='/usr/lib/fpc/userini.ini'
{$else}
   if FileExists(paramstr(0)+'/userini.ini') then
    fn:=paramstr(0)+'/userini.ini'
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
       if sl[i][1] in [' ',#9] then
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
       if (userini.rules[i][1]=s[1]) and
          (Copy(userini.rules[i],1,length(s))=s) then
         exit;
       inc(i);
     end;
    mf.Add(s+': fpc_'+s);
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
     Add('# Makefile generated from Makefile.fpc by '+Title);
     Add('#');
     Add('');
     AddSection(true,'osdetect');
     AddSection(true,'fpcdetect');

   { write the default & user settings }
     AddSection(true,'defaultsettings');
     AddSection(true,'usersettings');

   { Targets }
     if userini.defaultunits then
      Add('DEFAULTUNITS=1');
     Add('UNITOBJECTS='+userini.targetunits);
     Add('EXEOBJECTS='+userini.targetprograms);

   { Dirs }
     if userini.dirunit<>'' then
      Add('NEEDUNITDIR='+userini.dirunit);
     if userini.dirlib<>'' then
      Add('NEEDLIBDIR='+userini.dirlib);
     if userini.dirobj<>'' then
      Add('NEEDOBJDIR='+userini.dirobj);
     if userini.dirinc<>'' then
      Add('INC='+userini.dirinc);
     if userini.dirprocinc<>'' then
      Add('PROCINC='+userini.dirprocinc);
     if userini.dirosinc<>'' then
      Add('OSINC='+userini.dirosinc);
     if userini.dirtarget<>'' then
      Add('TARGETDIR='+userini.dirtarget);
     if userini.dirunittarget<>'' then
      Add('UNITTARGETDIR='+userini.dirunittarget);

   { Libs }
     if userini.libgcc then
      Add('NEEDGCCLIB=1');
     if userini.libother then
      Add('NEEDOTHERLIB=1');

   { Info }
     Add('');
     hs:='';
     if userini.infocfg then
      hs:=hs+'fpc_infocfg ';
     if userini.infodir then
      hs:=hs+'fpc_infodir ';
     if userini.infotools then
      hs:=hs+'fpc_infotools ';
     if userini.infoinstall then
      hs:=hs+'fpc_infoinstall ';
     if userini.infofiles then
      hs:=hs+'fpc_infofiles ';
     Add('FPCINFO='+hs);

   { commandline }
     Add('');
     AddSection(true,'command_begin');
     AddSection(true,'command_rtl');
     AddSection(true,'command_needopt');
     AddSection((userini.dirunit<>''),'command_needunit');
     AddSection((userini.dirlib<>''),'command_needlib');
     AddSection((userini.dirobj<>''),'command_needobj');
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
     AddSection(true,'tool_upx');
     AddSection(true,'tool_sed');
     AddSection(true,'tool_date');
     AddSection(true,'tool_zip');
     AddSection(true,'tool_cmp');
     AddSection(true,'tool_diff');

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
     AddRule('install');
     AddRule('staticinstall');
     AddRule('sharedinstall');
     AddRule('libinstall');
     AddRule('zipinstall');
     AddRule('zipinstalladd');
     AddRule('clean');
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
     AddSection(userini.infodir,'info_dir');
     AddSection(userini.infotools,'info_tools');
     AddSection(userini.infoobjects,'info_object');
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
   Error('Can''t read userini.ini');

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
  Revision 1.1  1999-11-02 23:57:40  peter
    * initial version

}
