{
    $Id$
    Copyright (c) 2001 by Peter Vreman

    FPCMake - Makefile writer

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$ifdef fpc}{$mode objfpc}{$endif}
{$H+}
unit fpcmwr;
interface

    uses
      sysutils,classes,
      fpcmmain;

    type
      tsections=(sec_none,
        sec_units,sec_exes,sec_loaders,sec_examples,
        sec_compile,sec_install,sec_exampleinstall,
        sec_zipinstall,sec_clean,sec_libs,
        sec_command,sec_exts,sec_dirs,sec_tools,sec_info
      );

      tRules=(
        r_all,r_debug,
        r_examples,
        r_smart,r_shared,
        r_install,r_sourceinstall,r_exampleinstall,
        r_zipinstall,r_zipsourceinstall,r_zipexampleinstall,
        r_clean,r_distclean,r_cleanall,
        r_info
      );


    const
      rule2str : array[trules] of string=(
        'all','debug',
        'examples',
        'smart','shared',
        'install','sourceinstall','exampleinstall',
        'zipinstall','zipsourceinstall','zipexampleinstall',
        'clean','distclean','cleanall',
        'info'
      );

      rule2sec : array[trules] of tsections=(
        sec_compile,sec_compile,
        sec_examples,
        sec_libs,sec_libs,
        sec_install,sec_install,sec_exampleinstall,
        sec_zipinstall,sec_zipinstall,sec_zipinstall,
        sec_clean,sec_clean,sec_clean,
        sec_info
      );



    type
      TMakefileWriter=class
      private
        FFileName : string;
        FIni    : TFPCMake;
        FInput  : TFPCMake;
        FOutput : TStringList;
        FPhony  : string;
        FHasSection : array[tsections] of boolean;
        procedure LoadFPCMakeIni;
        procedure AddIniSection(const s:string);
        procedure AddCustomSection(const s:string);
        procedure AddTargetVariable(const inivar:string);
        procedure AddVariable(const inivar:string);
        function  AddTargetDefines(const inivar,prefix:string):string;
        procedure AddRequiredPackages;
        procedure AddTool(const exename:string);
        procedure AddRules;
        procedure AddPhony(const s:string);
        procedure WritePhony;
        procedure AddTargetDirs(const inivar:string);
        function  CheckTargetVariable(const inivar:string):boolean;
        function  CheckVariable(const inivar:string):boolean;
      public
        constructor Create(AFPCMake:TFPCMake;const AFileName:string);
        destructor  Destroy;override;
        procedure WriteGenericMakefile;
      end;


implementation

{$i fpcmake.inc}

    type
      TMyMemoryStream=class(TMemoryStream)
      public
        constructor Create(p:pointer;mysize:integer);
      end;


{*****************************************************************************
                               Helpers
*****************************************************************************}

    function FixVariable(s:string):string;
      var
        i : integer;
      begin
        Result:=UpperCase(s);
        i:=pos('.',Result);
        if i>0 then
         Result[i]:='_';
      end;


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


{*****************************************************************************
                               TMyMemoryStream
*****************************************************************************}

    constructor TMyMemoryStream.Create(p:pointer;mysize:integer);
      begin
        inherited Create;
        SetPointer(p,mysize);
      end;


{*****************************************************************************
                               TMyMemoryStream
*****************************************************************************}

    constructor TMakefileWriter.Create(AFPCMake:TFPCMake;const AFileName:string);
      begin
        FInput:=AFPCMake;
        FFileName:=AFileName;
        FOutput:=TStringList.Create;
        FPhony:='';
        FillChar(FHasSection,sizeof(FHasSection),1);
        LoadFPCMakeIni;
      end;


    destructor TMakefileWriter.Destroy;
      begin
        FOutput.Free;
        FIni.Free;
      end;


    procedure TMakefileWriter.LoadFPCMakeIni;
      var
        IniStream : TStream;
      begin
        try
          IniStream:=TMyMemoryStream.Create(@fpcmakeini,sizeof(fpcmakeini));
          FIni:=TFPCMake.CreateFromStream(IniStream,'fpcmake.ini');
          { Leave the '#' comments in the output }
//          FIni.CommentChars:=[';'];
          FIni.LoadSections;
        finally
          IniStream.Destroy;
        end;
      end;


    procedure TMakefileWriter.AddIniSection(const s:string);
      var
        Sec : TFPCMakeSection;
      begin
        Sec:=TFPCMakeSection(FIni[s]);
        if assigned(Sec) then
         FOutput.AddStrings(Sec.List)
        else
         Raise Exception.Create(Format('Section "%s" doesn''t exists in fpcmake.ini',[s]));
      end;


    procedure TMakefileWriter.AddCustomSection(const s:string);
      var
        Sec : TFPCMakeSection;
      begin
        Sec:=TFPCMakeSection(FInput[s]);
        if assigned(Sec) then
         begin
           Sec.BuildMakefile;
           FOutput.AddStrings(Sec.List);
         end;
      end;


    function TMakefileWriter.CheckTargetVariable(const inivar:string):boolean;
      var
        t : TTarget;
      begin
        result:=false;
        for t:=low(TTarget) to high(TTarget) do
         if FInput.GetVariable(IniVar+TargetSuffix[t])<>'' then
          begin
            result:=true;
            exit;
          end;
      end;


    function TMakefileWriter.CheckVariable(const inivar:string):boolean;
      begin
        Result:=(FInput.GetVariable(IniVar)<>'');
      end;


    procedure TMakefileWriter.AddTargetVariable(const inivar:string);
      var
        s : string;
        T : TTarget;
      begin
        for t:=low(TTarget) to high(TTarget) do
         begin
           s:=FInput.GetVariable(IniVar+TargetSuffix[t]);
           if s<>'' then
            begin
              if t<>t_all then
               FOutput.Add('ifeq ($(OS_TARGET),'+TargetStr[t]+')');
              FOutput.Add('override '+FixVariable(IniVar)+'+='+s);
              if t<>t_all then
               FOutput.Add('endif');
            end;
         end;
      end;


    procedure TMakefileWriter.AddVariable(const inivar:string);
      var
        s : string;
      begin
        s:=FInput.GetVariable(IniVar);
        if s<>'' then
         FOutput.Add('override '+FixVariable(IniVar)+'='+s)
      end;


    function TMakefileWriter.AddTargetDefines(const inivar,prefix:string):string;
      var
        s : string;
        T : TTarget;
        name : string;
        i,k1,k2 : integer;
      begin
        result:='';
        for t:=low(TTarget) to high(TTarget) do
         begin
           s:=FInput.GetVariable(IniVar+TargetSuffix[t]);
           while (s<>'') do
            begin
              i:=pos(' ',s);
              if i=0 then
               i:=length(s)+1;
              name:=Copy(s,1,i-1);
              s:=TrimLeft(Copy(s,i+1,Length(s)-i));
              { Remove (..) }
              k1:=pos('(',name);
              if k1>0 then
               begin
                 k2:=PosIdx(')',name,k1);
                 if k2=0 then
                  k2:=length(name)+1;
                 Delete(Name,k1,k2);
               end;
              FOutput.Add(prefix+VarName(name)+'=1');
              { add to the list of dirs without duplicates }
              AddStrNoDup(result,name);
            end;
         end;
      end;


    procedure TMakefileWriter.AddTool(const exename:string);
      var
        varname : string;
      begin
        with FOutput do
         begin
           varname:=FixVariable(exename);
           Add('ifndef '+varname);
           Add(varname+':=$(strip $(wildcard $(addsuffix /'+exename+'$(SRCEXEEXT),$(SEARCHPATH))))');
           Add('ifeq ($('+varname+'),)');
           Add(varname+'=');
           Add('else');
           Add(varname+':=$(firstword $('+varname+'))');
           Add('endif');
           Add('endif');
           Add('export '+varname);
         end;
      end;


    procedure TMakefileWriter.AddRules;

        procedure AddRule(rule:trules);
        var
          i : integer;
          hs : string;
          Sec : TFPCMakeSection;
          Rules : TStringList;
        begin
          Sec:=TFPCMakeSection(FInput['rules']);
          if assigned(Sec) then
           begin
             Rules:=Sec.List;
             for i:=0 to Rules.Count-1 do
              begin
                if (length(rules[i])>length(rule2str[rule])) and
                   (rules[i][1]=rule2str[rule][1]) and
                   ((rules[i][length(rule2str[rule])+1]=':') or
                    ((length(rules[i])>length(rule2str[rule])+1) and
                     (rules[i][length(rule2str[rule])+2]=':'))) and
                   (Copy(rules[i],1,length(rule2str[rule]))=rule2str[rule]) then
                  exit;
              end;
           end;
          hs:='';
          if FHasSection[Rule2Sec[rule]] then
           hs:=hs+' fpc_'+rule2str[rule];
          { include target dirs }
          if CheckTargetVariable('target.dirs') then
           begin
             if not(rule in [r_sourceinstall,r_zipinstall,r_zipsourceinstall]) or
                not(CheckVariable('package.name')) then
              hs:=hs+' $(addsuffix _'+rule2str[rule]+',$(TARGET_DIRS))';
           end;
          { include cleaning of example dirs }
          if (rule=r_clean) and
             CheckTargetVariable('target.exampledirs') then
           hs:=hs+' $(addsuffix _'+rule2str[rule]+',$(TARGET_EXAMPLEDIRS))';
          if hs<>'' then
           begin
             AddPhony(Rule2Str[Rule]);
             FOutput.Add(rule2str[rule]+':'+hs);
           end;
        end;

      var
        rule : trules;
      begin
        for rule:=low(trules) to high(trules) do
         AddRule(rule);
        WritePhony;
      end;

    procedure TMakefileWriter.AddPhony(const s:string);
      begin
        FPhony:=FPhony+' '+s;
      end;


    procedure TMakefileWriter.WritePhony;
      begin
        if FPhony<>'' then
         begin
           FOutput.Add('.PHONY:'+FPhony);
           FPhony:='';
         end;
      end;


    procedure TMakefileWriter.AddTargetDirs(const inivar:string);

        procedure AddTargetDir(const s,defpref:string);
        var
          j  : trules;
        begin
          FOutput.Add('ifdef '+defpref+VarName(s));
          for j:=low(trules) to high(trules) do
           begin
             FOutput.Add(s+'_'+rule2str[j]+':');
             FOutput.Add(#9+'$(MAKE) -C '+s+' '+rule2str[j]);
             AddPhony(rule2str[j]);
           end;
          WritePhony;
          FOutput.Add('endif');
        end;

      var
        i  : integer;
        hs : string;
        prefix : string;
      begin
        prefix:=FixVariable(inivar)+'_';
        hs:=AddTargetDefines(inivar,prefix);
        while hs<>'' do
         begin
           i:=pos(' ',hs);
           if i=0 then
            i:=length(hs)+1;
           AddTargetDir(Copy(hs,1,i-1),prefix);
           delete(hs,1,i);
         end;
      end;


    procedure TMakefileWriter.AddRequiredPackages;

        procedure AddPackage(const pack,prefix:string);
        var
          packdirvar,unitdirvar : string;
        begin
          FOutput.Add('ifdef '+Prefix+VarName(pack));
          { create needed variables }
          packdirvar:='PACKAGEDIR_'+VarName(pack);
          unitdirvar:='UNITDIR_'+VarName(pack);
          { Search packagedir by looking for Makefile.fpc }
          FOutput.Add(packdirvar+':=$(subst /Makefile.fpc,,$(strip $(wildcard $(addsuffix /'+pack+'/$(OS_TARGET)/Makefile.fpc,$(PACKAGESDIR)))))');
          FOutput.Add('ifeq ($('+packdirvar+'),)');
          FOutput.Add(packdirvar+':=$(subst /Makefile.fpc,,$(strip $(wildcard $(addsuffix /'+pack+'/Makefile.fpc,$(PACKAGESDIR)))))');
          FOutput.Add('ifeq ($('+packdirvar+'),)');
          FOutput.Add(packdirvar+'=');
          FOutput.Add('else');
          FOutput.Add(packdirvar+':=$(firstword $('+packdirvar+'))');
          FOutput.Add('endif');
          FOutput.Add('else');
          FOutput.Add(packdirvar+':=$(firstword $('+packdirvar+'))');
          FOutput.Add('endif');
          { If Packagedir found look for FPCMade }
          FOutput.Add('ifdef '+packdirvar);
          FOutput.Add('ifeq ($(wildcard $('+packdirvar+')/$(FPCMADE)),)');
          FOutput.Add('override COMPILEPACKAGES+=package_'+pack);
          AddPhony('package_'+pack);
          FOutput.Add('package_'+pack+':');
          FOutput.Add(#9'$(MAKE) -C $('+packdirvar+') all');
          FOutput.Add('endif');
          FOutput.Add(unitdirvar+'=$('+packdirvar+')');
          { Package dir doesn''t exists, use unit dir }
          FOutput.Add('else');
          FOutput.Add(packdirvar+'=');
          FOutput.Add(unitdirvar+':=$(subst /Package.fpc,,$(strip $(wildcard $(addsuffix /'+pack+'/Package.fpc,$(UNITSDIR)))))');
          FOutput.Add('ifeq ($('+unitdirvar+'),)');
          FOutput.Add(unitdirvar+'=');
          FOutput.Add('else');
          FOutput.Add(unitdirvar+':=$(firstword $('+unitdirvar+'))');
          FOutput.Add('endif');
          FOutput.Add('endif');
          { Add Unit dir to the command line -Fu }
          FOutput.Add('ifdef '+unitdirvar);
          FOutput.Add('override COMPILER_UNITDIR+=$('+unitdirvar+')');
          FOutput.Add('endif');
          { endif for package }
          FOutput.Add('endif');
        end;

      var
        i  : integer;
        hs : string;
        prefix : string;
        t : Ttarget;
        sl : TStringList;
      begin
        prefix:='REQUIRE_PACKAGES_';
        { Add target defines }
        for t:=low(ttarget) to high(ttarget) do
         begin
           sl:=FInput.GetTargetRequires(t);
           if sl.count>0 then
            begin
              writeln(TargetStr[t]+' requires:');
              if t<>t_all then
               FOutput.Add('ifeq ($(OS_TARGET),'+TargetStr[t]+')');
              for i:=0 to sl.count-1 do
               begin
                 FOutput.Add(prefix+VarName(sl[i])+'=1');
                 writeln(sl[i]);
               end;
              if t<>t_all then
               FOutput.Add('endif');
            end;
           sl.Free;
         end;
        { Add all require packages }
        for i:=0 to FInput.RequireList.Count-1 do
         AddPackage(FInput.RequireList[i],prefix);
        WritePhony;
      end;

    procedure TMakefileWriter.WriteGenericMakefile;
      var
        i : integer;
        rule : trules;
      begin
        with FOutput do
         begin
           { Header }
           Add('#');
           Add('# Don''t edit, this file is generated by '+TitleDate);
           Add('#');
           Add('default: all');
           { Add automatic detect sections }
           AddIniSection('osdetect');
           AddIniSection('fpcdetect');
           AddIniSection('fpcdirdetect');
           { Package }
           AddVariable('package.name');
           AddVariable('package.version');
           { First add the required packages sections }
           for i:=0 to FInput.RequireList.Count-1 do
            AddCustomSection(FInput.Requirelist[i]);
           { Targets }
           AddTargetVariable('target.dirs');
           AddTargetVariable('target.programs');
           AddTargetVariable('target.units');
           AddTargetVariable('target.rsts');
           AddTargetVariable('target.examples');
           AddTargetVariable('target.exampledirs');
           { Clean }
           AddTargetVariable('clean.units');
           AddTargetVariable('clean.files');
           { Install }
           AddTargetVariable('install.units');
           AddTargetVariable('install.files');
           AddVariable('install.prefixdir');
           AddVariable('install.basedir');
           AddVariable('install.datadir');
           { Dist }
           AddVariable('dist.zipname');
           AddVariable('dist.ziptarget');
           { Compiler }
           AddVariable('compiler.options');
           AddVariable('compiler.version');
           AddVariable('compiler.includedir');
           AddVariable('compiler.sourcedir');
           AddVariable('compiler.objectdir');
           AddVariable('compiler.librarydir');
           AddVariable('compiler.targetdir');
           AddVariable('compiler.unittargetdir');
           { Require packages }
           AddRequiredPackages;
           { default dirs/tools/extensions }
           AddIniSection('shelltools');
           AddIniSection('defaulttools');
           AddIniSection('extensions');
           AddIniSection('defaultdirs');
           if CheckVariable('require.libc') then
            AddIniSection('dirlibc');
           { commandline }
           AddIniSection('command_begin');
           if CheckVariable('require.libc') then
            AddIniSection('command_libc');
           AddIniSection('command_end');
           { compile }
           if CheckTargetVariable('target.loaders') then
            AddIniSection('loaderrules');
           if CheckTargetVariable('target.units') then
            AddIniSection('unitrules');
           if CheckTargetVariable('target.programs') then
            AddIniSection('exerules');
           if CheckTargetVariable('target.rsts') then
            AddIniSection('rstrules');
           if CheckTargetVariable('target.examples') or
              CheckTargetVariable('target.exampledirs') then
            AddIniSection('examplerules');
           AddIniSection('compilerules');
           if CheckVariable('lib.name') then
            AddIniSection('libraryrules');
           { install }
           AddIniSection('installrules');
           if CheckTargetVariable('target.examples') or
              CheckTargetVariable('target.exampledirs') then
            AddIniSection('exampleinstallrules');
           if CheckVariable('package.name') then
            AddIniSection('zipinstallrules');
           { clean }
           AddIniSection('cleanrules');
           { info }
           AddIniSection('inforules');
           { Subdirs }
           AddTargetDirs('target.dirs');
           AddTargetDirs('target.exampledirs');
           { Rules }
           AddRules;
           { Users own rules }
           AddIniSection('localmakefile');
           AddIniSection('userrules');
           if assigned(FInput['rules']) then
            AddStrings(TFPCMakeSection(FInput['rules']).List);
         end;
        { write to disk }
        Fixtab(FOutput);
        FOutput.SaveToFile(FFileName);
      end;

end.
{
  $Log$
  Revision 1.1  2001-01-24 21:59:36  peter
    * first commit of new fpcmake

}
