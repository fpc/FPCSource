{
    $Id$
    Copyright (c) 2001 by Peter Vreman

    FPCMake - Main module

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$ifdef fpc}{$mode objfpc}{$endif}
{$H+}
unit fpcmmain;
interface

    uses
      sysutils,classes,
      fpcmdic;

    const
      Version='v1.99.0';
      Title='fpcmake '+Version;
      TitleDate=Title+' ['+{$ifdef fpc}{$i %DATE}{$else}'n/a'{$endif}+']';
    type
      TTarget=(t_all,
        t_linux,t_go32v2,t_win32,t_os2,t_freebsd
      );

    const
      TargetStr : array[TTarget] of string=('all',
        'linux','go32v2','win32','os2','freebsd'
      );

      TargetSuffix : array[TTarget] of string=('',
        '_linux','_go32v2','_win32','_os2','_freebsd'
      );


    type
      TKeyValueItem = class(TDictionaryItem)
      private
        FValue : string;
      public
        constructor Create(const k,v:string);
        property Value:string read FValue write FValue;
      end;

      TKeyValue = class(TDictionary)
      private
        function GetKey(const k:string):string;
      public
        procedure Add(const k,v:String);
        property Key[const s:string]:string read GetKey;default;
      end;

      TFPCMakeSection = class(TDictionaryItem)
      private
        FList       : TStringList;
        FDictionary : TKeyValue;
        procedure PrintDic(p:TDictionaryItem);
        procedure BuildIniDic(p:TDictionaryItem);
        procedure BuildMakefileDic(p:TDictionaryItem);
        function GetKey(const k:string):string;
      public
        constructor Create(const n:string);
        constructor CreateKeyValue(const n:string);
        destructor  Destroy;override;
        procedure AddLine(const s:string);
        procedure AddKey(const k,v:string);
        procedure ParseIni;
        procedure BuildIni;
        procedure BuildMakefile;
        procedure Print;
        property Key[const s:string]:string read GetKey;default;
        property List:TStringList read FList;
        property Dictionary:TKeyValue read FDictionary;
      end;

      TFPCMake = class
      private
        FStream         : TStream;
        FFileName       : string;
        FCommentChars   : TSysCharSet;
        FEmptyLines     : boolean;
        FSections       : TDictionary;
        FPackageSec,
        FExportSec      : TFPCMakeSection;
        FPackageName,
        FPackageVersion : string;
        FRequireList    : TStringList;
        FVariables      : TKeyValue;
        procedure Init;
        procedure ParseSec(p:TDictionaryItem);
        procedure PrintSec(p:TDictionaryItem);
        function  GetSec(const AName:string):TDictionaryItem;
      public
        constructor Create(const AFileName:string);
        constructor CreateFromStream(s:TStream;const AFileName:string);
        destructor  Destroy;override;
        procedure LoadSections;
        procedure LoadMakefileFPC;
        procedure LoadPackageSection;
        procedure LoadRequiredPackage(const ReqName,ReqVersion:string);
        procedure LoadRequires(FromFPCMake:TFPCMake);
        function  GetTargetRequires(t:TTarget):TStringList;
        procedure CreateExportSection;
        procedure SubstVariables(var s:string);
        function  GetVariable(const inivar:string):string;
        procedure Print;
        property Section[const s:string]:TDictionaryItem read GetSec;default;
        property RequireList:TStringList read FRequireList;
        property Variables:TKeyValue read FVariables;
        property PackageName:string read FPackageName;
        property PackageVersion:string read FPackageVersion;
        property PackageSec:TFPCMakeSection read FPackageSec;
        property ExportSec:TFPCMakeSection read FExportSec;
        property CommentChars:TSysCharSet read FCommentChars write FCommentChars;
        property EmptyLines:Boolean read FEmptyLines write FEmptyLines;
      end;

    function posidx(const substr,s : string;idx:integer):integer;


implementation

    resourcestring
      s_not_list_sec='Not a list section "%s"';
      s_not_key_value_sec='Not a key-value section "%s"';
      s_err_section_start='%s:%d: Wrong section start';
      s_err_not_key_value='Parse error key=value excepted: "%s"';
      s_err_no_section='%s:%d: Entries without section';
      s_no_package_section='No package section found';
      s_no_package_name='No package name set';
      s_no_package_version='No package version set';
      s_err_require_format='Wrong require format "%s"';

    type
      tspecialdir=record
        dir,unitdir,packdir : string;
      end;

    const
      specialdirs = 4;
      specialdir : array[1..specialdirs] of tspecialdir=(
        (dir: 'rtl';  unitdir: '$(UNITSDIR)/rtl';  packdir: '$(FPCDIR)/rtl'),
        (dir: 'fcl';  unitdir: '$(UNITSDIR)/fcl';  packdir: '$(FPCDIR)/fcl'),
        (dir: 'api';  unitdir: '$(UNITSDIR)/api';  packdir: '$(FPCDIR)/api'),
        (dir: 'fv';   unitdir: '$(UNITSDIR)/fv';   packdir: '$(FPCDIR)/fv')
      );

{****************************************************************************
                                 Helpers
****************************************************************************}

    Function PathExists ( F : String) : Boolean;
      Var
        Info : TSearchRec;
      begin
        if F[Length(f)] in ['/','\'] then
         Delete(f,length(f),1);
        PathExists:=(findfirst(F,fareadonly+faarchive+fahidden+fadirectory,info)=0) and
                    ((info.attr and fadirectory)=fadirectory);
        findclose(Info);
      end;


    function posidx(const substr,s : string;idx:integer):integer;
      var
        i,j : integer;
        e   : boolean;
      begin
        i:=idx;
        j:=0;
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



{****************************************************************************
                               TKeyValueItem
****************************************************************************}

    constructor TKeyValueItem.Create(const k,v:string);
      begin
        inherited Create(k);
        value:=v;
      end;


{****************************************************************************
                                 TKeyValue
****************************************************************************}

    function TKeyValue.GetKey(const k:string):string;
      var
        p : TKeyValueItem;
      begin
        p:=TKeyValueItem(Search(k));
        if p=nil then
         GetKey:=''
        else
         GetKey:=p.Value;
      end;


    procedure TKeyValue.Add(const k,v:string);
      begin
        Insert(TKeyValueItem.Create(k,v));
      end;


{****************************************************************************
                               TFPCMakeSection
****************************************************************************}

    constructor TFPCMakeSection.Create(const n:string);
      begin
        inherited Create(n);
        FList:=TStringList.Create;
        FDictionary:=nil;
      end;


    constructor TFPCMakeSection.CreateKeyValue(const n:string);
      begin
        inherited Create(n);
        FList:=nil;
        FDictionary:=TKeyValue.Create;
      end;


    destructor TFPCMakeSection.Destroy;
      begin
        inherited Destroy;
        FList.Free;
        FDictionary.Free;
      end;


    procedure TFPCMakeSection.AddLine(const s:string);
      begin
        if FList=nil then
         raise Exception.Create(Format(s_not_list_sec,[Name]));
        FList.Add(s);
      end;


    procedure TFPCMakeSection.AddKey(const k,v:string);
      begin
        if FDictionary=nil then
         raise Exception.Create(Format(s_not_key_value_sec,[Name]));
        { Don't add empty values }
        if v<>'' then
         FDictionary.Add(k,v);
      end;


    procedure TFPCMakeSection.PrintDic(p:TDictionaryItem);
      begin
        with TKeyValueItem(p) do
         begin
           writeln('   ',name,' = "',value,'"');
         end;
      end;


    function TFPCMakeSection.GetKey(const k:string):string;
      begin
        if FDictionary=nil then
         raise Exception.Create(Format(s_not_key_value_sec,[Name]));
        GetKey:=FDictionary[k];
      end;


    procedure TFPCMakeSection.Print;
      var
        i : integer;
      begin
        writeln('[',Name,']');
        if assigned(FList) then
         begin
           writeln('  List:');
           for i:=0 to FList.Count-1 do
            Writeln('   "'+FList[i],'"');
           if assigned(FDictionary) then
            writeln('');
         end;
        if assigned(FDictionary) then
         begin
           writeln('  Dictionary:');
           FDictionary.Foreach(@PrintDic);
         end;
      end;


    procedure TFPCMakeSection.ParseIni;
      var
        p : TKeyValueItem;
        i,j,len,maxi : integer;
        s,newkey,value : string;
      begin
        { If already processed skip }
        if assigned(FDictionary) then
         exit;
        { Don't process rules section }
        if (Name='rules') then
         exit;
        { Parse the section }
        FDictionary:=TKeyValue.Create;
        { Parse the list }
        maxi:=FList.Count;
        i:=0;
        while (i<maxi) do
         begin
           s:=Trim(FList[i]);
           len:=Length(s);
           { Concat lines ending with \ }
           while s[len]='\' do
            begin
              Delete(s,len,1);
              if i+1<maxi then
               begin
                 inc(i);
                 s:=s+Trim(FList[i]);
                 len:=Length(s);
               end;
            end;
           { Parse key=value line }
           j:=0;
           while (j<len) and (s[j+1] in ['A'..'Z','a'..'z','0'..'9','_']) do
            inc(j);
           NewKey:=Copy(s,1,j);
           While (j<len) and (s[j+1] in [' ',#9]) do
            inc(j);
           inc(j);
           if s[j]<>'=' then
            Raise Exception.Create(Format(s_err_not_key_value,[s]));
           While (j<len) and (s[j+1] in [' ',#9]) do
            inc(j);
           if j=len then
            Raise Exception.Create(Format(s_err_not_key_value,[s]));
           Value:=Copy(s,j+1,len-j);
           p:=TKeyValueItem(FDictionary[NewKey]);
           { Concat values if key already exists }
           if assigned(p) then
            p.Value:=p.Value+' '+Value
           else
            FDictionary.Add(NewKey,Value);
           inc(i);
         end;
        { List is not used anymore }
        FList.Free;
        FList:=nil;
      end;



    procedure TFPCMakeSection.BuildIniDic(p:TDictionaryItem);
      begin
        with TKeyValueItem(p) do
         begin
           FList.Add(Name+'='+Value);
         end;
      end;


    procedure TFPCMakeSection.BuildIni;
      begin
        if assigned(FList) then
         exit;
        FList:=TStringList.Create;
        FDictionary.Foreach(@BuildIniDic);
        FDictionary.Free;
        FDictionary:=nil;
      end;


    procedure TFPCMakeSection.BuildMakefileDic(p:TDictionaryItem);
      begin
        FList.Add(Uppercase(Name+'_'+TKeyValueItem(p).Name)+'='+TKeyValueItem(p).Value);
      end;


    procedure TFPCMakeSection.BuildMakefile;
      begin
        if assigned(FList) then
         exit;
        FList:=TStringList.Create;
        FDictionary.Foreach(@BuildMakefileDic);
        FDictionary.Free;
        FDictionary:=nil;
      end;


{****************************************************************************
                                   TFPCMake
****************************************************************************}

    constructor TFPCMake.Create(const AFileName:string);
      begin
        FFileName:=AFileName;
        FStream:=nil;
        Init;
      end;


    constructor TFPCMake.CreateFromStream(s:TStream;const AFileName:string);
      begin
        FFileName:=AFileName;
        FStream:=s;
        Init;
      end;


    procedure TFPCMake.Init;
      begin
        FSections:=TDictionary.Create;
        FRequireList:=TStringList.Create;
        FVariables:=TKeyValue.Create;
        FCommentChars:=[';','#'];
        FEmptyLines:=false;
        FPackageName:='';
        FPackageVersion:='';
        FPackageSec:=nil;
        FExportSec:=nil;
      end;


    destructor TFPCMake.Destroy;
      begin
        FSections.Free;
        FRequireList.Free;
        FVariables.Free;
      end;


    procedure TFPCMake.LoadSections;
      var
        SLInput : TStringList;
        i,j,n : integer;
        s,
        SecName : string;
        CurrSec : TFPCMakeSection;
      begin
        try
          SLInput:=TStringList.Create;
          if assigned(FStream) then
           SLInput.LoadFromStream(FStream)
          else
           SLInput.LoadFromFile(FFileName);
          { Load Input into sections list }
          n:=SLInput.Count;
          i:=0;
          while (i<n) do
           begin
             s:=Trim(SLInput[i]);
             if (EmptyLines and (s='')) or
                ((s<>'') and not(s[1] in FCommentChars)) then
              begin
                { section start? }
                if (s<>'') and (s[1]='[') then
                 begin
                   j:=pos(']',s);
                   if j=0 then
                    raise Exception.Create(Format(s_err_section_start,[FFileName,i]));
                   SecName:=Copy(s,2,j-2);
                   CurrSec:=TFPCMakeSection(FSections[SecName]);
                   if CurrSec=nil then
                    CurrSec:=TFPCMakeSection(FSections.Insert(TFPCMakeSection.Create(SecName)));
                 end
                else
                 begin
                   if CurrSec=nil then
                    raise Exception.Create(Format(s_err_no_section,[FFileName,i]));
                   { Insert string without spaces stripped }
                   CurrSec.AddLine(SLInput[i]);
                 end;
              end;
             inc(i);
           end;
        finally
          SLInput.Free;
        end;
      end;


    procedure TFPCMake.LoadMakefileFPC;
      begin
        LoadSections;
        { Parse all sections }
        FSections.Foreach(@ParseSec);
      end;


    procedure TFPCMake.LoadPackageSection;
      begin
        { Get package info from package section }
        FPackageSec:=TFPCMakeSection(FSections['package']);
        if FPackageSec=nil then
         begin
           Writeln('Note: no package section');
           exit;
         end;
        { Parse the section to key=value pairs }
        FPackageSec.ParseIni;
        { mandatory name }
        FPackageName:=FPackageSec['name'];
        if FPackageName='' then
         Raise Exception.Create(s_no_package_name);
        { mandatory version }
        FPackageVersion:=FPackageSec['version'];
        if FPackageVersion='' then
         Raise Exception.Create(s_no_package_version);
        { Set the ExportSec }
        FExportSec:=TFPCMakeSection(FSections[Lowercase(FPackageName)]);
      end;


    procedure TFPCMake.CreateExportSection;
      var
        t : TTarget;
      begin
        { Don't create a section twice }
        if FExportSec<>nil then
         exit;
        { Look if we've already an own section, else create a new
          key-value section }
        FExportSec:=TFPCMakeSection(FSections[LowerCase(FPackageName)]);
        if FExportSec=nil then
         FExportSec:=TFPCMakeSection(FSections.Insert(TFPCMakeSection.CreateKeyValue(LowerCase(FPackageName))));
        { Add default the values to the export section }
        FExportSec.AddKey('name',FPackageName);
        FExportSec.AddKey('version',FPackageVersion);
        { Add required packages }
        for t:=low(TTarget) to high(TTarget) do
         FExportSec.AddKey('require'+TargetSuffix[t],FPackageSec['require'+TargetSuffix[t]]);
        { Unit dir }
        {FExportSec.AddKey('unitdir','$(UNITSDIR)/'+Lowercase(PackageName));}
      end;


    procedure TFPCMake.LoadRequiredPackage(const ReqName,ReqVersion:string);

        function TryFile(const fn:string):boolean;
        var
          ReqFPCMake : TFPCMake;
          HList : TStringList;
          NewSec,ReqSec,Sec : TFPCMakeSection;
        begin
          TryFile:=false;
          if FileExists(fn) then
           begin
             writeln('Package ',ReqName,': ',fn);

             ReqFPCMake:=TFPCMake.Create(fn);
             ReqFPCMake.LoadMakefileFPC;
             ReqFPCMake.LoadPackageSection;
             { Check package name and version }
             if LowerCase(ReqFPCMake.PackageName)<>ReqName then
              raise Exception.Create('s_wrong_package_name');
             if (ReqVersion<>'') and (ReqFPCMake.PackageVersion<ReqVersion) then
              raise Exception.Create('s_wrong_package_version');
             { First load the requirements of this package }
             LoadRequires(ReqFPCMake);
             { Get a copy of the export section }
             if assigned(ReqFPCMake.ExportSec) then
              begin
                ReqFPCMake.ExportSec.BuildIni;
                NewSec:=TFPCMakeSection(FSections.Insert(TFPCMakeSection.Create(ReqName)));
                NewSec.List.AddStrings(ReqFPCMake.ExportSec.List);
                NewSec.ParseIni;
              end;
             { Get a copy of the require section }
             ReqSec:=TFPCMakeSection(ReqFPCMake['require']);
             if assigned(ReqSec) then
              begin
                ReqSec.BuildIni;
                NewSec:=TFPCMakeSection(FSections.Insert(TFPCMakeSection.Create(ReqName+'_require')));
                NewSec.List.AddStrings(ReqSec.List);
                NewSec.ParseIni;
              end;
             { Free }
             ReqFPCMake.Free;
             TryFile:=true;
           end;
        end;

      var
        s : string;
        i : integer;
      begin
        writeln(ReqName,' - ',ReqVersion);

        s:='$(PACKAGESDIR)/'+ReqName;
        For i:=1 to SpecialDirs do
         if SpecialDir[i].Dir=ReqName then
          begin
            s:=SpecialDir[i].PackDir;
            break;
          end;
        SubstVariables(s);
        if TryFile(s+'/Makefile.fpc') then
         exit;
        Raise Exception.Create('s_package_not_found');
      end;


    procedure TFPCMake.LoadRequires(FromFPCMake:TFPCMake);
      var
        s,
        ReqName,
        ReqVersion : string;
        i,j : integer;
        t : TTarget;
        Sec : TFPCMakeSection;
      begin
        Sec:=TFPCMakeSection(FromFPCMake['require']);
        if Sec=nil then
         exit;
        for t:=low(TTarget) to high(TTarget) do
         begin
           s:=Sec['packages'+TargetSuffix[t]];
           repeat
             i:=pos(' ',s);
             if i=0 then
              begin
                ReqName:=Trim(s);
                s:='';
              end
             else
              begin
                ReqName:=Trim(Copy(s,1,i));
                Delete(s,1,i);
                s:=TrimLeft(s);
              end;
             if ReqName<>'' then
              begin
                i:=Pos('(',ReqName);
                if i>0 then
                 begin
                   j:=Pos(')',ReqName);
                   if (i=1) or (j=0) then
                    Raise Exception.Create(Format(s_err_require_format,[ReqName]));
                   ReqVersion:=Copy(ReqName,i+1,j-i-1);
                   ReqName:=Copy(ReqName,1,i-1);
                 end
                else
                 ReqVersion:='';
                { We only use lowercase names }
                ReqName:=Lowercase(ReqName);
                { Already loaded ? }
                if not RequireList.Find(ReqName,i) then
                 begin
                   LoadRequiredPackage(ReqName,ReqVersion);
                   RequireList.Add(ReqName);
                 end;
              end;
           until s='';
         end;
        { Force an rtl dependency }
        if not RequireList.Find('rtl',i) then
         begin
           LoadRequiredPackage('rtl','');
           RequireList.Add('rtl');
         end;
      end;


    function TFPCMake.GetTargetRequires(t:TTarget):TStringList;
      var
        ReqSec  : TFPCMakeSection;
        ReqList : TStringList;

        procedure AddReqSec(t:TTarget;Sec:TFPCMakeSection);
        var
          s,
          ReqName : string;
          RSec : TFPCMakeSection;
          i,j : integer;
        begin
          s:=Sec['packages'+TargetSuffix[t]];
          while s<>'' do
           begin
             i:=pos(' ',s);
             if i=0 then
              begin
                ReqName:=Trim(s);
                s:='';
              end
             else
              begin
                ReqName:=Trim(Copy(s,1,i));
                Delete(s,1,i);
                s:=TrimLeft(s);
              end;
             if ReqName<>'' then
              begin
                i:=Pos('(',ReqName);
                if i>0 then
                 ReqName:=Copy(ReqName,1,i-1);
                { We only use lowercase names }
                ReqName:=Lowercase(ReqName);
                { Already loaded ? }
                if not ReqList.Find(ReqName,i) then
                 begin
                   RSec:=TFPCMakeSection(FSections[ReqName+'_require']);
                   if assigned(RSec) then
                    begin
                      { for packages we depend on always include the
                        target and all keys }
                      if t<>t_all then
                       AddReqSec(t_all,RSec);
                      AddReqSec(t,RSec);
                    end;
                   ReqList.Add(ReqName);
                 end;
              end;
           end;
        end;

      begin
        ReqList:=TStringList.Create;
        ReqSec:=TFPCMakeSection(FSections['require']);
        if assigned(ReqSec) then
         AddReqSec(t,ReqSec);
        GetTargetRequires:=ReqList;
      end;


    procedure TFPCMake.SubstVariables(var s:string);
      var
        i,j,k : integer;
        s2,s3 : string;
        Sec   : TFPCMakeSection;
      begin
        repeat
          i:=Pos('$(',s);
          if i=0 then
           break;
          j:=PosIdx(')',s,i+2);
          s2:=Copy(s,i+2,j-i-2);
          k:=pos('.',s2);
          if k>0 then
           begin
             s3:=Copy(s2,k+1,Length(s2)-k);
             s2:=Copy(s2,1,k-1);
             Sec:=TFPCMakeSection(Section[s2]);
             if assigned(Sec) then
              s2:=Sec[s3]
             else
              s2:='';
           end
          else
           s2:=Variables[s2];
          Delete(s,i,j-i+1);
          Insert(s2,s,i);
        until false;
      end;


    function TFPCMake.GetVariable(const inivar:string):string;
      var
        Sec : TFPCMakeSection;
        Dic : TKeyValue;
        i   : integer;
      begin
        Result:='';
        i:=Pos('.',inivar);
        if i<>0 then
         begin
           Sec:=TFPCMakeSection(FSections[Copy(Inivar,1,i-1)]);
           if assigned(Sec) then
            begin
              Dic:=TKeyValue(Sec.Dictionary);
              Result:=Dic[Copy(IniVar,i+1,Length(IniVar)-i)];
            end
           else
            exit;
         end
        else
         Result:=Variables[IniVar];
      end;


    procedure TFPCMake.ParseSec(p:TDictionaryItem);
      begin
        TFPCMakeSection(p).ParseIni;
      end;


    procedure TFPCMake.PrintSec(p:TDictionaryItem);
      begin
        TFPCMakeSection(p).Print;
      end;


    procedure TFPCMake.Print;
      begin
        FSections.Foreach(@PrintSec);
      end;


    function TFPCMake.GetSec(const AName:string):TDictionaryItem;
      begin
        GetSec:=FSections.Search(AName);
      end;

end.
{
  $Log$
  Revision 1.1  2001-01-24 21:59:36  peter
    * first commit of new fpcmake

}
