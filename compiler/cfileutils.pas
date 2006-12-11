{
    Copyright (c) 1998-2002 by Florian Klaempfl and Peter Vreman

    This module provides some basic file/dir handling utils and classes

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}
unit cfileutils;

{$i fpcdefs.inc}

{$define usedircache}

interface

    uses
{$ifdef hasunix}
      Baseunix,unix,
{$endif hasunix}
{$ifdef win32}
      Windows,
{$endif win32}
{$if defined(go32v2) or defined(watcom)}
      Dos,
{$endif}
{$IFNDEF USE_FAKE_SYSUTILS}
      SysUtils,
{$ELSE}
      fksysutl,
{$ENDIF}
      GlobType,
      CUtils,CClasses,
      Systems;

    type
      TCachedDirectory = class(TFPHashObject)
      private
        FDirectoryEntries : TFPHashList;
      public
        constructor Create(AList:TFPHashObjectList;const AName:string);
        destructor  destroy;override;
        procedure Reload;
        function FileExists(const AName:string):boolean;
        function DirectoryExists(const AName:string):boolean;
        property DirectoryEntries:TFPHashList read FDirectoryEntries;
      end;

      TCachedSearchRec = record
        Name       : string;
        Attr       : byte;
        Pattern    : string;
        CachedDir  : TCachedDirectory;
        EntryIndex : longint;
      end;

      TDirectoryCache = class
      private
        FDirectories : TFPHashObjectList;
        function GetDirectory(const ADir:string):TCachedDirectory;
      public
        constructor Create;
        destructor  destroy;override;
        function FileExists(const AName:string):boolean;
        function DirectoryExists(const AName:string):boolean;
        function FindFirst(const APattern:string;var Res:TCachedSearchRec):boolean;
        function FindNext(var Res:TCachedSearchRec):boolean;
        function FindClose(var Res:TCachedSearchRec):boolean;
      end;

      TSearchPathList = class(TStringList)
        procedure AddPath(s:string;addfirst:boolean);overload;
        procedure AddPath(SrcPath,s:string;addfirst:boolean);overload;
        procedure AddList(list:TSearchPathList;addfirst:boolean);
        function  FindFile(const f : string;allowcache:boolean;var foundfile:string):boolean;
      end;

    function  bstoslash(const s : string) : string;
    {Gives the absolute path to the current directory}
    function  GetCurrentDir:string;
    {Gives the relative path to the current directory,
     with a trailing dir separator. E. g. on unix ./ }
    function CurDirRelPath(systeminfo: tsysteminfo): string;
    function  path_absolute(const s : string) : boolean;
    Function  PathExists (const F : String;allowcache:boolean) : Boolean;
    Function  FileExists (const F : String;allowcache:boolean) : Boolean;
    function  FileExistsNonCase(const path,fn:string;allowcache:boolean;var foundfile:string):boolean;
    Function  RemoveDir(d:string):boolean;
    Function  FixPath(s:string;allowdot:boolean):string;
    function  FixFileName(const s:string):string;
    function  TargetFixPath(s:string;allowdot:boolean):string;
    function  TargetFixFileName(const s:string):string;
    procedure SplitBinCmd(const s:string;var bstr: String;var cstr:TCmdStr);
    function  FindFile(const f : string;path : string;allowcache:boolean;var foundfile:string):boolean;
    function  FindFilePchar(const f : string;path : pchar;allowcache:boolean;var foundfile:string):boolean;
    function  FindExe(const bin:string;allowcache:boolean;var foundfile:string):boolean;
    function  GetShortName(const n:string):string;

    procedure InitFileUtils;
    procedure DoneFileUtils;


implementation

    uses
      Comphook,
      Globals;

    var
      DirCache : TDirectoryCache;


{****************************************************************************
                           TCachedDirectory
****************************************************************************}

    constructor TCachedDirectory.create(AList:TFPHashObjectList;const AName:string);
      begin
        inherited create(AList,AName);
        FDirectoryEntries:=TFPHashList.Create;
      end;


    destructor TCachedDirectory.destroy;
      begin
        FDirectoryEntries.Free;
        inherited destroy;
      end;


    procedure TCachedDirectory.Reload;
      var
        dir  : TSearchRec;
      begin
        DirectoryEntries.Clear;
        if findfirst(IncludeTrailingPathDelimiter(Name)+'*',faAnyFile or faDirectory,dir) = 0 then
          begin
            repeat
              if ((dir.attr and faDirectory)<>faDirectory) or
                 (dir.Name<>'.') or
                 (dir.Name<>'..') then
                begin
                  if not(tf_files_case_sensitive in source_info.flags) then
                    DirectoryEntries.Add(Lower(Dir.Name),Pointer(Ptrint(Dir.Attr)))
                  else
                    DirectoryEntries.Add(Dir.Name,Pointer(Ptrint(Dir.Attr)));
                end;
            until findnext(dir) <> 0;
          end;
        findclose(dir);
      end;


    function TCachedDirectory.FileExists(const AName:string):boolean;
      var
        Attr : Longint;
      begin
        if not(tf_files_case_sensitive in source_info.flags) then
          Attr:=PtrInt(DirectoryEntries.Find(Lower(AName)))
        else
          Attr:=PtrInt(DirectoryEntries.Find(AName));
        if Attr<>0 then
          Result:=((Attr and faDirectory)=0)
        else
          Result:=false;
      end;


    function TCachedDirectory.DirectoryExists(const AName:string):boolean;
      var
        Attr : Longint;
      begin
        if not(tf_files_case_sensitive in source_info.flags) then
          Attr:=PtrInt(DirectoryEntries.Find(Lower(AName)))
        else
          Attr:=PtrInt(DirectoryEntries.Find(AName));
        if Attr<>0 then
          Result:=((Attr and faDirectory)=faDirectory)
        else
          Result:=false;
      end;


{****************************************************************************
                           TDirectoryCache
****************************************************************************}

    constructor TDirectoryCache.create;
      begin
        inherited create;
        FDirectories:=TFPHashObjectList.Create(true);
      end;


    destructor TDirectoryCache.destroy;
      begin
        FDirectories.Free;
        inherited destroy;
      end;


    function TDirectoryCache.GetDirectory(const ADir:string):TCachedDirectory;
      var
        CachedDir : TCachedDirectory;
        DirName   : string;
      begin
        if ADir='' then
          DirName:='.'
        else
          DirName:=ADir;
        CachedDir:=TCachedDirectory(FDirectories.Find(DirName));
        if not assigned(CachedDir) then
          begin
            CachedDir:=TCachedDirectory.Create(FDirectories,DirName);
            CachedDir.Reload;
          end;
        Result:=CachedDir;
      end;


    function TDirectoryCache.FileExists(const AName:string):boolean;
      var
        CachedDir : TCachedDirectory;
      begin
        Result:=false;
        CachedDir:=GetDirectory(ExtractFileDir(AName));
        if assigned(CachedDir) then
          Result:=CachedDir.FileExists(ExtractFileName(AName));
      end;


    function TDirectoryCache.DirectoryExists(const AName:string):boolean;
      var
        CachedDir : TCachedDirectory;
      begin
        Result:=false;
        CachedDir:=GetDirectory(ExtractFilePath(AName));
        if assigned(CachedDir) then
          Result:=CachedDir.DirectoryExists(ExtractFileName(AName));
      end;


    function TDirectoryCache.FindFirst(const APattern:string;var Res:TCachedSearchRec):boolean;
      begin
        Res.Pattern:=ExtractFileName(APattern);
        Res.CachedDir:=GetDirectory(ExtractFilePath(APattern));
        Res.EntryIndex:=0;
        if assigned(Res.CachedDir) then
          Result:=FindNext(Res)
        else
          Result:=false;
      end;


    function TDirectoryCache.FindNext(var Res:TCachedSearchRec):boolean;
      begin
        if Res.EntryIndex<Res.CachedDir.DirectoryEntries.Count then
          begin
            Res.Name:=Res.CachedDir.DirectoryEntries.NameOfIndex(Res.EntryIndex);
            Res.Attr:=PtrInt(Res.CachedDir.DirectoryEntries[Res.EntryIndex]);
            inc(Res.EntryIndex);
            Result:=true;
          end
        else
          Result:=false;
      end;


    function TDirectoryCache.FindClose(var Res:TCachedSearchRec):boolean;
      begin
        { nothing todo }
        result:=true;
      end;


{****************************************************************************
                                   Utils
****************************************************************************}

    function bstoslash(const s : string) : string;
    {
      return string s with all \ changed into /
    }
      var
         i : longint;
      begin
        for i:=1to length(s) do
         if s[i]='\' then
          bstoslash[i]:='/'
         else
          bstoslash[i]:=s[i];
         bstoslash[0]:=s[0];
      end;


   {Gives the absolute path to the current directory}
     var
       CachedCurrentDir : string;
   function GetCurrentDir:string;
     begin
       if CachedCurrentDir='' then
         begin
           GetDir(0,CachedCurrentDir);
           CachedCurrentDir:=FixPath(CachedCurrentDir,false);
         end;
       result:=CachedCurrentDir;
     end;

   {Gives the relative path to the current directory,
    with a trailing dir separator. E. g. on unix ./ }
   function CurDirRelPath(systeminfo: tsysteminfo): string;

   begin
     if systeminfo.system <> system_powerpc_macos then
       CurDirRelPath:= '.'+systeminfo.DirSep
     else
       CurDirRelPath:= ':'
   end;


   function path_absolute(const s : string) : boolean;
   {
     is path s an absolute path?
   }
     begin
        result:=false;
{$if defined(unix)}
        if (length(s)>0) and (s[1]='/') then
          result:=true;
{$elseif defined(amiga)}
        if ((length(s)>0) and ((s[1]='\') or (s[1]='/'))) or (Pos(':',s) = length(s)) then
          result:=true;
{$elseif defined(macos)}
        if IsMacFullPath(s) then
          result:=true;
        if ((length(s)>0) and ((s[1]='\') or (s[1]='/'))) or
           ((length(s)>2) and (s[2]=':') and ((s[3]='\') or (s[3]='/'))) then
          result:=true;
{$endif unix}
     end;

    Function FileExists ( Const F : String;allowcache:boolean) : Boolean;
      begin
{$ifdef usedircache}
        if allowcache then
          Result:=DirCache.FileExists(F)
        else
{$endif usedircache}
          Result:=SysUtils.FileExists(F);
        if assigned(do_comment) then
         begin
           if Result then
             do_comment(V_Tried,'Searching file '+F+'... found')
           else
             do_comment(V_Tried,'Searching file '+F+'... not found');
         end;
      end;


    function FileExistsNonCase(const path,fn:string;allowcache:boolean;var foundfile:string):boolean;
      var
        fn2 : string;
      begin
        result:=false;
        if tf_files_case_sensitive in source_info.flags then
          begin
            {
              Search order for case sensitive systems:
               1. NormalCase
               2. lowercase
               3. UPPERCASE
            }
            FoundFile:=path+fn;
            If FileExists(FoundFile,allowcache) then
             begin
               result:=true;
               exit;
             end;
            fn2:=Lower(fn);
            if fn2<>fn then
              begin
                FoundFile:=path+fn2;
                If FileExists(FoundFile,allowcache) then
                 begin
                   result:=true;
                   exit;
                 end;
              end;
            fn2:=Upper(fn);
            if fn2<>fn then
              begin
                FoundFile:=path+fn2;
                If FileExists(FoundFile,allowcache) then
                 begin
                   result:=true;
                   exit;
                 end;
              end;
          end
        else
          if tf_files_case_aware in source_info.flags then
            begin
              {
                Search order for case aware systems:
                 1. NormalCase
              }
              FoundFile:=path+fn;
              If FileExists(FoundFile,allowcache) then
               begin
                 result:=true;
                 exit;
               end;
           end
        else
          begin
            { None case sensitive only lowercase }
            FoundFile:=path+Lower(fn);
            If FileExists(FoundFile,allowcache) then
             begin
               result:=true;
               exit;
             end;
          end;
        { Set foundfile to something usefull }
        FoundFile:=fn;
      end;


    Function PathExists (const F : String;allowcache:boolean) : Boolean;
      Var
        i: longint;
        hs : string;
      begin
        if F = '' then
          begin
            result := true;
            exit;
          end;
        hs := ExpandFileName(F);
        I := Pos (DriveSeparator, hs);
        if (hs [Length (hs)] = DirectorySeparator) and
           (((I = 0) and (Length (hs) > 1)) or (I <> Length (hs) - 1)) then
          Delete (hs, Length (hs), 1);
{$ifdef usedircache}
        if allowcache then
          Result:=DirCache.DirectoryExists(hs)
        else
{$endif usedircache}
          Result:=SysUtils.DirectoryExists(hs);
      end;


    Function RemoveDir(d:string):boolean;
      begin
        if d[length(d)]=source_info.DirSep then
         Delete(d,length(d),1);
        {$I-}
         rmdir(d);
        {$I+}
        RemoveDir:=(ioresult=0);
      end;


    Function FixPath(s:string;allowdot:boolean):string;
      var
        i : longint;
      begin
        { Fix separator }
        for i:=1 to length(s) do
         if s[i] in ['/','\'] then
          s[i]:=source_info.DirSep;
        { Fix ending / }
        if (length(s)>0) and (s[length(s)]<>source_info.DirSep) and
           (s[length(s)]<>':') then
         s:=s+source_info.DirSep;
        { Remove ./ }
        if (not allowdot) and (s='.'+source_info.DirSep) then
         s:='';
        { return }
        if (tf_files_case_aware in source_info.flags) or
           (tf_files_case_sensitive in source_info.flags) then
         FixPath:=s
        else
         FixPath:=Lower(s);
      end;

  {Actually the version in macutils.pp could be used,
   but that would not work for crosscompiling, so this is a slightly modified
   version of it.}
  function TranslatePathToMac (const path: string; mpw: Boolean): string;

    function GetVolumeIdentifier: string;

    begin
      GetVolumeIdentifier := '{Boot}'
      (*
      if mpw then
        GetVolumeIdentifier := '{Boot}'
      else
        GetVolumeIdentifier := macosBootVolumeName;
      *)
    end;

    var
      slashPos, oldpos, newpos, oldlen, maxpos: Longint;

  begin
    oldpos := 1;
    slashPos := Pos('/', path);
    if (slashPos <> 0) then   {its a unix path}
      begin
        if slashPos = 1 then
          begin      {its a full path}
            oldpos := 2;
            TranslatePathToMac := GetVolumeIdentifier;
          end
        else     {its a partial path}
          TranslatePathToMac := ':';
      end
    else
      begin
        slashPos := Pos('\', path);
        if (slashPos <> 0) then   {its a dos path}
          begin
            if slashPos = 1 then
              begin      {its a full path, without drive letter}
                oldpos := 2;
                TranslatePathToMac := GetVolumeIdentifier;
              end
            else if (Length(path) >= 2) and (path[2] = ':') then {its a full path, with drive letter}
              begin
                oldpos := 4;
                TranslatePathToMac := GetVolumeIdentifier;
              end
            else     {its a partial path}
              TranslatePathToMac := ':';
          end;
      end;

    if (slashPos <> 0) then   {its a unix or dos path}
      begin
        {Translate "/../" to "::" , "/./" to ":" and "/" to ":" }
        newpos := Length(TranslatePathToMac);
        oldlen := Length(path);
        SetLength(TranslatePathToMac, newpos + oldlen);  {It will be no longer than what is already}
                                                                        {prepended plus length of path.}
        maxpos := Length(TranslatePathToMac);          {Get real maxpos, can be short if String is ShortString}

        {There is never a slash in the beginning, because either it was an absolute path, and then the}
        {drive and slash was removed, or it was a relative path without a preceding slash.}
        while oldpos <= oldlen do
          begin
            {Check if special dirs, ./ or ../ }
            if path[oldPos] = '.' then
              if (oldpos + 1 <= oldlen) and (path[oldPos + 1] = '.') then
                begin
                  if (oldpos + 2 > oldlen) or (path[oldPos + 2] in ['/', '\']) then
                    begin
                      {It is "../" or ".."  translates to ":" }
                      if newPos = maxPos then
                        begin {Shouldn't actually happen, but..}
                          Exit('');
                        end;
                      newPos := newPos + 1;
                      TranslatePathToMac[newPos] := ':';
                      oldPos := oldPos + 3;
                      continue;  {Start over again}
                    end;
                end
              else if (oldpos + 1 > oldlen) or (path[oldPos + 1] in ['/', '\']) then
                begin
                  {It is "./" or "."  ignor it }
                  oldPos := oldPos + 2;
                  continue;  {Start over again}
                end;

            {Collect file or dir name}
            while (oldpos <= oldlen) and not (path[oldPos] in ['/', '\']) do
              begin
                if newPos = maxPos then
                  begin {Shouldn't actually happen, but..}
                    Exit('');
                  end;
                newPos := newPos + 1;
                TranslatePathToMac[newPos] := path[oldPos];
                oldPos := oldPos + 1;
              end;

            {When we come here there is either a slash or we are at the end.}
            if (oldpos <= oldlen) then
              begin
                if newPos = maxPos then
                  begin {Shouldn't actually happen, but..}
                    Exit('');
                  end;
                newPos := newPos + 1;
                TranslatePathToMac[newPos] := ':';
                oldPos := oldPos + 1;
              end;
          end;

        SetLength(TranslatePathToMac, newpos);
      end
    else if (path = '.') then
      TranslatePathToMac := ':'
    else if (path = '..') then
      TranslatePathToMac := '::'
    else
      TranslatePathToMac := path;  {its a mac path}
  end;


   function FixFileName(const s:string):string;
     var
       i      : longint;
     begin
       if source_info.system = system_powerpc_MACOS then
         FixFileName:= TranslatePathToMac(s, true)
       else
        if (tf_files_case_aware in source_info.flags) or
           (tf_files_case_sensitive in source_info.flags) then
        begin
          for i:=1 to length(s) do
           begin
             case s[i] of
               '/','\' :
                 FixFileName[i]:=source_info.dirsep;
               else
                 FixFileName[i]:=s[i];
             end;
           end;
          FixFileName[0]:=s[0];
        end
       else
        begin
          for i:=1 to length(s) do
           begin
             case s[i] of
               '/','\' :
                  FixFileName[i]:=source_info.dirsep;
               'A'..'Z' :
                  FixFileName[i]:=char(byte(s[i])+32);
                else
                  FixFileName[i]:=s[i];
             end;
           end;
          FixFileName[0]:=s[0];
        end;
     end;


    Function TargetFixPath(s:string;allowdot:boolean):string;
      var
        i : longint;
      begin
        { Fix separator }
        for i:=1 to length(s) do
         if s[i] in ['/','\'] then
          s[i]:=target_info.DirSep;
        { Fix ending / }
        if (length(s)>0) and (s[length(s)]<>target_info.DirSep) and
           (s[length(s)]<>':') then
         s:=s+target_info.DirSep;
        { Remove ./ }
        if (not allowdot) and (s='.'+target_info.DirSep) then
         s:='';
        { return }
        if (tf_files_case_aware in target_info.flags) or
           (tf_files_case_sensitive in target_info.flags) then
         TargetFixPath:=s
        else
         TargetFixPath:=Lower(s);
      end;


   function TargetFixFileName(const s:string):string;
     var
       i : longint;
     begin
       if target_info.system = system_powerpc_MACOS then
         TargetFixFileName:= TranslatePathToMac(s, true)
       else
        if (tf_files_case_aware in target_info.flags) or
           (tf_files_case_sensitive in target_info.flags) then
         begin
           for i:=1 to length(s) do
           begin
             case s[i] of
               '/','\' :
                 TargetFixFileName[i]:=target_info.dirsep;
               else
                 TargetFixFileName[i]:=s[i];
             end;
           end;
           TargetFixFileName[0]:=s[0];
         end
       else
         begin
           for i:=1 to length(s) do
           begin
             case s[i] of
               '/','\' :
                  TargetFixFileName[i]:=target_info.dirsep;
               'A'..'Z' :
                  TargetFixFileName[i]:=char(byte(s[i])+32);
                else
                  TargetFixFileName[i]:=s[i];
             end;
           end;
           TargetFixFileName[0]:=s[0];
         end;
     end;


   procedure SplitBinCmd(const s:string;var bstr:String;var cstr:TCmdStr);
     var
       i : longint;
     begin
       i:=pos(' ',s);
       if i>0 then
        begin
          bstr:=Copy(s,1,i-1);
          cstr:=Copy(s,i+1,length(s)-i);
        end
       else
        begin
          bstr:=s;
          cstr:='';
        end;
     end;


    procedure TSearchPathList.AddPath(s:string;addfirst:boolean);
      begin
        AddPath('',s,AddFirst);
      end;


   procedure TSearchPathList.AddPath(SrcPath,s:string;addfirst:boolean);
     var
       staridx,
       j        : longint;
       prefix,
       suffix,
       CurrentDir,
       currPath : string;
       subdirfound : boolean;
{$ifdef usedircache}
       dir      : TCachedSearchRec;
{$else usedircache}
       dir      : TSearchRec;
{$endif usedircache}
       hp       : TStringListItem;

       procedure WarnNonExistingPath(const path : string);
       begin
         if assigned(do_comment) then
           do_comment(V_Tried,'Path "'+path+'" not found');
       end;

       procedure AddCurrPath;
       begin
         if addfirst then
          begin
            Remove(currPath);
            Insert(currPath);
          end
         else
          begin
            { Check if already in path, then we don't add it }
            hp:=Find(currPath);
            if not assigned(hp) then
             Concat(currPath);
          end;
       end;

     begin
       if s='' then
        exit;
     { Support default macro's }
       DefaultReplacements(s);
     { get current dir }
       CurrentDir:=GetCurrentDir;
       repeat
         { get currpath }
         if addfirst then
          begin
            j:=length(s);
            while (j>0) and (s[j]<>';') do
             dec(j);
            currPath:= TrimSpace(Copy(s,j+1,length(s)-j));
            DePascalQuote(currPath);
            currPath:=FixPath(currPath,false);
            if j=0 then
             s:=''
            else
             System.Delete(s,j,length(s)-j+1);
          end
         else
          begin
            j:=Pos(';',s);
            if j=0 then
             j:=255;
            currPath:= TrimSpace(Copy(s,1,j-1));
            DePascalQuote(currPath);
            currPath:=SrcPath+FixPath(currPath,false);
            System.Delete(s,1,j);
          end;

         { fix pathname }
         if currPath='' then
           currPath:= CurDirRelPath(source_info)
         else
          begin
            currPath:=FixPath(ExpandFileName(currpath),false);
            if (CurrentDir<>'') and (Copy(currPath,1,length(CurrentDir))=CurrentDir) then
             begin
{$ifdef AMIGA}
               currPath:= CurrentDir+Copy(currPath,length(CurrentDir)+1,255);
{$else}
               currPath:= CurDirRelPath(source_info)+Copy(currPath,length(CurrentDir)+1,255);
{$endif}
             end;
          end;
         { wildcard adding ? }
         staridx:=pos('*',currpath);
         if staridx>0 then
          begin
            prefix:=ExtractFilePath(Copy(currpath,1,staridx));
            suffix:=Copy(currpath,staridx+1,length(currpath));
            subdirfound:=false;
{$ifdef usedircache}
            if DirCache.FindFirst(Prefix+'*',dir) then
              begin
                repeat
                  if (dir.attr and faDirectory)<>0 then
                    begin
                      subdirfound:=true;
                      currpath:=prefix+dir.name+suffix;
                      if (suffix='') or PathExists(currpath,true) then
                        begin
                          hp:=Find(currPath);
                          if not assigned(hp) then
                            AddCurrPath;
                        end;
                    end;
                until not DirCache.FindNext(dir);
              end;
            DirCache.FindClose(dir);
{$else usedircache}
            if findfirst(prefix+'*',faDirectory,dir) = 0 then
              begin
                repeat
                  if (dir.name<>'.') and
                      (dir.name<>'..') and
                      ((dir.attr and faDirectory)<>0) then
                    begin
                      subdirfound:=true;
                      currpath:=prefix+dir.name+suffix;
                      if (suffix='') or PathExists(currpath,false) then
                        begin
                          hp:=Find(currPath);
                          if not assigned(hp) then
                            AddCurrPath;
                        end;
                    end;
                until findnext(dir) <> 0;
              end;
            FindClose(dir);
{$endif usedircache}
            if not subdirfound then
              WarnNonExistingPath(currpath);
          end
         else
          begin
            if PathExists(currpath,true) then
             AddCurrPath
            else
             WarnNonExistingPath(currpath);
          end;
       until (s='');
     end;


   procedure TSearchPathList.AddList(list:TSearchPathList;addfirst:boolean);
     var
       s : string;
       hl : TSearchPathList;
       hp,hp2 : TStringListItem;
     begin
       if list.empty then
        exit;
       { create temp and reverse the list }
       if addfirst then
        begin
          hl:=TSearchPathList.Create;
          hp:=TStringListItem(list.first);
          while assigned(hp) do
           begin
             hl.insert(hp.Str);
             hp:=TStringListItem(hp.next);
           end;
          while not hl.empty do
           begin
             s:=hl.GetFirst;
             Remove(s);
             Insert(s);
           end;
          hl.Free;
        end
       else
        begin
          hp:=TStringListItem(list.first);
          while assigned(hp) do
           begin
             hp2:=Find(hp.Str);
             { Check if already in path, then we don't add it }
             if not assigned(hp2) then
              Concat(hp.Str);
             hp:=TStringListItem(hp.next);
           end;
        end;
     end;


   function TSearchPathList.FindFile(const f : string;allowcache:boolean;var foundfile:string):boolean;
     Var
       p : TStringListItem;
     begin
       FindFile:=false;
       p:=TStringListItem(first);
       while assigned(p) do
        begin
          result:=FileExistsNonCase(p.Str,f,allowcache,FoundFile);
          if result then
            exit;
          p:=TStringListItem(p.next);
        end;
       { Return original filename if not found }
       FoundFile:=f;
     end;


   function FindFile(const f : string;path : string;allowcache:boolean;var foundfile:string):boolean;
      Var
        singlepathstring : string;
        i : longint;
     begin
{$ifdef Unix}
       for i:=1 to length(path) do
        if path[i]=':' then
         path[i]:=';';
{$endif Unix}
       FindFile:=false;
       repeat
          i:=pos(';',path);
          if i=0 then
           i:=256;
          singlepathstring:=FixPath(copy(path,1,i-1),false);
          delete(path,1,i);
          result:=FileExistsNonCase(singlepathstring,f,allowcache,FoundFile);
          if result then
            exit;
       until path='';
       FoundFile:=f;
     end;


   function FindFilePchar(const f : string;path : pchar;allowcache:boolean;var foundfile:string):boolean;
      Var
        singlepathstring : string;
        startpc,pc : pchar;
        sepch : char;
     begin
       FindFilePchar:=false;
       if Assigned (Path) then
        begin
{$ifdef Unix}
          sepch:=':';
{$else}
{$ifdef macos}
          sepch:=',';
{$else}
          sepch:=';';
{$endif macos}
{$endif Unix}
          pc:=path;
          repeat
             startpc:=pc;
             while (pc^<>sepch) and (pc^<>';') and (pc^<>#0) do
              inc(pc);
             move(startpc^,singlepathstring[1],pc-startpc);
             singlepathstring[0]:=char(longint(pc-startpc));
             singlepathstring:=FixPath(ExpandFileName(singlepathstring),false);
             result:=FileExistsNonCase(singlepathstring,f,allowcache,FoundFile);
             if result then
               exit;
             if (pc^=#0) then
               break;
             inc(pc);
          until false;
        end;
       foundfile:=f;
     end;


   function  FindExe(const bin:string;allowcache:boolean;var foundfile:string):boolean;
     var
       p : pchar;
       found : boolean;
     begin
       found:=FindFile(FixFileName(ChangeFileExt(bin,source_info.exeext)),'.;'+exepath,allowcache,foundfile);
       if not found then
        begin
{$ifdef macos}
          p:=GetEnvPchar('Commands');
{$else}
          p:=GetEnvPchar('PATH');
{$endif}
          found:=FindFilePChar(FixFileName(ChangeFileExt(bin,source_info.exeext)),p,allowcache,foundfile);
          FreeEnvPChar(p);
        end;
       FindExe:=found;
     end;


    function GetShortName(const n:string):string;
{$ifdef win32}
      var
        hs,hs2 : string;
        i : longint;
{$endif}
{$if defined(go32v2) or defined(watcom)}
      var
        hs : string;
{$endif}
      begin
        GetShortName:=n;
{$ifdef win32}
        hs:=n+#0;
        i:=Windows.GetShortPathName(@hs[1],@hs2[1],high(hs2));
        if (i>0) and (i<=high(hs2)) then
          begin
            hs2[0]:=chr(strlen(@hs2[1]));
            GetShortName:=hs2;
          end;
{$endif}
{$if defined(go32v2) or defined(watcom)}
        hs:=n;
        if Dos.GetShortName(hs) then
         GetShortName:=hs;
{$endif}
      end;


{****************************************************************************
                           Init / Done
****************************************************************************}

    procedure InitFileUtils;
      begin
        DirCache:=TDirectoryCache.Create;
      end;


    procedure DoneFileUtils;
      begin
        DirCache.Free;
      end;

end.
