{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2004 by Olle Raab

    Some utilities specific for Mac OS.
    Modified portions from Peter N. Lewis (PNL Libraries). Thanks !

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{NOTE: This file requires the following global variables to be declared:
   workingDirectorySpec: FSSpec;}

function FourCharCodeToLongword(fourcharcode: Shortstring): Longword;

begin
  FourCharCodeToLongword:=
    (ord(fourcharcode[1]) shl 24) or
    (ord(fourcharcode[2]) shl 16) or
    (ord(fourcharcode[3]) shl 8) or
    (ord(fourcharcode[4]))
end;

function BitIsSet(arg: Longint; bitnr: Integer): Boolean;

begin
  BitIsSet:= (arg and (1 shl bitnr)) <> 0;
end;

{ Converts MacOS specific error codes to the correct FPC error code.
  All non zero MacOS errors corresponds to a nonzero FPC error.}
Function MacOSErr2RTEerr(err: OSErr): Integer;

var
  res: Integer;

begin
  if err = noErr then { Else it will go through all the cases }
    res:= 0
  else case err of
    dirFulErr, { Directory full }
    dskFulErr  { disk full }
      :res:=101;
    nsvErr     { no such volume }
      :res:=3;
    ioErr,     { I/O error (bummers) }
    bdNamErr   { there may be no bad names in the final system! }
      :res:=1; //TODO Exchange to something better
    fnOpnErr   { File not open }
      :res:=103;
    eofErr,    { End of file }
    posErr     { tried to position to before start of file (r/w) }
      :res:=100;
    mFulErr    { memory full (open) or file won't fit (load) }
      :res:=1; //TODO Exchange to something better
    tmfoErr    { too many files open}
      :res:=4;
    fnfErr     { File not found }
      :res:=2;
    wPrErr     { diskette is write protected. }
      :res:=150;
    fLckdErr   { file is locked }
      :res:=5;
    vLckdErr   { volume is locked }
      :res:=150;
    fBsyErr    { File is busy (delete) }
      :res:=5;
    dupFNErr   { duplicate filename (rename) }
      :res:=5;
    opWrErr    { file already open with with write permission }
      :res:=5;
    rfNumErr,  { refnum error }
    gfpErr     { get file position error }
      :res:=1; //TODO Exchange to something better
    volOffLinErr   { volume not on line error (was Ejected) }
      :res:=152;
    permErr    { permissions error (on file open) }
      :res:=5;
    volOnLinErr{ drive volume already on-line at MountVol }
      :res:=1; //TODO Exchange to something other
    nsDrvErr       { no such drive (tried to mount a bad drive num) }
      :res:=1; //TODO Perhaps exchange to something better
    noMacDskErr,   { not a mac diskette (sig bytes are wrong) }
    extFSErr       { volume in question belongs to an external fs }
      :res:=157; //TODO Perhaps exchange to something better
    fsRnErr,   { file system internal error:during rename the old
                 entry was deleted but could not be restored. }
    badMDBErr  { bad master directory block }
      :res:=1; //TODO Exchange to something better
    wrPermErr  { write permissions error }
      :res:=5;
    dirNFErr   { Directory not found }
      :res:=3;
    tmwdoErr   { No free WDCB available }
      :res:=1; //TODO Exchange to something better
    badMovErr  { Move into offspring error }
      :res:=5;
    wrgVolTypErr   { Wrong volume type error [operation not
                     supported for MFS] }
      :res:=1; //TODO Exchange to something better
    volGoneErr { Server volume has been disconnected. }
      :res:=152;

    diffVolErr         { files on different volumes }
      :res:=17;
    catChangedErr      { the catalog has been modified }
                       { OR comment: when searching with PBCatSearch }
      :res:=1; //TODO Exchange to something other
    afpAccessDenied,   {  Insufficient access privileges for operation  }
    afpDenyConflict    {  Specified open/deny modes conflict with current open modes  }
      :res:=5;
    afpNoMoreLocks     {  Maximum lock limit reached  }
      :res:=5;
    afpRangeNotLocked, {  Tried to unlock range that was not locked by user  }
    afpRangeOverlap    {  Some or all of range already locked by same user  }
      :res:=1; //TODO Exchange to something better
    afpObjectTypeErr   {  File/Directory specified where Directory/File expected  }
      :res:=3;
    afpCatalogChanged  { OR comment: when searching with PBCatSearch }
      :res:=1; //TODO Exchange to something other
    afpSameObjectErr
      :res:=5; //TODO Exchange to something better

    memFullErr { Not enough room in heap zone }
      :res:=203;
  else
    res := 1; //TODO Exchange to something better
  end;
  MacOSErr2RTEerr:= res;
end;

    {Translates a unix or dos path to a mac path. Even a mac path can be input, }
    {then it is returned as is. A trailing directory separator in input}
    {will result in a trailing mac directory separator. For absolute paths, the }
    {parameter mpw affects how the root volume is denoted. If mpw is true, }
    {the path is intended for use in MPW, and the environment variable Boot is}
    {prepended. Otherwise the actual boot volume name is appended.}
    {All kinds of paths are attempted to be translated, except the unusal }
    {dos construct: a relative path on a certain drive like : C:xxx\yyy}

  function TranslatePathToMac (const path: string; mpw: Boolean): string;

    function GetVolumeIdentifier: string;

      var
        s: Str255;
        dummy: Integer;
        err: OSErr;

    begin
      if mpw then
        GetVolumeIdentifier := '{Boot}'
      else
        GetVolumeIdentifier := macosBootVolumeName;
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
                  if (oldpos + 2 > oldlen) or (path[oldPos + 2] in AllowDirectorySeparators) then
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
              else if (oldpos + 1 > oldlen) or (path[oldPos + 1] in AllowDirectorySeparators) then
                begin
                  {It is "./" or "."  ignor it }
                  oldPos := oldPos + 2;
                  continue;  {Start over again}
                end;

            {Collect file or dir name}
            while (oldpos <= oldlen) and not (path[oldPos] in AllowDirectorySeparators) do
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

  {Concats the relative or full path path1 and the relative path path2.}
  function ConcatMacPath (path1, path2: string): string;

  begin
    if Pos(':', path1) = 0 then    {its partial}
      Insert(':', path1, 1);    {because otherwise it would be interpreted}
                  {as a full path, when path2 is appended.}

    if path1[Length(path1)] = ':' then
      begin
        if path2[1] = ':' then
          begin
            Delete(path1, Length(path1), 1);
            ConcatMacPath := Concat(path1, path2)
          end
        else
          ConcatMacPath := Concat(path1, path2)
      end
    else
      begin
        if path2[1] = ':' then
          ConcatMacPath := Concat(path1, path2)
        else
          ConcatMacPath := Concat(path1, ':', path2)
      end;
  end;

  function IsMacFullPath (const path: string): Boolean;

  begin
    if Pos(':', path) = 0 then    {its partial}
      IsMacFullPath := false
    else if path[1] = ':' then
      IsMacFullPath := false
    else
      IsMacFullPath := true
  end;

  function IsDirectory (var spec: FSSpec): Boolean;

    var
      err: OSErr;
      paramBlock: CInfoPBRec;

  begin
    with paramBlock do
      begin
        ioVRefNum := spec.vRefNum;
        ioDirID := spec.parID;
        ioNamePtr := @spec.name;
        ioFDirIndex := 0;

        err := PBGetCatInfoSync(@paramBlock);

        if err = noErr then
          IsDirectory := (paramBlock.ioFlAttrib and $10) <> 0
        else
          IsDirectory := false;
      end;
  end;


{Gives the path for a given file or directory. If parent is true,
 a path to the directory, where the file or directory is located,
 is returned. Functioning even with System 6.}

function FSpGetFullPath (spec: FSSpec; var fullPath: AnsiString;
  parent: Boolean): OSErr;

  var
    res: OSErr;
    pb: CInfoPBRec;

begin
  res := noErr;
  if spec.parID = fsRtParID then { The object is a volume }
    begin
      if not parent then
        begin
          { Add a colon to make it a full pathname }
          fullPath:= spec.name + ':';
        end
      else
        begin
          fullPath:= '';
          res:= afpObjectTypeErr; {to have something close to this error.}
        end;
    end
  else
    begin
      { The object isn't a volume }

      { Add the object name }
      if not parent then
        fullPath:= spec.name
      else
        fullPath:= '';

      { Get the ancestor directory names }
      pb.ioNamePtr := @spec.name;
      pb.ioVRefNum := spec.vRefNum;
      pb.ioDrParID := spec.parID;

      repeat { loop until we have an error or find the root directory }
        begin
          pb.ioFDirIndex := -1;
          pb.ioDrDirID := pb.ioDrParID;
          res := PBGetCatInfoSync(@pb);

          if res = noErr then
            begin
              { Append colon to directory name }
              spec.name := spec.name + ':';
              { Add directory name to fullPathHandle }
              fullPath:= spec.name + fullPath;
            end
        end
      until not ((res = noErr) and (pb.ioDrDirID <> fsRtDirID));
    end;

  FSpGetFullPath := res;
end;

function PathArgToFSSpec(s: string; var spec: FSSpec): Integer;
var
  err: OSErr;
begin
  if pathTranslation then
    s := TranslatePathToMac(s, false);
  err:= FSMakeFSSpec(workingDirectorySpec.vRefNum,
      workingDirectorySpec.parID, s, spec);

  if s <> '' then
    PathArgToFSSpec := MacOSErr2RTEerr(err)
  else
    PathArgToFSSpec := 3; {Empty paths are invalid paths}
end;

function PathArgToFullPath(s: string; var fullpath: AnsiString): Integer;

var
  err: OSErr;
  res: Integer;
  spec: FSSpec;

begin
  res:= PathArgToFSSpec(s, spec);
  if (res = 0) or (res = 2) then
    begin
      err:= FSpGetFullPath(spec, fullpath, false);
      PathArgToFullPath:= MacOSErr2RTEerr(err);
    end
  else
    PathArgToFullPath:=res;
end;

function GetVolumeName(vRefNum: Integer; var volName: String): OSErr;

var
  pb: HParamBlockRec;

begin
  pb.ioNamePtr := @volName;
  pb.ioVRefNum := vRefNum;
  pb.ioVolIndex := 0;
  PBHGetVInfoSync(@pb);
  volName:= volName + ':';
  GetVolumeName:= pb.ioResult;
end;

function GetWorkingDirectoryVRefNum: Integer;

begin
  GetWorkingDirectoryVRefNum:= workingDirectorySpec.vRefNum;
end;

  function GetVolInfo (var name: Str63; var vrn: integer; index: integer; var CrDate: longint): OSErr;
    var
      pb: ParamBlockRec;
      oe: OSErr;
  begin
    if (name <> '') and (name[length(name)] <> ':') then begin
      name := concat(name, ':');
    end;
    pb.ioNamePtr := @name;
    pb.ioVRefNum := vrn;
    pb.ioVolIndex := index;
    oe := PBGetVInfoSync(@pb);
    if oe = noErr then begin
      vrn := pb.ioVRefNum;
      CrDate := pb.ioVCrDate;
    end;
    GetVolInfo := oe;
  end;

  {Checks that fs really is an application with the specified creator}
  function ConfirmApplicationExists (creator: OSType; var fs: FSSpec): OSErr;

    var
      err: OSErr;
      info: FInfo;
  begin
    err := HGetFInfo(fs.vRefNum, fs.parID, fs.name, info);
    if err = noErr then begin
      if (info.fdType <> FourCharCodeToLongword('APPL')) or (info.fdCreator <> creator) then begin
        err := fnfErr;
      end;
    end;
    ConfirmApplicationExists := err;
  end;

  {Find an application with the given creator, in any of the mounted volumes.}
  function FindApplication (creator: OSType; var fs: FSSpec): OSErr;
    var
      i: integer;
      pbdt: DTPBRec;
      crdate: longint;
      oe: OSErr;
      found: Boolean;
  begin
    found := false;
    if (macosSystemVersion >= $0700) then begin
      i := 1;
      repeat
        fs.vRefNum := 0;

        {Get info for volume i}
        oe := GetVolInfo(fs.name, fs.vRefNum, i, crdate);
        i := i + 1;
        if oe = noErr then begin
          with pbdt do begin
            fs.name := '';
            ioNamePtr := @fs.name;
            ioVRefNum := fs.vRefNum;

            {Get the desktop database for this volume}
            oe := PBDTGetPath(@pbdt);
            if oe = noErr then begin
              ioFileCreator := creator;

              {Look first for the "default" (newest) application file}
              ioIndex := 0;
              oe := PBDTGetAPPLSync(@pbdt);
              if oe = noErr then begin
                fs.parID := pbdt.ioAPPLParID;
                found := ConfirmApplicationExists(creator,fs)=noErr;
              end;

              {If not ok, look for older ones.}
              if not found then begin
                ioIndex := 1;
                repeat
                  oe := PBDTGetAPPLSync(@pbdt);
                  if oe = noErr then begin
                    fs.parID := pbdt.ioAPPLParID;
                    found := ConfirmApplicationExists(creator,fs)=noErr;
                  end;
                  ioIndex := ioIndex + 1;
                until found or (oe <> noErr);
              end;

            end;
          end;
          oe := noErr;
        end;
      until found or (oe <> noErr);
    end;
    if found then begin
      oe := noErr;
    end else begin
      oe := fnfErr;
      fs.vRefNum := 0;
      fs.parID := 2;
      fs.name := '';
    end;
    FindApplication := oe;
  end;

function LaunchFSSpec (tofront: Boolean; const applicationFileSpec: FSSpec): OSErr;
var
  launchThis: LaunchParamBlockRec;
begin
  launchThis.launchAppSpec := @applicationFileSpec;
  launchThis.launchAppParameters := nil;
  launchThis.launchBlockID := extendedBlock;
  launchThis.launchEPBLength := extendedBlockLen;
  launchThis.launchFileFlags := 0;
  launchThis.launchControlFlags := launchContinue or launchNoFileFlags;
  if not tofront then begin
    launchThis.launchControlFlags := launchThis.launchControlFlags or launchDontSwitch;
  end;

  LaunchFSSpec:= LaunchApplication(@launchThis);
end;

