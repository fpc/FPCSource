unit filecomparer;

{$mode objfpc}

interface

uses Classes;



type
  TFileComparer = class(TObject)
  private
  protected
    function LoadFromFile(const aFilename: String): TMemoryStream;

    function Compare(const aFileName1, aFilename2: String; var aMsg: String): boolean;
  public
    procedure CompareFiles(aNoSourcefileExt, aNoDestfileExt, aSilent: boolean; const aSourceMask, aDestPath, aDestFileExtension: String);
  end;


implementation

uses SysUtils;


{ TFileComparer }

function TFileComparer.Compare(const aFileName1,
  aFilename2: String; var aMsg: String): boolean;
var
  MStream1: TMemoryStream;
  MStream2: TMemoryStream;
begin
  result := false;
  aMsg   := '';

  if not(FileExists(aFileName1)) then
  begin
    aMsg := format('file "%s" not found', [aFileName1]);
  end
  else if not(FileExists(aFileName2)) then
  begin
    aMsg := format('file "%s" not found', [aFileName2]);
  end
  else
  begin
    MStream1 := LoadFromFile(aFilename1);
    try
      MStream1.Position := 0;

      MStream2 := LoadFromFile(aFilename2);
      try
        MStream2.Position := 0;

        if MStream1.Size < 1 then aMsg := format('file "%s": start or endmarker not found', [aFilename1])
        else if MStream2.Size < 1 then
        begin
          aMsg := format('file "%s": start or endmarker not found', [aFilename2]);
          aMsg := aMsg + #13#10 + format('Size: %d', [MStream2.Size]);
        end
        else
        begin
          if MStream1.Size <> MStream2.Size then aMsg := format('diff: file: "%s"  size: %d - file: "%s"  size: %d',
                                                                [aFilename1, MStream1.Size,
                                                                 aFilename2, MStream2.Size])
          else
          begin
            if CompareMem(MStream1.Memory, MStream2.Memory, MStream1.Size) then result := true
             else aMsg := format('diff: file: "%s" <> file: "%s"', [aFileName1, aFileName2]);
          end;
        end;
      finally
        FreeAndNil(MStream2);
      end;
    finally
      FreeAndNil(MStream1);
    end;
  end;
end;

procedure TFileComparer.CompareFiles(aNoSourcefileExt, aNoDestfileExt, aSilent: boolean; const aSourceMask, aDestPath, aDestFileExtension: String);
var
  i: integer;
  sl: TStringList;
  sr: TSearchRec;
  Path: String;
  FileName: String;
  SourceFileName: String;
  DestFileName: String;
  DestFileExtension: String;
  Msg: String;
begin
  Path := IncludeTrailingBackslash(ExtractFilePath(aSourceMask));
  DestFileExtension := aDestFileExtension;

  if (DestFileExtension <> '') and
     (copy(DestFileExtension, 1, 1) <> '.') then
  begin
    DestFileExtension := '.' + DestFileExtension;
  end;

  sl := TStringList.Create;
  try
    if FindFirst(aSourceMask, faAnyFile - faDirectory - faVolumeID , sr) = 0 then
    repeat
      if not((aNoSourcefileExt) and (ExtractFileExt(sr.Name) <> '')) then sl.Add(sr.Name);
    until FindNext(sr) <> 0;
    FindClose(sr);

    for i := 0 to sl.Count - 1 do
    begin
      sl.Sort;

      if aDestFileExtension <> '' then
      begin
        FileName := copy(sl[i], 1, length(sl[i]) - length(ExtractFileExt(sl[i])));

        if FileName = '' then FileName := sl[i];
      end
      else
      begin
        if aNoDestfileExt then
        begin
          if ExtractFileExt(sl[i]) = '' then Filename := sl[i]
          else
          begin
            FileName := copy(sl[i], 1, length(sl[i]) - length(ExtractFileExt(sl[i])));
          end;
        end
        else Filename := sl[i];
      end;

      SourceFileName := Path + sl[i];
      DestFileName := IncludeTrailingBackslash(aDestpath) + FileName + DestFileExtension;

      if FileExists(SourceFileName) then
      begin
        if FileExists(DestFileName) then
        begin
          if Compare(SourceFileName, DestFileName, Msg) then
          begin
            if not(aSilent) then writeln(format('compare = equal (source: "%s"  destination: "%s")', [SourceFileName, DestFileName]));
          end
          else if Msg <> '' then writeln(ErrOutPut, Msg);
        end
        else writeln(ErrOutPut, format('Comparefile "%s" not found', [DestFileName]));
      end
      else writeln(ErrOutPut, format('Sourcefile "%s" not found', [SourceFileName]));
    end;
  finally
    FreeAndNil(sl);
  end;

end;

function TFileComparer.LoadFromFile(
  const aFilename: String): TMemoryStream;
var
  MStream  : TMemoryStream;

  StartPos : integer;
  EndPos   : integer;


  function FindPos(aStream: TStream; aStartPos: integer; aEndPos: boolean): integer;
  var
    NopCount : integer;
    ch       : byte;
  begin
    result := -1;

    if assigned(aStream) then
    begin
      aStream.Position := aStartPos;

      NopCount := 0;

      while aStream.Position < aStream.Size do
      begin
        aStream.Read(ch, 1);
        if ch = 144 then
        begin
          inc(NopCount);
        end
        else
        begin
          if NopCount >= 10 then
          begin
            if not(aEndPos) then result := aStream.Position
             else result := aStream.Position - NopCount - 1;

            break;
          end
          else NopCount := 0;
        end;
      end;

      if NopCount >= 10 then
      begin
        if (result < 0) and
           (aStream.Position = aStream.Size) then
        begin
          if not(aEndPos) then result := aStream.Position
           else result := aStream.Position - NopCount;
        end;
      end

    end;
  end;

begin
  result := TMemoryStream.Create;

  if FileExists(aFileName) then
  begin
    MStream := TMemoryStream.Create;
    try
      MStream.LoadFromFile(aFileName);

      StartPos := FindPos(MStream, 0, false);
      if StartPos >= 0 then
      begin
        if MStream.Size > StartPos + 16384 then
        begin
          EndPos := FindPos(MStream, MStream.Size - 16384, true);
          if EndPos < 0 then
          begin
            EndPos := FindPos(MStream, StartPos, true);
          end;
        end
        else EndPos := FindPos(MStream, StartPos, true);
      end;

      if (StartPos < 0) OR
         (EndPos < 0) then exit;

      MStream.Position := StartPos - 1;
      result.CopyFrom(MStream, EndPos - StartPos + 1);
    finally
      FreeAndNil(MStream);
    end;
  end;
end;

end.
