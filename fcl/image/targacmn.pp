{$mode objfpc}
{$h+}
unit targacmn;

interface

Type
  TWordRec = Packed Record
    Lo,Hi : byte;
  end;

  TTargaHeader = packed record
    IDLen        : Byte;
    MapType      : Byte;
    ImgType      : Byte;
    MapStart     : TWordRec;
    MapLength    : TWordRec;
    MapEntrySize : Byte;
    OriginX      : TWordrec;
    OriginY      : TWordRec;
    Width        : TWordRec;
    Height       : TWordRec;
    PixelSize    : Byte;
    Flags        : Byte;
  end;

  TBGREntry = packed record
    Blue, Green, Red : Byte;
  end;

Function ToWord(AWord : TWordRec) : Word;

implementation

Function ToWord(AWord : TWordRec) : Word;
  
begin
  Result:=(AWord.Lo) or (AWord.Hi shl 8);
end;

end.