unit Dbf_IdxCur;

{force CR/LF fix}

interface

{$I Dbf_Common.inc}

uses
  SysUtils,
  Classes,
  Db,
  Dbf_Cursor,
  Dbf_PgFile,
  Dbf_IdxFile,
  Dbf_PrsDef,
  Dbf_Common;

type

//====================================================================
//=== Index support
//====================================================================
  TIndexCursor = class(TVirtualCursor)
  private
    FIndexFile: TIndexFile;
  protected
    function  GetPhysicalRecNo: Integer; override;
    function  GetSequentialRecNo: Integer; override;
    function  GetSequentialRecordCount: Integer; override;
    procedure SetPhysicalRecNo(RecNo: Integer); override;
    procedure SetSequentialRecNo(RecNo: Integer); override;

  public
    constructor Create(DbfIndexFile: TIndexFile);
    destructor Destroy; override;

    function  Next: Boolean; override;
    function  Prev: Boolean; override;
    procedure First; override;
    procedure Last; override;

    procedure GotoBookmark(Bookmark: rBookmarkData); override;
    function  GetBookMark: rBookmarkData; override;

    procedure Insert(RecNo: Integer; Buffer: PChar); override;
    procedure Update(RecNo: Integer; PrevBuffer, NewBuffer: PChar); override;

{$ifdef SUPPORT_VARIANTS}
    procedure VariantToBuffer(Key: Variant; ABuffer: PChar); { override; }
{$endif}
    function  CheckUserKey(Key: PChar; StringBuf: PChar): PChar; { override; }
    function  SearchKey(Key: PChar; SearchType: TSearchKeyType): Boolean; { override; }
    procedure CancelRange; { override; }
    procedure SetBracketLow; { override;}
    procedure SetBracketHigh; { override; }

    property IndexFile: TIndexFile read FIndexFile;
  end;

//====================================================================
//  TIndexCursor = class;
//====================================================================
  PIndexPosInfo = ^TIndexPage;

//====================================================================
implementation

//==========================================================
//============ TIndexCursor
//==========================================================
constructor TIndexCursor.Create(DbfIndexFile: TIndexFile);
begin
  inherited Create(DbfIndexFile);

  FIndexFile := DbfIndexFile;
end;

destructor TIndexCursor.Destroy; {override;}
begin
  inherited Destroy;
end;

procedure TIndexCursor.Insert(RecNo: Integer; Buffer: PChar);
begin
  TIndexFile(PagedFile).Insert(RecNo,Buffer);
  // TODO SET RecNo and Key
end;

procedure TIndexCursor.Update(RecNo: Integer; PrevBuffer, NewBuffer: PChar);
begin
  TIndexFile(PagedFile).Update(RecNo, PrevBuffer, NewBuffer);
end;

procedure TIndexCursor.First;
begin
  TIndexFile(PagedFile).First;
end;

procedure TIndexCursor.Last;
begin
  TIndexFile(PagedFile).Last;
end;

function TIndexCursor.Prev: Boolean;
begin
  Result := TIndexFile(PagedFile).Prev;
end;

function TIndexCursor.Next: Boolean;
begin
  Result := TIndexFile(PagedFile).Next;
end;

function TIndexCursor.GetPhysicalRecNo: Integer;
begin
  Result := TIndexFile(PagedFile).PhysicalRecNo;
end;

procedure TIndexCursor.SetPhysicalRecNo(RecNo: Integer);
begin
  TIndexFile(PagedFile).PhysicalRecNo := RecNo;
end;

function TIndexCursor.GetSequentialRecordCount: Integer;
begin
  Result := TIndexFile(PagedFile).SequentialRecordCount;
end;

function TIndexCursor.GetSequentialRecNo: Integer;
begin
  Result := TIndexFile(PagedFile).SequentialRecNo;
end;

procedure TIndexCursor.SetSequentialRecNo(RecNo: Integer);
begin
  TIndexFile(PagedFile).SequentialRecNo := RecNo;
end;

procedure TIndexCursor.GotoBookmark(Bookmark: rBookmarkData);
begin
  TIndexFile(PagedFile).GotoBookMark(Bookmark);
end;

function TIndexCursor.GetBookMark: rBookmarkData;
begin
  Result := TIndexFile(PagedFile).GetBookmark;
end;

procedure TIndexCursor.SetBracketLow;
begin
  TIndexFile(PagedFile).SetBracketLow;
end;

procedure TIndexCursor.SetBracketHigh;
begin
  TIndexFile(PagedFile).SetBracketHigh;
end;

procedure TIndexCursor.CancelRange;
begin
  TIndexFile(PagedFile).CancelRange;
end;

{$ifdef SUPPORT_VARIANTS}

procedure TIndexCursor.VariantToBuffer(Key: Variant; ABuffer: PChar);
// assumes ABuffer is large enough ie. at least max key size
var
  currLen: Integer;
begin
  if (TIndexFile(PagedFile).KeyType='N') then
  begin
    PDouble(ABuffer)^ := Key;
    if (TIndexFile(PagedFile).IndexVersion <> xBaseIII) then
    begin
      // make copy of userbcd to buffer
      Move(TIndexFile(PagedFile).PrepareKey(ABuffer, etFloat)[0], ABuffer[0], 11);
    end
  end else begin
    StrPLCopy(ABuffer, Key, TIndexFile(PagedFile).KeyLen);
    // we have null-terminated string, pad with spaces if string too short
    currLen := StrLen(ABuffer);
    FillChar(ABuffer[currLen], TIndexFile(PagedFile).KeyLen-currLen, ' ');
  end;
end;

{$endif}

function TIndexCursor.CheckUserKey(Key: PChar; StringBuf: PChar): PChar;
var
  keyLen, userLen: Integer;
begin
  // default is to use key
  Result := Key;
  // if key is double, then no check
  if (TIndexFile(PagedFile).KeyType = 'N') then
  begin
    // nothing needs to be done
  end else begin
    // check if string long enough then no copying needed
    userLen := StrLen(Key);
    keyLen := TIndexFile(PagedFile).KeyLen;
    if userLen < keyLen then
    begin
      // copy string
      Move(Key^, StringBuf[0], userLen);
      // add spaces to searchstring
      FillChar(StringBuf[userLen], keyLen - userLen, ' ');
      // set buffer to temporary buffer
      Result := StringBuf;
    end;
  end;
end;

function TIndexCursor.SearchKey(Key: PChar; SearchType: TSearchKeyType): Boolean;
var
  findres, currRecNo: Integer;
begin
  // save current position
  currRecNo := TIndexFile(PagedFile).SequentialRecNo;
  // search, these are always from the root: no need for first
  findres := TIndexFile(PagedFile).Find(-2, Key);
  // test result
  case SearchType of
    stEqual:
      Result := findres = 0;
    stGreaterEqual:
      Result := findres <= 0;
    stGreater:
      begin
        if findres = 0 then
        begin
          // find next record that is greater
          // NOTE: MatchKey assumes key to search for is already specified
          //   in FUserKey, it is because we have called Find
          repeat
            Result := TIndexFile(PagedFile).Next;
          until not Result or (TIndexFile(PagedFile).MatchKey <> 0);
        end else
          Result := findres < 0;
      end;
    else
      Result := false;
  end;
  // search failed -> restore previous position
  if not Result then
    TIndexFile(PagedFile).SequentialRecNo := currRecNo;
end;

end.

