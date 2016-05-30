
  { TStringBuilder }

  TStringBuilder = class
  private
    const
      DefaultCapacity = 64;
  private
    Function  GetCapacity: Integer;
    Procedure SetCapacity(AValue: Integer);
    Function  GetC(Index: Integer): SBChar;
    Procedure SetC(Index: Integer; AValue: SBChar);
    Function  GetLength: Integer; inline;
    Procedure SetLength(AValue: Integer);
  protected
    FData: TSBCharArray;
    FLength: Integer;
    FMaxCapacity: Integer;
    // Raise error on range check.
    Procedure CheckRange(Idx,Count,MaxLen : Integer);inline;
    Procedure CheckNegative(Const AValue : Integer; Const AName: SBString); inline;
    // All appends/inserts pass through here.

    Procedure DoAppend(Const S : {$IFDEF SBUNICODE}SBString{$ELSE}RawByteString{$ENDIF});virtual;
    Procedure DoAppend(const AValue: TSBCharArray; Idx, aCount: Integer); virtual;
    Procedure DoInsert(Index: Integer; const AValue: SBString); virtual;
    Procedure DoInsert(Index: Integer; const AValue: TSBCharArray; StartIndex, SBCharCount: Integer); virtual;
    Procedure DoReplace(Index: Integer; const Old, New: SBString); virtual;
    Procedure Grow;
    Procedure Shrink;
  public
    Constructor Create;
    Constructor Create(aCapacity: Integer);
    Constructor Create(const AValue: SBString);
    Constructor Create(aCapacity: Integer; aMaxCapacity: Integer);
    Constructor Create(const AValue: SBString; aCapacity: Integer);
    Constructor Create(const AValue: SBString; StartIndex: Integer; aLength: Integer; aCapacity: Integer);

    Function Append(const AValue: Boolean): TStringBuilder;
    Function Append(const AValue: Byte): TStringBuilder;
    Function Append(const AValue: SBChar): TStringBuilder;
    Function Append(const AValue: Currency): TStringBuilder;
    Function Append(const AValue: Double): TStringBuilder;
    Function Append(const AValue: Smallint): TStringBuilder;
    Function Append(const AValue: LongInt): TStringBuilder;
    Function Append(const AValue: Int64): TStringBuilder;
    Function Append(const AValue: TObject): TStringBuilder;
    Function Append(const AValue: Shortint): TStringBuilder;
    Function Append(const AValue: Single): TStringBuilder;
    Function Append(const AValue: UInt64): TStringBuilder;
    Function Append(const AValue: TSBCharArray): TStringBuilder;
    Function Append(const AValue: Word): TStringBuilder;
    Function Append(const AValue: Cardinal): TStringBuilder;
    Function Append(const AValue: PSBChar): TStringBuilder;
{$IFDEF SBUNICODE}
    // Do not use SBRawstring, we need 2 versions in case of unicode
    Function Append(const AValue: SBString): TStringBuilder;
{$ENDIF}
    Function Append(const AValue: RawByteString): TStringBuilder;
    Function Append(const AValue: SBChar; RepeatCount: Integer): TStringBuilder;
    Function Append(const AValue: TSBCharArray; StartIndex: Integer; SBCharCount: Integer): TStringBuilder;
    Function Append(const AValue: SBString; StartIndex: Integer; Count: Integer): TStringBuilder;

    Function Append(const Fmt: SBString; const Args: array of const): TStringBuilder;
    Function AppendFormat(const Fmt: SBString; const Args: array of const): TStringBuilder;
    Function AppendLine: TStringBuilder;
    Function AppendLine(const AValue: RawByteString): TStringBuilder;

    Procedure Clear;
    Procedure CopyTo(SourceIndex: Integer; Var Destination: TSBCharArray; DestinationIndex: Integer; Count: Integer);
    Function EnsureCapacity(aCapacity: Integer): Integer;
    Function Equals(StringBuilder: TStringBuilder): Boolean; reintroduce;

    Function Insert(Index: Integer; const AValue: Boolean): TStringBuilder;
    Function Insert(Index: Integer; const AValue: Byte): TStringBuilder;
    Function Insert(Index: Integer; const AValue: SBChar): TStringBuilder;
    Function Insert(Index: Integer; const AValue: Currency): TStringBuilder;
    Function Insert(Index: Integer; const AValue: Double): TStringBuilder;
    Function Insert(Index: Integer; const AValue: Smallint): TStringBuilder;
    Function Insert(Index: Integer; const AValue: LongInt): TStringBuilder;
    Function Insert(Index: Integer; const AValue: TSBCharArray): TStringBuilder;
    Function Insert(Index: Integer; const AValue: Int64): TStringBuilder;
    Function Insert(Index: Integer; const AValue: TObject): TStringBuilder;
    Function Insert(Index: Integer; const AValue: Shortint): TStringBuilder;
    Function Insert(Index: Integer; const AValue: Single): TStringBuilder;
    Function Insert(Index: Integer; const AValue: SBString): TStringBuilder;
    Function Insert(Index: Integer; const AValue: Word): TStringBuilder;
    Function Insert(Index: Integer; const AValue: Cardinal): TStringBuilder;
    Function Insert(Index: Integer; const AValue: UInt64): TStringBuilder;
    Function Insert(Index: Integer; const AValue: SBString; const aRepeatCount: Integer): TStringBuilder;
    Function Insert(Index: Integer; const AValue: TSBCharArray; startIndex: Integer; SBCharCount: Integer): TStringBuilder;

    Function Remove(StartIndex: Integer; RemLength: Integer): TStringBuilder;

    Function Replace(const OldChar, NewChar: SBChar): TStringBuilder;
    Function Replace(const OldChar, NewChar: SBChar; StartIndex: Integer; Count: Integer): TStringBuilder;
    Function Replace(const OldValue, NewValue: SBRawString): TStringBuilder;
    Function Replace(const OldValue, NewValue: SBRawString; StartIndex: Integer; Count: Integer): TStringBuilder;
{$IFDEF SBUNICODE}
    Function ToString: SBString;
{$ELSE}
    Function ToString: SBString; override;
{$ENDIF}
    Function ToString(aStartIndex: Integer; aLength: Integer): SBString; reintroduce;

    property Chars[index: Integer]: SBChar read GetC write SetC; default;
    property Length: Integer read GetLength write SetLength;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property MaxCapacity: Integer read FMaxCapacity;
  end;


