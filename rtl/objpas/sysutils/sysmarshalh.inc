type
  { Must be in System rather than SysUtils once “generic procedure” are stabilized. }
  TMarshal = class sealed
    constructor Create;

    class function AllocMem(Size: SizeInt): TPtrWrapper; static; inline;
    class function ReallocMem(OldPtr: TPtrWrapper; NewSize: SizeInt): TPtrWrapper; static; inline;
    class procedure FreeMem(Ptr: TPtrWrapper); static; inline;
    class procedure Move(Src, Dest: TPtrWrapper; Count: SizeInt); static; inline;

    class function AsAnsi(const S: UnicodeString): AnsiString; static; inline;
    class function AsAnsi(S: PUnicodeChar): AnsiString; static; inline;

    class function InOutString(const S: UnicodeString): PUnicodeChar; static; inline;
    class function InString(const S: UnicodeString): PUnicodeChar; static; inline;
    class function OutString(const S: UnicodeString): PUnicodeChar; static; inline;

    class function UnsafeAddrOf(var Value): TPtrWrapper; static; inline;

    class function AllocStringAsAnsi(const Str: UnicodeString): TPtrWrapper; static; inline;
    class function AllocStringAsAnsi(const Str: UnicodeString; CodePage: Word): TPtrWrapper; static; inline;
    class function AllocStringAsAnsi(S: PUnicodeChar): TPtrWrapper; static; inline;
    class function AllocStringAsAnsi(S: PUnicodeChar; CodePage: Word): TPtrWrapper; static; inline;
    class function AllocStringAsUnicode(const Str: UnicodeString): TPtrWrapper; static;
    class function AllocStringAsUtf8(const Str: UnicodeString): TPtrWrapper; static; inline;
    class function AllocStringAsUtf8(S: PUnicodeChar): TPtrWrapper; static; inline;

    { Generalization of all AllocStringAsAnsi* above, public because used in TMarshaller. }
    class function AllocStringAsAnsi(S: PUnicodeChar; Len: SizeInt; CodePage: Word): TPtrWrapper; static;

    class procedure Copy(const Src: specialize TArray<UnicodeChar>; StartIndex: SizeInt; Dest: TPtrWrapper; Count: SizeInt); static; inline;
    class procedure Copy(Src: TPtrWrapper; var Dest: specialize TArray<UnicodeChar>; StartIndex: SizeInt; Count: SizeInt); static; inline;
    class procedure Copy(const Src: specialize TArray<UInt8>; StartIndex: SizeInt; Dest: TPtrWrapper; Count: SizeInt); static; inline;
    class procedure Copy(Src: TPtrWrapper; var Dest: specialize TArray<UInt8>; StartIndex: SizeInt; Count: SizeInt); static; inline;
    class procedure Copy(const Src: specialize TArray<Int8>; StartIndex: SizeInt; Dest: TPtrWrapper; Count: SizeInt); static; inline;
    class procedure Copy(Src: TPtrWrapper; var Dest: specialize TArray<Int8>; StartIndex: SizeInt; Count: SizeInt); static; inline;
    class procedure Copy(const Src: specialize TArray<UInt16>; StartIndex: SizeInt; Dest: TPtrWrapper; Count: SizeInt); static; inline;
    class procedure Copy(Src: TPtrWrapper; var Dest: specialize TArray<UInt16>; StartIndex: SizeInt; Count: SizeInt); static; inline;
    class procedure Copy(const Src: specialize TArray<Int16>; StartIndex: SizeInt; Dest: TPtrWrapper; Count: SizeInt); static; inline;
    class procedure Copy(Src: TPtrWrapper; var Dest: specialize TArray<Int16>; StartIndex: SizeInt; Count: SizeInt); static; inline;
    class procedure Copy(const Src: specialize TArray<Int32>; StartIndex: SizeInt; Dest: TPtrWrapper; Count: SizeInt); static; inline;
    class procedure Copy(Src: TPtrWrapper; var Dest: specialize TArray<Int32>; StartIndex: SizeInt; Count: SizeInt); static; inline;
    class procedure Copy(const Src: specialize TArray<Int64>; StartIndex: SizeInt; Dest: TPtrWrapper; Count: SizeInt); static; inline;
    class procedure Copy(Src: TPtrWrapper; var Dest: specialize TArray<Int64>; StartIndex: SizeInt; Count: SizeInt); static; inline;
    class procedure Copy(const Src: specialize TArray<TPtrWrapper>; StartIndex: SizeInt; Dest: TPtrWrapper; Count: SizeInt); static; inline;
    class procedure Copy(Src: TPtrWrapper; var Dest: specialize TArray<TPtrWrapper>; StartIndex: SizeInt; Count: SizeInt); static; inline;

    generic class function FixArray<T>(const Arr: specialize TArray<T>): TPtrWrapper; static;
    generic class procedure UnfixArray<T>(ArrPtr: TPtrWrapper); static;

    class function FixString(var Str: UnicodeString): TPtrWrapper; static;
    class procedure UnfixString(Ptr: TPtrWrapper); static;
    class function UnsafeFixString(const Str: UnicodeString): TPtrWrapper; static;

    class function ReadByte(Ptr: TPtrWrapper; Ofs: SizeInt = 0): Byte; static; inline;
    class procedure WriteByte(Ptr: TPtrWrapper; Ofs: SizeInt; Value: Byte); static; inline;
    class procedure WriteByte(Ptr: TPtrWrapper; Value: Byte); static; inline;

    class function ReadInt16(Ptr: TPtrWrapper; Ofs: SizeInt = 0): Int16; static; inline;
    class procedure WriteInt16(Ptr: TPtrWrapper; Ofs: SizeInt; Value: Int16); static; inline;
    class procedure WriteInt16(Ptr: TPtrWrapper; Value: Int16); static; inline;

    class function ReadInt32(Ptr: TPtrWrapper; Ofs: SizeInt = 0): Int32; static; inline;
    class procedure WriteInt32(Ptr: TPtrWrapper; Ofs: SizeInt; Value: Int32); static; inline;
    class procedure WriteInt32(Ptr: TPtrWrapper; Value: Int32); static; inline;

    class function ReadInt64(Ptr: TPtrWrapper; Ofs: SizeInt = 0): Int64; static; inline;
    class procedure WriteInt64(Ptr: TPtrWrapper; Ofs: SizeInt; Value: Int64); static; inline;
    class procedure WriteInt64(Ptr: TPtrWrapper; Value: Int64); static; inline;

    class function ReadPtr(Ptr: TPtrWrapper; Ofs: SizeInt = 0): TPtrWrapper; static; inline;
    class procedure WritePtr(Ptr: TPtrWrapper; Ofs: SizeInt; Value: TPtrWrapper); static; inline;
    class procedure WritePtr(Ptr, Value: TPtrWrapper); static; inline;

    class function ReadStringAsAnsi(Ptr: TPtrWrapper; Len: SizeInt = -1): UnicodeString; static; inline;
    class function ReadStringAsAnsi(CodePage: Word; Ptr: TPtrWrapper; Len: SizeInt = -1): UnicodeString; static;
    class function ReadStringAsAnsiUpTo(CodePage: Word; Ptr: TPtrWrapper; MaxLen: SizeInt): UnicodeString; static;
    class procedure WriteStringAsAnsi(Ptr: TPtrWrapper; const Value: UnicodeString; MaxCharsIncNull: SizeInt); static; inline;
    class procedure WriteStringAsAnsi(Ptr: TPtrWrapper; const Value: UnicodeString; MaxCharsIncNull: SizeInt; CodePage: Word); static; inline;
    class procedure WriteStringAsAnsi(Ptr: TPtrWrapper; Ofs: SizeInt; const Value: UnicodeString; MaxCharsIncNull: SizeInt); static; inline;
    class procedure WriteStringAsAnsi(Ptr: TPtrWrapper; Ofs: SizeInt; const Value: UnicodeString; MaxCharsIncNull: SizeInt; CodePage: Word); static;

    class function ReadStringAsUnicode(Ptr: TPtrWrapper; Len: SizeInt = -1): UnicodeString; static;
    class function ReadStringAsUnicodeUpTo(Ptr: TPtrWrapper; MaxLen: SizeInt): UnicodeString; static;
    class procedure WriteStringAsUnicode(Ptr: TPtrWrapper; const Value: UnicodeString; MaxCharsIncNull: SizeInt); static;
    class procedure WriteStringAsUnicode(Ptr: TPtrWrapper; Ofs: SizeInt; const Value: UnicodeString; MaxCharsIncNull: SizeInt); static;

    class function ReadStringAsUtf8(Ptr: TPtrWrapper; Len: SizeInt = -1): UnicodeString; static; inline;
    class function ReadStringAsUtf8UpTo(Ptr: TPtrWrapper; MaxLen: SizeInt): UnicodeString; static; inline;
    class procedure WriteStringAsUtf8(Ptr: TPtrWrapper; const Value: UnicodeString; MaxCharsIncNull: SizeInt); static; inline;
    class procedure WriteStringAsUtf8(Ptr: TPtrWrapper; Ofs: SizeInt; const Value: UnicodeString; MaxCharsIncNull: SizeInt); static; inline;
  end;

  TMarshaller = record
  private type
    { Descendants of TDeferBase are stored in-place inside TDeferQueueNode.Mem.
      First node is built into TState and uses StaticStore. }
    PDeferQueueNode = ^TDeferQueueNode;
    TDeferQueueNode = record
      Used, Alloc: Int32;
      Next: PDeferQueueNode;
    case Cardinal of
      0: (Mem: array[0 .. 0] of Byte);
      1: (StaticStore: array[0 .. 15] of Pointer); { Also aligns variable part on SizeOf(Pointer). }
    end;

    TState = class(TInterfacedObject, IInterface)
      DeferHead: TDeferQueueNode;
      DeferTail: PDeferQueueNode;
      constructor Create;
      destructor Destroy; override;
      procedure Flush;
      procedure FlushQueue;
      procedure ClearQueue;
      procedure NotePointerChanged(OldPtr, NewPtr: TPtrWrapper);
    end;

    { Deferred operation (performed by Done). }
    PDeferBase = ^TDeferBase;
    TDeferBase = object
      constructor Init;
      destructor Done; virtual; abstract;
      procedure NotePointerChanged(OldPtr, NewPtr: TPtrWrapper); virtual;
    end;

    PDeferFreeMem = ^TDeferFreeMem;
    TDeferFreeMem = object(TDeferBase)
      P: TPtrWrapper;
      destructor Done; virtual;
      procedure NotePointerChanged(OldPtr, NewPtr: TPtrWrapper); virtual;
    end;

    TUnfixProc = procedure(Ptr: TPtrWrapper);

    { Not required if there is a way to take an address of a generic procedure specialization... }
    generic TAddressableUnfixArraySpecialization<T> = record
      class procedure UnfixArray(ArrPtr: TPtrWrapper); static;
    end;

    PDeferUnfix = ^TDeferUnfix;
    TDeferUnfix = object(TDeferBase)
      Unfix: TUnfixProc;
      P: TPtrWrapper;
      destructor Done; virtual;
    end;

    PDeferMoveToSBAndFree = ^TDeferMoveToSBAndFree;
    TDeferMoveToSBAndFree = object(TDeferBase)
      Src: TPtrWrapper;
      SB: TUnicodeStringBuilder;
      MaxLen: SizeInt;
      destructor Done; virtual;
    end;

    function PushDefer(InstanceSize: SizeInt): PDeferBase;

  var
    FState: TState;
    FStateLife: IInterface;

  public
    procedure Flush;

    function AllocMem(Size: SizeInt): TPtrWrapper;
    function ReallocMem(OldPtr: TPtrWrapper; NewSize: NativeInt): TPtrWrapper;

    function AllocStringAsAnsi(const Str: UnicodeString): TPtrWrapper; inline;
    function AllocStringAsAnsi(const Str: UnicodeString; CodePage: Word): TPtrWrapper; inline;
    function AllocStringAsUnicode(const Str: UnicodeString): TPtrWrapper;
    function AllocStringAsUtf8(const Str: UnicodeString): TPtrWrapper; inline;

    function AsAnsi(const S: UnicodeString): TPtrWrapper; inline;
    function AsAnsi(S: PUnicodeChar): TPtrWrapper; inline;
    function AsAnsi(const S: UnicodeString; CodePage: Word): TPtrWrapper; inline;
    function AsAnsi(S: PUnicodeChar; CodePage: Word): TPtrWrapper; inline;

    function AsUtf8(const S: UnicodeString): TPtrWrapper; inline;
    function AsUtf8(S: PUnicodeChar): TPtrWrapper; inline;
  private
    function AllocStringAsAnsi(S: PUnicodeChar; Len: SizeInt; CodePage: Word): TPtrWrapper;

  public
    { No clue what's it, let it be a synonym of FixArray for now... }
    function AsRaw(const B: TBytes): TPtrWrapper; inline;

    generic function FixArray<T>(const Arr: specialize TArray<T>): TPtrWrapper;
    function FixString(var Str: UnicodeString): TPtrWrapper;
    function UnsafeFixString(const Str: UnicodeString): TPtrWrapper;

    function InString(SB: TUnicodeStringBuilder; MaxLen: SizeInt): TPtrWrapper;
    function OutString(const S: UnicodeString): TPtrWrapper; inline;
    function InOutString(SB: TUnicodeStringBuilder; MaxLen: SizeInt): TPtrWrapper;
  end;
