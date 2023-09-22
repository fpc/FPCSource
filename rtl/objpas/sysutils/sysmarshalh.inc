type

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
