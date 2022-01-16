{$ifdef NDS_INTERFACE}
type
  swiRSAHeapContext = packed record 
    heapStart: pointer;
	heapEnd: pointer;
	heapSize: size_t;
  end;
  swiRSAHeapContext_t = swiRSAHeapContext;
  TswiRSAHeapContext = swiRSAHeapContext;
  PswiRSAHeapContext = ^TswiRSAHeapContext;


  swiRSAbuffers = packed record
	dst: pointer;
	sig: pointer;
	key: pointer;
  end;
  swiRSAbuffers_t = swiRSAbuffers;
  TswiRSAbuffers = swiRSAbuffers;
  PswiRSAbuffers = ^TswiRSAbuffers;

function swiRSAInitHeap(ctx: PswiRSAHeapContext; heapStart: pointer; heapSize: size_t): cint; cdecl; external;
function swiRSADecryptRAW(ctx: PswiRSAHeapContext; rsabuffers: PswiRSAbuffers; len: size_t): cint; cdecl; external;
function swiRSADecrypt(ctx: PswiRSAHeapContext; dst: pointer; const sig: pointer; const key: pointer): cint; cdecl; external;
function swiRSADecryptPGP(ctx: PswiRSAHeapContext; dst: pointer; const sig: pointer; const key: pointer): cint; cdecl; external;
{$endif NDS_INTERFACE}

{$ifdef NDS_IMPLEMENTATION} 


{$endif NDS_IMPLEMENTATION}