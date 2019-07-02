{$ifdef NDS_INTERFACE}

type
  swiSHA1context = packed record 
  type
  TSHA_Block = procedure(var ctx: swiSHA1context; src: pointer; len: size_t);
  PSHA_Block = ^TSHA_Block;
  var
	state: array [0..4] of cuint32;
	total: array [0..1] of cuint32;
	buffer: array [0..63] of cuint8;
	fragment_size: cuint32;
	sha_block: PSHA_Block;
  end;
  swiSHA1context_t =  swiSHA1context;
  TswiSHA1context_t = swiSHA1context_t;
  PswiSHA1context_t = ^TswiSHA1context_t;
  

procedure swiSHA1Init(var ctx: TswiSHA1context_t); cdecl; external;
procedure swiSHA1Update(var ctx: TswiSHA1context_t; data: pointer; len: size_t); cdecl; external;
procedure swiSHA1Final(digest: pointer; var ctx: TswiSHA1context_t); cdecl; external;
procedure swiSHA1Calc(digest: pointer; data: pointer; len: size_t); cdecl; external;
procedure swiSHA1Verify(digest1, digest2: pointer); cdecl; external;
{$endif NDS_INTERFACE}

{$ifdef NDS_IMPLEMENTATION} 


{$endif NDS_IMPLEMENTATION}