{$IFDEF OGC_INTERFACE}

const
//  GQR2 = 914;
//  GQR3 = 915;
//  GQR4 = 916;
//  GQR5 = 917;
//  GQR6 = 918;
//  GQR7 = 919;
  GQR_TYPE_F32 = 0;
  GQR_TYPE_U8 = 4;
  GQR_TYPE_U16 = 5;
  GQR_TYPE_S8 = 6;
  GQR_TYPE_S16 = 7;
  GQR_CAST_U8 = 2;
  GQR_CAST_U16 = 3;
  GQR_CAST_S8 = 4;
  GQR_CAST_S16 = 5;



{$ifdef GEKKO}

procedure __set_gqr(_reg,_val: cint); inline;
procedure CAST_Init(); inline;
procedure CAST_SetGQR2(_type, scale: cuint32); inline;
procedure CAST_SetGQR3(_type, scale: cuint32); inline;
procedure CAST_SetGQR4(_type, scale: cuint32); inline;
procedure CAST_SetGQR5(_type, scale: cuint32); inline;
procedure CAST_SetGQR6(_type, scale: cuint32); inline;
procedure CAST_SetGQR7(_type, scale: cuint32); inline;
function __castu8f32(_in: pcuint8): f32; inline;
function __castu16f32(_in: pcuint16): f32; inline;
function __casts8f32(_in: pcint8): f32; inline
function __casts16f32(register s16 *in): f32; inline;
procedure castu8f32(_in: pcuint8; out _out: pf32); inline;
procedure castu16f32(_in: pcuint16; out _out: pf32); inline;
procedure casts8f32(_in: pcint8; out _out: pf32); inline;
procedure casts16f32(_in: pcint16; out _out: pf32); inline;
function __castf32u8(_in: f32): cuint8; inline;
function __castf32u16(_in: f32): cuint16; inline;
function __castf32s8(_in: f32): cint8; inline;
function __castf32s16(_in: f32): cint16; inline;
procedure castf32u8(_in: pf32; out _out: pcuint8); inline;
procedure castf32u16(_in: pf32; out _out: pcuint16); inline;
procedure castf32s8(_in: pf32; out _out: pcint8); inline;
procedure castf32s16(_in: pf32; out _out: pcint16); inline;

{$endif GEKKO}



{$ENDIF OGC_INTERFACE}

{$IFDEF OGC_IMPLEMENTATION}
{$ifdef GEKKO}

procedure __set_gqr(_reg,_val: cint); inline;
begin
  asm 
    mtspr r3,r4
  end;
end;

// does a default init
procedure CAST_Init(); inline;
begin
  asm
    li		3,0x0004
    oris	3,3,0x0004
    mtspr	914,3
    li		3,0x0005
    oris	3,3,0x0005
    mtspr	915,3
    li		3,0x0006
    oris	3,3,0x0006
    mtspr	916,3
    li		3,0x0007
    oris	3,3,0x0007
    mtspr	917,3
  end;
end;

procedure CAST_SetGQR2(_type, scale: cuint32); inline;
var
  val: cuint32;
begin
  val := (((((scale) shl 8) or (_type)) shl 16) or (((scale) shl 8) or (_type)));
	__set_gqr(GQR2,val);
end;

procedure CAST_SetGQR3(_type, scale: cuint32); inline;
var
  val: cuint32;
begin
	val := (((((scale) shl 8) or (_type)) shl 16) or (((scale) shl 8) or (_type)));
	__set_gqr(GQR3,val);
end;

procedure CAST_SetGQR4(_type, scale: cuint32); inline;
var
  val: cuint32;
begin
	val := (((((scale) shl 8) or (_type)) shl 16) or (((scale) shl 8) or (_type)));
	__set_gqr(GQR4,val);
end;

procedure CAST_SetGQR5(_type, scale: cuint32); inline;
var
  val: cuint32;
begin
	val := (((((scale) shl 8) or (_type)) shl 16) or (((scale) shl 8) or (_type)));
	__set_gqr(GQR5,val);
end;

procedure CAST_SetGQR6(_type, scale: cuint32); inline;
var
  val: cuint32;
begin
	val := (((((scale) shl 8) or (_type)) shl 16) or (((scale) shl 8) or (_type)));
	__set_gqr(GQR6,val);
end;

procedure CAST_SetGQR7(_type, scale: cuint32); inline;
var
  val: cuint32;
begin
	val := (((((scale) shl 8) or (_type)) shl 16) or (((scale) shl 8) or (_type)));
	__set_gqr(GQR7,val);
end;



function __castu8f32(_in: pcuint8): f32; inline;
var
  rval: f32;
begin
  asm
    psq_l	%[rval],0(%[_in]),1,2
  end;
  result := rval;
end;

function __castu16f32(_in: pcuint16): f32; inline;
var
	rval: f32;
begin
  asm
    psq_l	%[rval],0(%[in]),1,3
  end;
	result := rval;
end;

function __casts8f32(_in: pcint8): f32; inline
var
  rval: f32;
begin
  asm
    psq_l	%[rval],0(%[in]),1,4
  end;
	result := rval;
end;

function __casts16f32(register s16 *in): f32; inline;
var
  rval: f32;
begin
  asm
		psq_l	%[rval],0(%[in]),1,5
	end;
	result := rval;
end;

procedure castu8f32(_in: pcuint8; out _out: pf32); inline;
begin
	_out^ := __castu8f32(_in);
end;

procedure castu16f32(_in: pcuint16; out _out: pf32); inline;
begin
	_out^ := __castu16f32(_in);
end;

procedure casts8f32(_in: pcint8; out _out: pf32); inline;
begin
	_out^ := __casts8f32(_in);
end;

procedure casts16f32(_in: pcint16; out _out: pf32); inline;
begin
	_out^ := __casts16f32(_in);
end;

function __castf32u8(_in: f32): cuint8; inline;
var
	a: f32;
	rval: cuint8;
	ptr: pf32;
begin
	ptr := @a;
	asm
    psq_st	%[in],0(%[ptr]),1,2
    lbz	%[out],0(%[ptr])
  end;
	result := rval;
end;

function __castf32u16(_in: f32): cuint16; inline;
var
	a: f32;
	rval: cuint16;
	ptr: pf32;
begin
	ptr := @a;

	asm
    psq_st	%[in],0(%[ptr]),1,3
    lhz	%[out],0(%[ptr])
  end;
	result := rval;
end;

function __castf32s8(_in: f32): cint8; inline;
var
	a: f32;
	rval: cint8;
	ptr: pf32;
begin
	ptr := @a;
  asm
		psq_st	%[in],0(%[ptr]),1,4
		lbz	%[out],0(%[ptr])
  end;
	result := rval;
end;

function __castf32s16(_in: f32): cint16; inline;
var
	a: f32;
	rval: cint16;
	ptr: pf32;
begin
	ptr := @a;

	asm
    psq_st	%[in],0(%[ptr]),1,5
		lha	%[out],0(%[ptr])
  end;
	result := rval;
end;

procedure castf32u8(_in: pf32; out _out: pcuint8); inline;
begin
	_out^ := __castf32u8(_in^);
end;

procedure castf32u16(_in: pf32; out _out: pcuint16); inline;
begin
	_out^ := __castf32u16(_in^);
end;

procedure castf32s8(_in: pf32; out _out: pcint8); inline;
begin
	_out^ := __castf32s8(_in^);
end;

procedure castf32s16(_in: pf32; out _out: pcint16); inline;
begin
	_out^ := __castf32s16(_in^);
end;

{$endif GEKKO}

{$endif OGC_IMPLEMENTATION}

