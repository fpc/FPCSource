{
  Translation of the LDAP lber headers for FreePascal
  Copyright (C) 2006 by Ivo Steinmann
}

unit lber;

{$mode objfpc}

interface

{$linklib lber}

{$include lber_typesh.inc}
{$include lberh.inc}

implementation

function LBER_INVALID(t: ber_tag_t): ber_tag_t;
// #define LBER_INVALID(t)     (((t) & (ber_tag_t) = $080UL) && (((t) & (ber_tag_t) ~ = $0FF))
begin
  LBER_INVALID := (t and $80) and (t and $FF);
end;

function LBER_OPT_ON: Pointer;
// #define LBER_OPT_ON     ((void *) &ber_pvt_opt_on)
begin
  LBER_OPT_ON := @ber_pvt_opt_on;
end;

function LBER_SBIOD_READ_NEXT(sbiod: PSockbuf_IO_Desc; buf: Pointer; len: ber_len_t): ber_slen_t;
// #define LBER_SBIOD_READ_NEXT( sbiod, buf, len ) ( (sbiod)->sbiod_next->sbiod_io->sbi_read( (sbiod)->sbiod_next,  buf, len ) )
begin
  LBER_SBIOD_READ_NEXT := sbiod^.sbiod_next^.sbiod_io^.sbi_read(sbiod^.sbiod_next, buf, len);
end;

function LBER_SBIOD_WRITE_NEXT(sbiod: PSockbuf_IO_Desc; buf: Pointer; len: ber_len_t): ber_slen_t;
// #define LBER_SBIOD_WRITE_NEXT( sbiod, buf, len ) ( (sbiod)->sbiod_next->sbiod_io->sbi_write( (sbiod)->sbiod_next, buf, len ) )
begin
  LBER_SBIOD_WRITE_NEXT := sbiod^.sbiod_next^.sbiod_io^.sbi_write(sbiod^.sbiod_next, buf, len);
end;

function LBER_SBIOD_CTRL_NEXT(sbiod: PSockbuf_IO_Desc; opt: cint; arg: Pointer): cint;
// #define LBER_SBIOD_CTRL_NEXT( sbiod, opt, arg ) ( (sbiod)->sbiod_next ? ( (sbiod)->sbiod_next->sbiod_io->sbi_ctrl( (sbiod)->sbiod_next, opt, arg ) ) : 0 )
begin
  if Assigned(sbiod^.sbiod_next) then
    LBER_SBIOD_CTRL_NEXT := sbiod^.sbiod_next^.sbiod_io^.sbi_ctrl(sbiod^.sbiod_next, opt, arg) else
    LBER_SBIOD_CTRL_NEXT := 0;
end;

function ber_bvstr(const str: pcchar): PBerval;
begin
  ber_bvstr := ber_str2bv(str, 0, false, nil);
end;

function ber_bvstrdup(const str: pcchar): PBerval;
begin
  ber_bvstrdup := ber_str2bv(str, 0, true, nil);
end;

function memcmp(p1, p2: Pointer; len: cint): cint;
var
  I: cint;
begin
  for I := 0 to len -1 do
  begin
    if pbyte(p1)^ < pbyte(p2)^ then
    begin
      memcmp := -1;
      Exit;
    end;

    if pbyte(p1)^ > pbyte(p2)^ then
    begin
      memcmp := 1;
      Exit;
    end;

    inc(p1, 1);
    inc(p2, 1);
  end;

  memcmp := 0;
end;

function ber_bvcmp(v1, v2: PBerval): cint;
// #define ber_bvcmp(v1,v2) ((v1)->bv_len < (v2)->bv_len ? -1 : ((v1)->bv_len > (v2)->bv_len ? 1 : memcmp((v1)->bv_val, (v2)->bv_val, (v1)->bv_len) ))
begin
  if v1^.bv_len < v2^.bv_len then ber_bvcmp := -1 else
  if v1^.bv_len > v2^.bv_len then ber_bvcmp :=  1 else
    ber_bvcmp := memcmp(v1^.bv_val, v2^.bv_val, v1^.bv_len);
end;

function ber_errno: cint;
begin
  ber_errno := ber_errno_addr^;
end;

end.
