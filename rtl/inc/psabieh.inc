{
    This file is part of the Free Pascal run time library.
    Translated to Pascal by Jonas Maebe,
    member of the Free Pascal development team

    This file is based on the source code of libsupc++ from GCC 4.2.1.

    See below for details about the copyright. While it is GPLv2 rather
    than LGPLv2 like the rest of the FPC RTL, it has the same linking
    exception as the rest of the FPC RTL and hence it can be used in the
    same way.

 **********************************************************************}

// -*- C++ -*- The GNU C++ exception personality routine.
// Copyright (C) 2001, 2002, 2003, 2006 Free Software Foundation, Inc.
//
// This file is part of GCC.
//
// GCC is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// GCC is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with GCC; see the file COPYING.  If not, write to
// the Free Software Foundation, 51 Franklin Street, Fifth Floor,
// Boston, MA 02110-1301, USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.


{$packrecords c}

{$ifdef linux}
{$linklib c}
{$linklib libgcc_s}
{$endif}

{$ifdef __ARM_EABI_UNWINDER__}
{$define PSABIEH_NO_SIZEOF_ENCODED_VALUE}
{$endif}

function FPC_psabieh_GetExceptionWrapper(exceptionObject: PFPC_Unwind_Exception): PExceptObject; inline;
  begin
    { go to end of the wrapped exception (it's the last field in PFPC_Unwind_Exception), then to the start }
    result:=PExceptObject(exceptionObject+1)-1;
  end;

function _Unwind_Resume_or_Rethrow (context:PFPC_Unwind_Context): FPC_Unwind_Reason_Code;cdecl;external;
procedure _Unwind_DeleteException(context:PFPC_Unwind_Context);cdecl;external;
function _Unwind_GetRegionStart(context:PFPC_Unwind_Context):PtrUInt;cdecl;external;
function _Unwind_GetLanguageSpecificData(context:PFPC_Unwind_Context):PtrUInt;cdecl;external;

function _Unwind_GetDataRelBase(context:PFPC_Unwind_Context):PtrUInt;cdecl;external;
function _Unwind_GetTextRelBase(context:PFPC_Unwind_Context):PtrUInt;cdecl;external;

{$ifdef __ARM_EABI_UNWINDER__}
procedure _Unwind_Complete(exceptionObject: PFPC_Unwind_Exception);cdecl;external;
function __gnu_unwind_frame(exception:PFPC_Unwind_Exception;context:PFPC_Unwind_Context):FPC_Unwind_Reason_Code;cdecl;external;

type
  FPC_Unwind_VRS_RegClass = UInt32;
const
  FPC_UVRSC_CORE = FPC_Unwind_VRS_RegClass(0);      // integer register
  FPC_UVRSC_VFP = FPC_Unwind_VRS_RegClass(1);       // vfp
  FPC_UVRSC_FPA = FPC_Unwind_VRS_RegClass(2);       // fpa
  FPC_UVRSC_WMMXD = FPC_Unwind_VRS_RegClass(3);     // Intel WMMX data register
  FPC_UVRSC_WMMXC = FPC_Unwind_VRS_RegClass(4);     // Intel WMMX control register

type
  FPC_Unwind_VRS_DataRepresentation = UInt32;
const
  FPC_UVRSD_UINT32 = FPC_Unwind_VRS_DataRepresentation(0);
  FPC_UVRSD_VFPX = FPC_Unwind_VRS_DataRepresentation(1);
  FPC_UVRSD_FPAX = FPC_Unwind_VRS_DataRepresentation(2);
  FPC_UVRSD_UINT64 = FPC_Unwind_VRS_DataRepresentation(3);
  FPC_UVRSD_FLOAT = FPC_Unwind_VRS_DataRepresentation(4);
  FPC_UVRSD_DOUBLE = FPC_Unwind_VRS_DataRepresentation(5);

type
  FPC_Unwind_VRS_Result = UInt32;
const
  FPC_UVRSR_OK = FPC_Unwind_VRS_Result(0);
  FPC_UVRSR_NOT_IMPLEMENTED = FPC_Unwind_VRS_Result(1);
  FPC_UVRSR_FAILED = FPC_Unwind_VRS_Result(2);

Function _Unwind_VRS_Set(context: PFPC_Unwind_Context; regclass: FPC_Unwind_VRS_RegClass;
                                   regnr: PTRUint {uw}; repr: FPC_Unwind_VRS_DataRepresentation;
                                   value: pointer): FPC_Unwind_VRS_Result; cdecl; external;

function _Unwind_VRS_Get(context: PFPC_Unwind_Context; regclass: FPC_Unwind_VRS_RegClass;
                                   regnr: PTRUint {uw}; repr: FPC_Unwind_VRS_DataRepresentation;
                                   value: pointer): FPC_Unwind_VRS_Result; cdecl; external;


procedure _Unwind_SetGR(context:PFPC_Unwind_Context;index:cint; new_value:PtrUInt); inline;
  begin
    _Unwind_VRS_Set(context,FPC_UVRSC_CORE, index, FPC_UVRSD_UINT32, @new_Value);
  end;

function _Unwind_GetGR(context:PFPC_Unwind_Context;index:cint):PtrUInt; inline;
  begin
    _Unwind_VRS_Get(context,FPC_UVRSC_CORE, index, FPC_UVRSD_UINT32, @result);
  end;


procedure _Unwind_SetIP(context:PFPC_Unwind_Context;new_value:PtrUInt); inline;
  begin
    _Unwind_SetGR(context,15,new_value or (_Unwind_GetGR(context,15) and 1));
  end;

function _Unwind_GetIP(context:PFPC_Unwind_Context):PtrUInt;cdecl; inline;
  begin
    result:=_Unwind_GetGR(context,15) and not(1);
  end;

{$else}

function _Unwind_GetGR(context:PFPC_Unwind_Context; index:cint):PtrUInt;cdecl;external;
procedure _Unwind_SetGR(context:PFPC_Unwind_Context; index:cint; new_value:PtrUInt);cdecl;external;
function _Unwind_GetIP(context:PFPC_Unwind_Context):PtrUInt;cdecl;external;
procedure _Unwind_SetIP(context:PFPC_Unwind_Context; new_value:PtrUInt);cdecl;external;
{$endif}

procedure DoUnHandledException; forward;

{ _Unwind_Backtrace() is a gcc extension that walks the stack and calls the  }
{ _Unwind_Trace_Fn once per frame until it reaches the bottom of the stack }
{ or the _Unwind_Trace_Fn function returns something other than _URC_NO_REASON. }
{ }
type
  FPC_Unwind_Trace_Fn = function (_para1:PFPC_Unwind_Context; _para2:pointer):FPC_Unwind_Reason_Code;cdecl;

function _Unwind_Backtrace(_para1:FPC_Unwind_Trace_Fn; _para2:pointer):FPC_Unwind_Reason_Code;cdecl;weakexternal;

{ _Unwind_GetCFA is a gcc extension that can be called from within a personality  }
{ handler to get the CFA (stack pointer before call) of current frame. }
{ }
function _Unwind_GetCFA(_para1:PFPC_Unwind_Context):PtrUInt;cdecl;weakexternal;

const
  DW_EH_PE_absptr       = $00;
  DW_EH_PE_omit         = $ff;

  DW_EH_PE_uleb128      = $01;
  DW_EH_PE_udata2       = $02;
  DW_EH_PE_udata4       = $03;
  DW_EH_PE_udata8       = $04;
  DW_EH_PE_sleb128      = $09;
  DW_EH_PE_sdata2       = $0A;
  DW_EH_PE_sdata4       = $0B;
  DW_EH_PE_sdata8       = $0C;
  DW_EH_PE_signed       = $08;

  DW_EH_PE_pcrel        = $10;
  DW_EH_PE_textrel      = $20;
  DW_EH_PE_datarel      = $30;
  DW_EH_PE_funcrel      = $40;
  DW_EH_PE_aligned      = $50;

  DW_EH_PE_indirect	= $80;

{$ifndef PSABIEH_NO_SIZEOF_ENCODED_VALUE}
function FPC_psabieh_size_of_encoded_value(encoding: byte): longint;
  begin
    if encoding = DW_EH_PE_omit then
      exit(0);

    case (encoding and 7) of
      DW_EH_PE_absptr:
        exit(sizeof(pointer));
      DW_EH_PE_udata2:
        exit(2);
      DW_EH_PE_udata4:
        exit(4);
      DW_EH_PE_udata8:
        exit(8);
      else
        begin
{$ifdef excdebug}
          writeln('Unsupported encoding: $', hexstr(encoding,sizeof(encoding)*2));
{$endif}
          halt(217);
        end;
    end
  end;
{$endif PSABIEH_NO_SIZEOF_ENCODED_VALUE}

{ Given an encoding and an _Unwind_Context, return the base to which
   the encoding is relative.  This base may then be passed to
   read_encoded_value_with_base for use when the _Unwind_Context is
   not available.  }

function FPC_psabieh_base_of_encoded_value (encoding: byte; context: PFPC_Unwind_Context): PtrUInt;
  begin
    if encoding = DW_EH_PE_omit then
      exit(0);

    case (encoding and $70) of
      DW_EH_PE_absptr,
      DW_EH_PE_pcrel,
      DW_EH_PE_aligned:
        exit(0);
      DW_EH_PE_textrel:
        exit(_Unwind_GetTextRelBase(context));
      DW_EH_PE_datarel:
        exit(_Unwind_GetDataRelBase(context));
      DW_EH_PE_funcrel:
        exit(_Unwind_GetRegionStart(context));
      else
        begin
{$ifdef excdebug}
          writeln('Unsupported base of encoding: $', hexstr(encoding,sizeof(encoding)*2));
{$endif}
          halt(217);
        end;
      end;
  end;

function fpc_read_uleb128 (p: PByte; out val: PTRUInt): PByte;
  var
    shift: longint;
    b: byte;
    res: PtrUInt;
  begin
    shift:=0;

    res:=0;
    repeat
      b:=p^;
      inc(p);
      res:=res or (PtrUInt(b and $7f) shl shift);
      inc(shift,7);
    until (b and $80)=0;

    val:=res;
    result:=p;
  end;

function fpc_read_sleb128 (p: PByte; out val: PtrInt): PByte;
  var
    shift: longint;
    b: byte;
    res: PtrUInt;
  begin
    shift:=0;

    res:=0;
    repeat
      b:=p^;
      inc(p);
      res:=res or (PtrUInt(b and $7f) shl shift);
      inc(shift,7);
    until (b and $80)=0;
    if (shift<8*(sizeof(res))) and
       ((b and $40)<>0) then
      res:=res or -(PtrUInt(1) shl shift);

    val:=PTRInt(res);
    result:=p;
  end;

function FPC_psabieh_read_encoded_value_with_base (encoding: byte; base: PtrUInt; p: PByte; out val: PtrUInt): PByte;
  var
    res: PtrUInt;
    tmpres: PtrInt;
    alignedp: PPtrUint;
  begin
    if encoding=DW_EH_PE_aligned then
      begin
        alignedp:=PPtrUint(align(PtrUInt(p),sizeof(PtrUint)));
        res:=alignedp^;
        result:=PByte(alignedp)+sizeof(PtrUInt);
      end
    else
      begin
        case encoding and $0f of
          DW_EH_PE_absptr:
            begin
              res:=unaligned(PPtrUint(p)^);
              result:=p+sizeof(PtrUInt);
            end;
          DW_EH_PE_uleb128:
            begin
              result:=fpc_read_uleb128(p,res);
            end;
          DW_EH_PE_sleb128:
            begin
              result:=fpc_read_sleb128(p,tmpres);
              res:=PtrUInt(tmpres);;
            end;
          DW_EH_PE_udata2:
            begin
              res:=unaligned(pword(p)^);
              result:=p+2;
            end;
          DW_EH_PE_udata4:
            begin
              res:=unaligned(pdword(p)^);
              result:=p+4;
            end;
          DW_EH_PE_udata8:
            begin
              res:=unaligned(pqword(p)^);
              result:=p+8;
            end;
          DW_EH_PE_sdata2:
            begin
              res:=PtrUInt(unaligned(psmallint(p)^));
              result:=p+2;
            end;
          DW_EH_PE_sdata4:
            begin
              res:=PtrUInt(unaligned(plongint(p)^));
              result:=p+4;
            end;
          DW_EH_PE_sdata8:
            begin
              res:=PtrUInt(unaligned(pint64(p)^));
              result:=p+8;
            end;
          else
            begin
{$ifdef excdebug}
              writeln('Unsupported encoding of value with base: $', hexstr(encoding,sizeof(encoding)*2));
{$endif}
              halt(217);
            end;
        end;
        if res<>0 then
          begin
            if (encoding and $70)=DW_EH_PE_pcrel then
              inc(res,PtrUInt(p))
            else
              inc(res, base);
            if (encoding and DW_EH_PE_indirect)<>0 then
              res:=PPtrUInt(res)^;
          end;
      end;
    val:=res;
  end;

function FPC_psabieh_read_encoded_value (context: PFPC_Unwind_Context; encoding: byte; p: PByte; out val: PtrUInt): PByte; inline;
  begin
    result:=FPC_psabieh_read_encoded_value_with_base(encoding,FPC_psabieh_base_of_encoded_value(encoding,context),p,val);
  end;

type
  FPC_psabieh_lsda_header_info = record
    Start: PtrUInt;
    LPStart: PtrUInt;
    ttype_base: PtrUInt;
    TType: Pointer;
    action_table: pointer;
    ttype_encoding: byte;
    call_site_encoding: byte;
  end;

function FPC_psabieh_parse_lsda_header(context: PFPC_Unwind_Context; p: PByte; out info: FPC_psabieh_lsda_header_info): PByte;
  var
    tmp: PTRUint;
    lpstart_encoding: byte;
  begin
    if assigned(context) then
      info.Start:=_Unwind_GetRegionStart(context)
    else
      info.Start:=0;

    // Find @LPStart, the base to which landing pad offsets are relative.
    lpstart_encoding:=p^;
    inc(p);
    if lpstart_encoding<>DW_EH_PE_omit then
      p:=FPC_psabieh_read_encoded_value(context,lpstart_encoding,p,info.LPStart)
    else
      info.LPStart:=info.Start;

    // Find @TType, the base of the handler and exception spec type data.
    info.ttype_encoding:=p^;
    inc(p);
    if info.ttype_encoding<>DW_EH_PE_omit then
      begin
        p:=fpc_read_uleb128(p,tmp);
        info.TType:=p+tmp;
      end
    else
      info.TType:=nil;

    // The encoding and length of the call-site table; the action table
    // immediately follows.
    info.call_site_encoding:=p^;
    inc(p);
    p:=fpc_read_uleb128(p,tmp);
    info.action_table:=p+tmp;
{$ifdef excdebug}
    writeln('lsda header');
    writeln('  * start: $',hexstr(info.start,sizeof(info.start)*2));
    writeln('  * lp_start encoding: $',hexstr(lpstart_encoding,sizeof(lpstart_encoding)*2));
    writeln('  * lp_start: $',hexstr(info.LPStart,sizeof(info.LPStart)*2));
    writeln('  * ttype_encoding: $',hexstr(info.ttype_encoding,sizeof(info.ttype_encoding)*2));
    writeln('  * ttype base: $',hexstr(info.TType));
    writeln('  * call_site_encoding: $',hexstr(info.call_site_encoding,sizeof(info.call_site_encoding)*2));
    writeln('  * action table: $', hexstr(p),' (offset: ',tmp,')');
{$endif}
    result:=p;
  end;

{$ifdef __ARM_EABI_UNWINDER__}
function FPC_psabieh_Unwind_decode_target2(ptr: PtrUInt {_Unwind_Word}):  PtrUInt {_Unwind_Word}; inline;
  begin
    result:=PPtrUInt(ptr)^;
    // Zero values are always NULL.
    if result<>0 then
      begin
{$if defined(linux) or defined(netbsd)}
        // Pc-relative indirect.
        inc(result,ptr);
        result:=PPtrUint(result)^;
{$else}
        // Pc-relative pointer.
        inc(result,ptr);
{$endif}
      end;
  end;
{$endif __ARM_EABI_UNWINDER__}


// Return an element from a type table.
{$ifdef __ARM_EABI_UNWINDER__}
function FPC_psabieh_get_ttype_entry(const info: FPC_psabieh_lsda_header_info; i: PtrUInt {_Unwind_Word}): TClass;
var
  ptr: PtrUInt {_Unwind_Word};
begin
  ptr:=PtrUInt(info.TType)-(i*4);
  ptr:=FPC_psabieh_Unwind_decode_target2(ptr);
  result:=TClass(ptr);
end;

{$else}

function FPC_psabieh_get_ttype_entry(const info: FPC_psabieh_lsda_header_info; i: PtrUInt): TClass;
  var
    ptr: PtrUInt;
  begin
    i:=i*FPC_psabieh_size_of_encoded_value(info.ttype_encoding);
    FPC_psabieh_read_encoded_value_with_base(info.ttype_encoding,info.ttype_base,info.TType-i,ptr);
    result:=TClass(ptr);
  end;
{$endif}


// Return true if THROW_TYPE matches one if the filter types.
function FPC_psabieh_check_exception_spec(const info: FPC_psabieh_lsda_header_info; thrown: TObject; filter_value: PtrInt): boolean;
  var
    e: PByte;
    catch_type: TClass;
    tmp: PtrUInt;
  begin
    e:=info.TType - filter_value - 1;
    repeat
      e:=fpc_read_uleb128(e,tmp);
      // Zero signals the end of the list.  If we've not found
      // a match by now, then we've failed the specification.
      if tmp=0 then
        exit(false);

{$ifdef __ARM_EABI_UNWINDER__}
      tmp:=FPC_psabieh_Unwind_decode_target2(PtrUInt(e)); {_Unwind_Word}
{$endif}
      // Match a ttype entry.
      catch_type:=FPC_psabieh_get_ttype_entry(info,tmp);

    until thrown is catch_type;
    result:=true;
  end;


{$ifdef __ARM_EABI_UNWINDER__}
// Save stage1 handler information in the exception object
procedure FPC_psabieh_save_caught_exception(ue_header: PFPC_Unwind_Exception;
                      context: PFPC_Unwind_Context;
                      handler_switch_value: longint;
                      language_specific_data: PByte;
                      landing_pad: PtrUInt);
  begin
    with ue_header^.barrier_cache do
      begin
        sp:=_Unwind_GetGR(context,13);
        { bitpattern[0] is assigned but never used in the original code }
        bitpattern[1]:=handler_switch_value;
        bitpattern[2]:=PtrUInt(language_specific_data);
        bitpattern[3]:=landing_pad;
      end;
  end;

// Restore the catch handler information saved during phase1.
procedure FPC_psabieh_restore_caught_exception(ue_header: PFPC_Unwind_Exception;
                         out handler_switch_value: longint;
                         out language_specific_data: PByte;
                         out landing_pad: PtrUInt);
  begin
    with ue_header^.barrier_cache do
      begin
        handler_switch_value:=longint(bitpattern[1]);
        language_specific_data:=PByte(bitpattern[2]);
        landing_pad:=bitpattern[3];
      end;
  end;

{$else __ARM_EABI_UNWINDER__}

// Save stage1 handler information in the exception object
procedure FPC_psabieh_save_caught_exception(ue_header: PFPC_Unwind_Exception;
                      context: PFPC_Unwind_Context;
                      handler_switch_value: longint;
                      language_specific_data: PByte;
                      landing_pad: PtrUInt);
  var
    xh: PExceptObject;
  begin
    xh:=FPC_psabieh_GetExceptionWrapper(ue_header);
    xh^.handler_switch_value:=handler_switch_value;
    xh^.language_specific_data:=language_specific_data;
    xh^.landing_pad:=landing_pad;
  end;

// Restore the catch handler information saved during phase1.
procedure FPC_psabieh_restore_caught_exception(ue_header: PFPC_Unwind_Exception;
                         out handler_switch_value: longint;
                         out language_specific_data: PByte;
                         out landing_pad: PtrUInt);
  var
    xh: PExceptObject;
  begin
    xh:=FPC_psabieh_GetExceptionWrapper(ue_header);
    handler_switch_value:=xh^.handler_switch_value;
    language_specific_data:=xh^.language_specific_data;
    landing_pad:=xh^.landing_pad;
  end;
{$endif __ARM_EABI_UNWINDER__}

function FPC_psabieh_find_action_record(const info: FPC_psabieh_lsda_header_info; var p: PByte; const ip: PTRUint; var landing_pad: PtrUInt; var action_record: PByte): boolean;
  var
    cs_start, cs_len, cs_lp: PtrUint{_Unwind_Ptr};
    cs_action: PtrUInt {_Unwind_Word};
  begin
    result:=false;
{$ifdef excdebug}
    writeln('find action record start: p: $',hexstr(p),'; lsda covered code start: $',hexstr(info.Start,sizeof(info.start)*2),'; lsda action table: $',hexstr(info.action_table),'; lsda call site encoding: $',hexstr(info.call_site_encoding,2),'; ip: $', hexstr(ip,sizeof(ip)*2));
{$endif}
    while (p<info.action_table) do
      begin
        // Note that all call-site encodings are "absolute" displacements.
        p:=FPC_psabieh_read_encoded_value (nil, info.call_site_encoding, p, cs_start);
        p:=FPC_psabieh_read_encoded_value (nil, info.call_site_encoding, p, cs_len);
        p:=FPC_psabieh_read_encoded_value (nil, info.call_site_encoding, p, cs_lp);
        p:=FPC_read_uleb128 (p, &cs_action);

{$ifdef excdebug}
        writeln('find action record: cs_start: $',hexstr(cs_start,sizeof(cs_start)*2),', len: ',cs_len,
          ' (ip=$',hexstr(info.Start+cs_start,sizeof(PtrUInt)*2),'...$',hexstr(info.Start+cs_start+cs_len,sizeof(PtrUInt)*2),')',
          ', lp: ', cs_lp,' action ofs: ',cs_action);
{$endif}
        // The table is sorted, so if we've passed the ip, stop.
        if ip<info.Start+cs_start then
          p:=info.action_table
        else if (ip<(info.Start+cs_start+cs_len)) then
          begin
            if cs_lp<>0 then
              landing_pad:=info.LPStart+cs_lp;
            if cs_action<>0 then
              action_record:=info.action_table+cs_action-1;
{$ifdef excdebug}
            writeln('action record result: action_record: $',hexstr(cs_start,sizeof(cs_start)*2),', len: ',cs_len,', lp: ', cs_lp,
              ',landing_pad: $',hexstr(landing_pad,sizeof(landing_pad)*2));
{$endif}
            result:=true;
            exit;
          end;
      end;
{$ifdef excdebug}
    writeln('find action record failed');
{$endif}
  end;


// Return true if the filter spec is empty, ie throw().

function fpc_psabieh_empty_exception_spec(const info: FPC_psabieh_lsda_header_info; const filter_value: PtrInt {_Unwind_Sword}): boolean;
  var
    e: PByte;
    tmp: PtrUInt;
  begin
    e:=PByte(info.ttype - filter_value - 1);
    e:=fpc_read_uleb128(e,tmp);
    result:=tmp = 0;
  end;

type
  FPC_psabieh_found_handler_type = (
    found_nothing,
    found_terminate,
    found_cleanup,
    found_handler
  );

function FPC_psabieh_find_handler(const info: FPC_psabieh_lsda_header_info; const foreign_exception: boolean; actions: FPC_Unwind_Action; thrown: TObject; var action_record: PByte; var handler_switch_value: longint): FPC_psabieh_found_handler_type;
  var
    ar_filter, ar_disp: PtrInt;
    catch_type: TClass;
    throw_type: TOBject;
    saw_cleanup, saw_handler: boolean;
    p: PByte;
  begin
    saw_cleanup:=false;
    saw_handler:=false;

    // During forced unwinding, we only run cleanups.  With a foreign
    // exception class, there's no exception type.
    if ((actions and FPC_UA_FORCE_UNWIND)<>0) or
       foreign_exception then
      throw_type:=nil
    else
      throw_type:=thrown;

    while true do
      begin
        p:=action_record;
        p:=fpc_read_sleb128(p,ar_filter);
        fpc_read_sleb128(p,ar_disp);

        if ar_filter=0 then
          begin
            // Zero filter values are cleanups.
            saw_cleanup:=true;
          end
        else if ar_filter>0 then
          begin
            // Positive filter values are handlers.
            catch_type:=FPC_psabieh_get_ttype_entry(info,ar_filter);

            // Null catch type is a catch-all handler; we can catch foreign
            // exceptions with this.  Otherwise we must match types.
            if not assigned(catch_type) or
               (assigned(throw_type) and
                (throw_type is catch_type)) then
              begin
                saw_handler:=true;
                break;
              end
          end
        else
          begin
            // Negative filter values are exception specifications.
            // ??? How do foreign exceptions fit in?  As far as I can
            // see we can't match because there's no __cxa_exception
            // object to stuff bits in for __cxa_call_unexpected to use.
            // Allow them iff the exception spec is non-empty.  I.e.
            // a throw() specification results in __unexpected.
            if (assigned(throw_type) and
                not FPC_psabieh_check_exception_spec(info,thrown,ar_filter)) or
               (not assigned(throw_type) and
                FPC_psabieh_empty_exception_spec(info,ar_filter)) then
              begin
                saw_handler:=true;
                break;
              end;
            end;

        if ar_disp=0 then
          break;
        action_record:=p+ar_disp;
      end;

    if saw_handler then
      begin
        handler_switch_value:=ar_filter;
        result:=found_handler;
      end
    else
      begin
        if saw_cleanup then
          result:=found_cleanup
        else
          result:=found_nothing;
      end;
  end;

{$ifdef FPC_PSABIEH_CPLUSPLUSSUPPORT}
procedure __gxx_personality_v0(version: cint; actions: FPC_Unwind_Action; exceptionClass: cuint64; libunwind_exception: PFPC_Unwind_Exception; context: PFPC_Unwind_Context); cdecl; external;
{$endif FPC_PSABIEH_CPLUSPLUSSUPPORT}


{$ifdef __ARM_EABI_UNWINDER__}
function continue_unwinding(libunwind_exception: PFPC_Unwind_Exception; context: PFPC_Unwind_Context): FPC_Unwind_Reason_Code; inline;
  begin
    if __gnu_unwind_frame(libunwind_exception, context)<>FPC_URC_OK then
      result:=FPC_URC_FAILURE
    else
      result:=FPC_URC_CONTINUE_UNWIND;
  end;

function _FPC_psabieh_personality_v0(state: FPC_Unwind_State; libunwind_exception: PFPC_Unwind_Exception; context: PFPC_Unwind_Context): FPC_Unwind_Reason_Code; cdecl;
{$else}

function continue_unwinding(libunwind_exception: PFPC_Unwind_Exception; context: PFPC_Unwind_Context): FPC_Unwind_Reason_Code; inline;
  begin
    result:=FPC_URC_CONTINUE_UNWIND;
  end;

function _FPC_psabieh_personality_v0(version: longint; actions: FPC_Unwind_Action; exceptionClass: qword; libunwind_exception: PFPC_Unwind_Exception; context: PFPC_Unwind_Context): FPC_Unwind_Reason_Code; cdecl;
{$endif}
  var
    WrappedException: PExceptObject;
    found_type: FPC_psabieh_found_handler_type;
    info: FPC_psabieh_lsda_header_info;
    language_specific_data: PByte;
    action_record: PByte;
    p: PByte;
    landing_pad, ip: PtrUInt; { _Unwind_Ptr }
    handler_switch_value: longint;
    foreign_exception: boolean;
{$ifdef __ARM_EABI_UNWINDER__}
    actions: FPC_Unwind_Action;
{$endif}
  begin
{$ifdef __ARM_EABI_UNWINDER__}
    { convert the state flags to FPC_Unwind_Action flags so we can share the rest of the code }
    case (state and FPC_US_ACTION_MASK) of
      FPC_US_VIRTUAL_UNWIND_FRAME:
        begin
          actions:=FPC_UA_SEARCH_PHASE;
        end;
      FPC_US_UNWIND_FRAME_STARTING:
        begin
          actions:=FPC_UA_CLEANUP_PHASE;
          if ((state and FPC_US_FORCE_UNWIND)<>0) and
             (libunwind_exception^.barrier_cache.sp=_Unwind_GetGR(context,13)) then
            actions:=actions or FPC_UA_HANDLER_FRAME;
        end;
      FPC_US_UNWIND_FRAME_RESUME:
        begin
          result:=continue_unwinding(libunwind_exception,context);
          exit;
        end;
    end;
    actions:=actions or (state and FPC_US_FORCE_UNWIND);

    // The dwarf unwinder assumes the context structure holds things like the
    // function and LSDA pointers.  The ARM implementation caches these in
    // the exception header (UCB).  To avoid rewriting everything we make the
    // virtual IP register point at the UCB.
    ip:=PtrUInt(libunwind_exception);
    _Unwind_SetGR(context, 12, ip);

    { foreign exception type -> let c++ runtime handle it }
    foreign_exception:=libunwind_exception^.exception_class<>FPC_psabieh_exceptionClass_ID.u;
{$else __ARM_EABI_UNWINDER__}
{ unsupported version -> failure }
    if version<>1 then
      begin
        result:=FPC_URC_FATAL_PHASE1_ERROR;
        exit;
      end;

    { foreign exception type -> let c++ runtime handle it }
    foreign_exception:=exceptionClass<>FPC_psabieh_exceptionClass_ID.u;
{$endif __ARM_EABI_UNWINDER__}

{$ifdef FPC_PSABIEH_CPLUSPLUSSUPPORT}
    if foreign_exception then
      begin
        result:=__gxx_personality_v0(version, actions, exceptionClass, libunwind_exception, context)
        exit;
      end;
{$endif FPC_PSABIEH_CPLUSPLUSSUPPORT}

    WrappedException:=FPC_psabieh_GetExceptionWrapper(libunwind_exception);

{$ifdef excdebug}
    writeln('Personality (version = ',{$ifndef __ARM_EABI_UNWINDER__}version{$else}0{$endif},', actions = $',hexstr(actions,4),') started for wrapper ',hexstr(WrappedException),' = fpc exc ',hexstr(WrappedException^.FObject),
      ', refcount is now ',WrappedException^.refcount);
    writeln('  ip=$',hexstr(_Unwind_GetIP(context),sizeof(pointer)*2));
{$endif}
    // Shortcut for phase 2 found handler for domestic exception.
    if (actions=(FPC_UA_CLEANUP_PHASE or FPC_UA_HANDLER_FRAME)) and
       not foreign_exception then
      begin
{$ifdef excdebug}
        writeln('restoring caught exception');
{$endif}
        FPC_psabieh_restore_caught_exception(libunwind_exception,handler_switch_value,
                                 language_specific_data,landing_pad);
{$ifdef excdebug}
        writeln('restoring caught exception, landing_pad = $',hexstr(landing_pad,sizeof(landing_pad)*2));
{$endif}
        if landing_pad<>0 then
          found_type:=found_handler
        else
          found_type:=found_terminate;
      end
    else
      begin
        language_specific_data:=PByte(_Unwind_GetLanguageSpecificData(context));

        // If no LSDA, then there are no handlers or cleanups.
        if not assigned(language_specific_data) then
          begin
{$ifdef excdebug}
            writeln('did not find lsda for ip $',hexstr(_Unwind_GetIP(context),sizeof(pointer)*2));
{$endif}

            exit(continue_unwinding(libunwind_exception,context));
          end;

        // Parse the LSDA header.
        p:=FPC_psabieh_parse_lsda_header(context,language_specific_data,info);
        info.ttype_base:=FPC_psabieh_base_of_encoded_value(info.ttype_encoding,context);
        ip:=_Unwind_GetIP(context);
        dec(ip);
        landing_pad:=0;
        action_record:=nil;
        handler_switch_value:=0;

        // Search the call-site table for the action associated with this IP.
        if FPC_psabieh_find_action_record(info,p,ip,landing_pad,action_record) then
          begin
{$ifdef excdebug}
            writeln('found action record for ip ',hexstr(_Unwind_GetIP(context),sizeof(pointer)*2));
{$endif}
            if landing_pad=0 then
              begin
                // If ip is present, and has a null landing pad, there are
                // no cleanups or handlers to be run.
                found_type:=found_nothing;
              end
            else if action_record=nil then
              begin
                // If ip is present, has a non-null landing pad, and a null
                // action table offset, then there are only cleanups present.
                // Cleanups use a zero switch value, as set above.
                found_type:=found_cleanup;
              end
            else
              begin
                // Otherwise we have a catch handler or exception specification.
                found_type:=FPC_psabieh_find_handler(info,foreign_exception,actions,WrappedException^.FObject,action_record,handler_switch_value);
              end
          end
        else
          begin
            // If ip is not present in the table, call terminate.  This is for
            // a destructor inside a cleanup, or a library routine the compiler
            // was not expecting to throw.
            found_type:=found_terminate;
          end;
{$ifdef excdebug}
        writeln('find_handler: ',found_type);
{$endif}

         if found_type=found_nothing then
           exit(continue_unwinding(libunwind_exception,context));

        if (actions and FPC_UA_SEARCH_PHASE)<>0 then
          begin
            if found_type=found_cleanup then
              exit(continue_unwinding(libunwind_exception,context));

            if not foreign_exception then
              begin
{$ifdef excdebug}
                writeln('saving native exception: $',hexstr(landing_pad,sizeof(landing_pad)*2));
{$endif}
                // For domestic exceptions, we cache data from phase 1 for phase 2.
                FPC_psabieh_save_caught_exception(libunwind_exception,context,
                                        handler_switch_value,language_specific_data,
                                        landing_pad);
              end;
            exit(FPC_URC_HANDLER_FOUND);
          end;
      end;

    if ((actions and FPC_UA_FORCE_UNWIND)<>0) or
       foreign_exception then
      begin
        if found_type=found_terminate then
          begin
{$ifdef excdebug}
            writeln('foreign exception or force unwind, and found type = found terminate; actions = $',hexstr(actions,sizeof(actions)*2),'; foreign exception ', foreign_exception);
{$endif}
            DoUnHandledException;
          end
        { can only perform cleanups when force-unwinding }
        else if handler_switch_value<0 then
          begin
{$ifdef excdebug}
            writeln('foreign exception or force unwind, handler_switch_value < 0: ', handler_switch_value);
{$endif}
            DoUnHandledException;
          end
      end
    else
      begin
        if found_type=found_terminate then
          begin
{$ifdef excdebug}
            writeln('native exception and no force unwind, and force_terminate');
{$endif}
            DoUnHandledException;
          end
        else if handler_switch_value<0 then
          begin
            { C++ calls __cxa_call_unexpected in this case }
{$ifdef excdebug}
            writeln('native exception and no force unwind, and handler_switch_value<0: ', handler_switch_value);
{$endif}
            DoUnHandledException;
          end;
      end;
    { For targets with pointers smaller than the word size, we must extend the
       pointer, and this extension is target dependent.  }
    {$if sizeof(pointer)<>sizeof(SizeInt)}
      {$error Add support for extending pointer values}
    {$endif}
{$ifdef excdebug}
    writeln('returning exception $',hexstr(libunwind_exception),' with switch value ',handler_switch_value);
{$endif}
    _Unwind_SetGR(context,fpc_eh_return_data_regno(0),PtrUInt(libunwind_exception));
    _Unwind_SetGR (context,fpc_eh_return_data_regno(1),handler_switch_value);
    _Unwind_SetIP(context,landing_pad);
    result:=FPC_URC_INSTALL_CONTEXT;
  end;

//////////////////////////////
///// Raising an exception
//////////////////////////////

procedure FPC_psabieh_ExceptionCleanUp(reason: FPC_Unwind_Reason_Code; exc:PFPC_Unwind_Exception); cdecl;
  var
    ExceptWrapper: PExceptObject;
  begin
    // If we haven't been caught by a foreign handler, then this is
    // some sort of unwind error.  In that case just die immediately.
    // _Unwind_DeleteException in the HP-UX IA64 libunwind library
    //  returns _URC_NO_REASON and not _URC_FOREIGN_EXCEPTION_CAUGHT
    // like the GCC _Unwind_DeleteException function does.
    if (reason<>FPC_URC_FOREIGN_EXCEPTION_CAUGHT) and
       (reason<>FPC_URC_NO_REASON) then
      begin
{$ifdef excdebug}
        writeln('exception cleanup and reason not foreign exception or no reason, reason = $',hexstr(reason,sizeof(reason)*2));
{$endif}
        halt(217);
      end;

    ExceptWrapper:=FPC_psabieh_GetExceptionWrapper(exc);
{$ifdef excdebug}
        writeln('exception cleanup: deleting wrapper ',hexstr(ExceptWrapper),' and fpc exception ',hexstr(ExceptWrapper^.FObject));
{$endif}
    ExceptWrapper^.FObject.free;
    ExceptWrapper^.FObject:=nil;
    if assigned(ExceptWrapper^.frames) then
      freemem(ExceptWrapper^.frames);
    Dispose(ExceptWrapper);
  end;

function PushExceptObject(Obj : TObject; AnAddr : CodePointer; AFrame : Pointer): PExceptObject; forward;

{$define FPC_SYSTEM_HAS_RAISEEXCEPTION}
procedure fpc_RaiseException(Obj: TObject; AnAddr: CodePointer; AFrame: Pointer); compilerproc;
var
  _ExceptObjectStack : PExceptObject;
  _ExceptAddrstack : PExceptAddr;
  ExceptWrapper: PExceptObject;
  RaiseResult: FPC_Unwind_Reason_Code;
begin
{$ifdef excdebug}
  writeln ('In psabieh RaiseException for object ',hexstr(obj),' of class type ',obj.classname);
{$endif}
  if ExceptTryLevel<>0 then
    begin
{$ifdef excdebug}
      writeln('exception while raising exception, aborting');
{$endif}
      Halt(217);
    end;
  ExceptTryLevel:=1;
  ExceptWrapper:=PushExceptObject(Obj,AnAddr,AFrame);
  ExceptWrapper^.unwind_exception.exception_class:=FPC_psabieh_exceptionClass_ID.u;
  ExceptWrapper^.unwind_exception.exception_cleanup:=@FPC_psabieh_ExceptionCleanUp;
  { if PushExceptObject causes another exception, the following won't be executed,
    causing halt upon entering this routine recursively. }
  ExceptTryLevel:=0;
  _ExceptObjectStack:=ExceptObjectStack;
  if (RaiseProc <> nil) and (_ExceptObjectStack <> nil) then
    with _ExceptObjectStack^ do
      RaiseProc(FObject,Addr,FrameCount,Frames);
  RaiseResult:=_Unwind_RaiseException(@ExceptWrapper^.unwind_exception);
  // Only returns if there is no exception catching block anymore
{$ifdef excdebug}
  writeln('_Unwind_RaiseException returned: ',RaiseResult);
{$endif}
  DoUnHandledException;
end;

{$ifdef FPC_PSABIEH_CPLUSPLUSSUPPORT}
function __cxa_begin_catch(exc:PFPC_Unwind_Exception): pointer; cdecl; external;
{$endif FPC_PSABIEH_CPLUSPLUSSUPPORT}

function FPC_psabi_begin_catch(exc:PFPC_Unwind_Exception): pointer; compilerproc;
  var
    ExceptWrapper: PExceptObject;
    _ExceptObjectStack : PExceptObject;
    count: longint;
  begin
{$ifdef excdebug}
    writeln('start begin_catch unwind exception ',hexstr(exc));
{$endif}
    _ExceptObjectStack:=ExceptObjectStack;
    // hand off foreign exceptions to the C++ runtime
    if exc^.exception_class<>FPC_psabieh_exceptionClass_ID.u then
      begin
        // Can't link foreign exceptions with our stack
        if assigned(_ExceptObjectStack) then
          begin
{$ifdef excdebug}
            writeln('begin catch for nested foreign exception');
{$endif}
            DoUnHandledException;
          end;
        // This is a wrong conversion, but as long as afterwards we only access
        // fields of PFPC_Unwind_Exception, it's fine
        _ExceptObjectStack:=FPC_psabieh_GetExceptionWrapper(exc);
{$ifdef FPC_PSABIEH_CPLUSPLUSSUPPORT}
        result:=__cxa_begin_catch(exc);
{$else}
         // ??? No sensible value to return; we don't know what the
         // object is, much less where it is in relation to the header.
        result:=nil;
{$endif}
        exit;
      end;

    ExceptWrapper:=FPC_psabieh_GetExceptionWrapper(exc);

    count:=ExceptWrapper^.refcount;
    // Count is less than zero if this exception was rethrown from an
    // immediately enclosing region.
    if count < 0 then
      count:=-count+1
    else
      inc(count);
    ExceptWrapper^.refcount:=count;
//    globals->uncaughtExceptions -= 1;
    if _ExceptObjectStack<>ExceptWrapper then
      begin
        ExceptWrapper^.Next:=_ExceptObjectStack;
        ExceptObjectStack:=ExceptWrapper;
      end;

{$ifdef excdebug}
    writeln('stop begin_catch for wrapper ',hexstr(ExceptWrapper),' = fpc exc ',hexstr(ExceptWrapper^.FObject),', refcount is now ',count);
{$endif}
    result:= ExceptWrapper^.FObject;
{$ifdef __ARM_EABI_UNWINDER__}
    _Unwind_Complete(exc);
{$endif}
  end;

{$ifdef FPC_PSABIEH_CPLUSPLUSSUPPORT}
procedure __cxa_end_catch; cdecl; external;
{$endif FPC_PSABIEH_CPLUSPLUSSUPPORT}

procedure FPC_psabi_end_catch; cdecl; compilerproc;
  var
    _ExceptObjectStack: PExceptObject;
    refcount: longint;
  begin
    _ExceptObjectStack:=ExceptObjectStack;
{$ifdef excdebug}
    writeln('start end_catch unwind exception ',hexstr(@_ExceptObjectStack^.unwind_exception));
{$endif}
    // A rethrow of a foreign exception will be removed from the
    // the exception stack immediately by __cxa_rethrow -> stack could be empty here
    if not assigned(_ExceptObjectStack) then
      exit;

    // Pass foreign exception to the C++ runtime
    if _ExceptObjectStack^.unwind_exception.exception_class<>FPC_psabieh_exceptionClass_ID.u then
      begin
        { remove foreign exception; since we never link multiple foreign
          exceptions, we know the stack is now empty }
        ExceptObjectStack:=nil;
{$ifdef FPC_PSABIEH_CPLUSPLUSSUPPORT}
        __cxa_end_catch();
{$else}
       _Unwind_DeleteException(@_ExceptObjectStack^.unwind_exception);
{$endif}
        exit;
      end;

    refcount:=_ExceptObjectStack^.refcount;
{$ifdef excdebug}
    writeln('middle end_catch for wrapper ',hexstr(_ExceptObjectStack),' = fpc exception ',hexstr(_ExceptObjectStack^.FObject),' with refcount ',refcount);
{$endif}
    if refcount<0 then
      begin
        { Can happen in the original glibc code, but not for us. When re-raising an
          exception, we always immediately do this to an outer frame }
        halt(217);
      end
    else
      begin
        dec(refcount);
{$ifdef excdebug}
        writeln('stop end_catch, not rethrown, new refcount: ',refcount);
{$endif}
        if refcount=0 then
          begin
            // Handling for this exception is complete.  Destroy the object.
            ExceptObjectStack:=_ExceptObjectStack^.next;
            _Unwind_DeleteException(@_ExceptObjectStack^.unwind_exception);
            exit;
          end
        else if refcount<0 then
          begin
            // A bug in the exception handling library or compiler.
{$ifdef excdebug}
            writeln('refcount for exception is negative in end catch');
{$endif}
            RunError(217);
          end;
      end;
    _ExceptObjectStack^.refcount:=refcount;
  end;

{$ifdef FPC_PSABIEH_CPLUSPLUSSUPPORT}
procedure __cxa_rethrow; cdecl; external; noreturn;
{$endif FPC_PSABIEH_CPLUSPLUSSUPPORT}

{$define FPC_SYSTEM_HAS_RERAISE}
procedure fpc_ReRaise; [public,alias:'FPC_RERAISE']; compilerproc;
  var
    _ExceptObjectStack: PExceptObject;
    refcount: longint;
    reraise_error: FPC_Unwind_Reason_Code;
  begin
    _ExceptObjectStack:=ExceptObjectStack;
    // globals->uncaughtExceptions += 1;

{$ifdef excdebug}
    writeln('start reraise for wrapper ',hexstr(_ExceptObjectStack));
{$endif}
    // Watch for luser rethrowing with no active exception.
    if assigned(_ExceptObjectStack) then
      begin
        // Tell __cxa_end_catch this is a rethrow.
        if _ExceptObjectStack^.unwind_exception.exception_class<>FPC_psabieh_exceptionClass_ID.u then
{$ifdef FPC_PSABIEH_CPLUSPLUSSUPPORT}
          begin
            { remove foreign exception; since we never link multiple foreign
              exceptions, we know the stack is now empty }
            ExceptObjectStack:=nil;
            __cxa_rethrow;
            { should never be reached }
            DoUnHandledException;
          end
{$endif FPC_PSABIEH_CPLUSPLUSSUPPORT}
        else
          begin
            { reraise has to reset the refcount, this is also what the
              generic exception handling does }
            _ExceptObjectStack^.refcount := 0;
          end;

{$ifdef excdebug}
    writeln('Stop reraise, new refcount = ',_ExceptObjectStack^.refcount);
{$endif}
//  #ifdef _GLIBCXX_SJLJ_EXCEPTIONS
//        _Unwind_SjLj_Resume_or_Rethrow (&header->unwindHeader);
//  #else
//  #if defined(_LIBUNWIND_STD_ABI)
//        _Unwind_RaiseException (@_ExceptObjectStack^.unwind_exception);
//  #else
        reraise_error:=_Unwind_Resume_or_Rethrow (@_ExceptObjectStack^.unwind_exception);
{$ifdef excdebug}
        writeln('reraise failed, error = ',reraise_error);
{$endif}
//  #endif
//  #endif
        // Some sort of unwinding error.
        DoUnHandledException;
      end;
    DoUnHandledException;
  end;

{$define FPC_SYSTEM_HAS_RAISENESTED}
procedure fpc_raise_nested;compilerproc;
  var
    hp, _ExceptObjectStack: PExceptObject;
  begin
    _ExceptObjectStack:=ExceptObjectStack;
    if not(assigned(_ExceptObjectStack)) or
       not(assigned(_ExceptObjectStack^.next)) then
      begin
{$ifdef excdebug}
        writeln ('raise_nested: At end of ExceptionObjectStack');
{$endif}
        halt(1);
      end;

    if _ExceptObjectStack^.unwind_exception.exception_class<>FPC_psabieh_exceptionClass_ID.u then
      begin
{$ifdef excdebug}
        writeln ('raise_nested: top of stack contains foreign exception');
{$endif}
        halt(1);
      end;

    hp:=_ExceptObjectStack^.next;
    _ExceptObjectStack^.next:=hp^.next;
{$ifdef excdebug}
    writeln('raise_nested: raising nested wrapper ',hexstr(_ExceptObjectStack),' = fpc exception ',hexstr(_ExceptObjectStack^.FObject),' with refcount ',_ExceptObjectStack^.refcount{,' (will increase to ',_ExceptObjectStack^.refcount+1,')'});
    writeln('raise_nested: previous exception ',hexstr(hp),' = fpc exception ',hexstr(hp^.FObject),' with refcount ',hp^.refcount,' (will delete if refcount = 1, otherwise decrease to',hp^.refcount-1,')');
{$endif}
    if hp^.refcount=1 then
      { we need to free the original exception object if its refcount=1
        (means it was not acquired, only refcount increase by begin_catch) }
      _Unwind_DeleteException(@hp^.unwind_exception)
    else
      dec(hp^.refcount);
    _Unwind_RaiseException(@_ExceptObjectStack^.unwind_exception);
    DoUnHandledException;
  end;

procedure FPC_DummyPotentialRaise; nostackframe; assembler;
  asm
  end;
