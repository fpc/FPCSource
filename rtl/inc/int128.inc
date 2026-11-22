
{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    This file contains some helper routines for int128 and uint128

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$Q- no overflow checking }
{$R- no range checking }

    type
{$ifdef ENDIAN_LITTLE}
       tuint128rec = packed record
         low : qword;
         high : qword;
       end;
{$endif ENDIAN_LITTLE}
{$ifdef ENDIAN_BIG}
       tuint128rec = packed record
         high : qword;
         low : qword;
       end;
{$endif ENDIAN_BIG}


{$ifdef  FPC_INCLUDE_SOFTWARE_SHIFT_INT128}

{$ifndef FPC_SYSTEM_HAS_SHL_UINT128}
    function fpc_shl_uint128(value : uint128;shift : ALUUInt) : uint128; [public,alias: 'FPC_SHL_UINT128']; compilerproc;
      begin
        shift:=shift and 127;
        if shift=0 then
          result:=value
        else if shift>63 then
          begin
            tuint128rec(result).low:=0;
            tuint128rec(result).high:=tuint128rec(value).low shl (shift-64);
          end
        else
          begin
            tuint128rec(result).low:=tuint128rec(value).low shl shift;
            tuint128rec(result).high:=(tuint128rec(value).high shl shift) or (tuint128rec(value).low shr (64-shift));
          end;
      end;
{$endif FPC_SYSTEM_HAS_SHL_UINT128}


{$ifndef FPC_SYSTEM_HAS_SHL_ASSIGN_UINT128}
    procedure fpc_shl_assign_uint128(var value : uint128;shift : ALUUInt); [public,alias: 'FPC_SHL_ASSIGN_UINT128']; compilerproc;
      begin
        shift:=shift and 127;
        if shift<>0 then
          begin
            if shift>63 then
              begin
                tuint128rec(value).high:=tuint128rec(value).low shl (shift-64);
                tuint128rec(value).low:=0;
              end
            else
              begin
                tuint128rec(value).high:=(tuint128rec(value).high shl shift) or (tuint128rec(value).low shr (64-shift));
                tuint128rec(value).low:=tuint128rec(value).low shl shift;
              end;
          end;
      end;
{$endif FPC_SYSTEM_HAS_SHL_ASSIGN_UINT128}


{$ifndef FPC_SYSTEM_HAS_SHR_UINT128}
   function fpc_shr_uint128(value : uint128;shift : ALUUInt) : uint128; [public,alias: 'FPC_SHR_UINT128']; compilerproc;
      begin
        shift:=shift and 127;
        if shift=0 then
          result:=value
        else if shift>63 then
          begin
            tuint128rec(result).high:=0;
            tuint128rec(result).low:=tuint128rec(value).high shr (shift-64);
          end
        else
          begin
            tuint128rec(result).high:=tuint128rec(value).high shr shift;
            tuint128rec(result).low:=(tuint128rec(value).low shr shift) or (tuint128rec(value).high shl (64-shift));
          end;
      end;
{$endif FPC_SYSTEM_HAS_SHR_UINT128}


{$ifndef FPC_SYSTEM_HAS_SHR_ASSIGN_UINT128}
   procedure fpc_shr_assign_uint128(var value : uint128;shift : ALUUInt); [public,alias: 'FPC_SHR_ASSIGN_UINT128']; compilerproc;
      begin
        shift:=shift and 127;
        if shift<>0 then
          begin
            if shift>63 then
              begin
                tuint128rec(value).low:=tuint128rec(value).high shr (shift-64);
                tuint128rec(value).high:=0;
              end
            else
              begin
                tuint128rec(value).low:=(tuint128rec(value).low shr shift) or (tuint128rec(value).high shl (64-shift));
                tuint128rec(value).high:=tuint128rec(value).high shr shift;
              end;
          end;
      end;
{$endif FPC_SYSTEM_HAS_SHR_ASSIGN_UINT128}


{$ifndef FPC_SYSTEM_HAS_SHL_INT128}
    function fpc_shl_int128(value : int128;shift : ALUUInt) : int128; [public,alias: 'FPC_SHL_INT128']; compilerproc;
      begin
        shift:=shift and 127;
        if shift=0 then
          result:=value
        else if shift>63 then
          begin
            tuint128rec(result).low:=0;
            tuint128rec(result).high:=tuint128rec(value).low shl (shift-64);
          end
        else
          begin
            tuint128rec(result).low:=tuint128rec(value).low shl shift;
            tuint128rec(result).high:=(tuint128rec(value).high shl shift) or (tuint128rec(value).low shr (64-shift));
          end;
      end;
{$endif FPC_SYSTEM_HAS_SHL_INT128}


{$ifndef FPC_SYSTEM_HAS_SHL_ASSIGN_INT128}
    procedure fpc_shl_assign_int128(var value : int128;shift : ALUUInt); [public,alias: 'FPC_SHL_ASSIGN_INT128']; compilerproc;
      begin
        shift:=shift and 127;
        if shift<>0 then
          begin
            if shift>63 then
              begin
                tuint128rec(value).high:=tuint128rec(value).low shl (shift-64);
                tuint128rec(value).low:=0;
              end
            else
              begin
                tuint128rec(value).high:=(tuint128rec(value).high shl shift) or (tuint128rec(value).low shr (64-shift));
                tuint128rec(value).low:=tuint128rec(value).low shl shift;
              end;
          end;
      end;
{$endif FPC_SYSTEM_HAS_SHL_ASSIGN_INT128}


{$ifndef FPC_SYSTEM_HAS_SHR_INT128}
    function fpc_shr_int128(value : int128;shift : ALUUInt) : int128; [public,alias: 'FPC_SHR_INT64']; compilerproc;
      begin
        shift:=shift and 127;
        if shift=0 then
          result:=value
        else if shift>63 then
          begin
            tuint128rec(result).high:=0;
            tuint128rec(result).low:=tuint128rec(value).high shr (shift-64);
          end
        else
          begin
            tuint128rec(result).high:=tuint128rec(value).high shr shift;
            tuint128rec(result).low:=(tuint128rec(value).low shr shift) or (tuint128rec(value).high shl (64-shift));
          end;
      end;
{$endif FPC_SYSTEM_HAS_SHR_INT128}


{$ifndef FPC_SYSTEM_HAS_SHR_ASSIGN_INT128}
    procedure fpc_shr_assign_int128(var value : int128;shift : ALUUInt); [public,alias: 'FPC_SHR_ASSIGN_INT128']; compilerproc;
      begin
        shift:=shift and 127;
        if shift<>0 then
          begin
            if shift>63 then
              begin
                tuint128rec(value).low:=tuint128rec(value).high shr (shift-64);
                tuint128rec(value).high:=0;
              end
            else
              begin
                tuint128rec(value).low:=(tuint128rec(value).low shr shift) or (tuint128rec(value).high shl (64-shift));
                tuint128rec(value).high:=tuint128rec(value).high shr shift;
              end;
          end;
      end;
{$endif FPC_SYSTEM_HAS_SHR_ASSIGN_INT128}


{$endif FPC_INCLUDE_SOFTWARE_SHIFT_INT128}


{$ifndef FPC_SYSTEM_HAS_MUL_UINT128}
    function fpc_mul_uint128(f1,f2 : uint128) : uint128;[public,alias: 'FPC_MUL_UINT128']; compilerproc;
      var
        b : byte;
      begin
        result:=0;

        for b:=0 to 127 do
          begin
            if odd(f2) then
              result:=result+f1;
            f1:=f1 shl 1;
            f2:=f2 shr 1;
          end;
      end;


    function fpc_mul_uint128_checkoverflow(f1,f2 : uint128) : uint128;[public,alias: 'FPC_MUL_UINT128_CHECKOVERFLOW']; compilerproc;
      var
         _f1,bitpos : uint128;
         b : byte;
         f1overflowed : boolean;
      begin
        result:=0;
        bitpos:=1;
        f1overflowed:=false;

        for b:=0 to 127 do
          begin
            if (f2 and bitpos)<>0 then
              begin
                _f1:=result;
                result:=result+f1;

                { if one of the operands is greater than the result an
                  overflow occurs                                      }
                if (f1overflowed or ((_f1<>0) and (f1<>0) and
                  ((_f1>result) or (f1>result)))) then
                  HandleErrorAddrFrameInd(215,get_pc_addr,get_frame);
              end;
            { when bootstrapping, we forget about overflow checking for uint128 :) }
            f1overflowed:=f1overflowed or ((f1 and (uint128(1) shl 127))<>0);
            f1:=f1 shl 1;
            bitpos:=bitpos shl 1;
          end;
      end;
{$endif FPC_SYSTEM_HAS_MUL_UINT128}
