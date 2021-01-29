{
    Copyright (c) 2017 by Nikolay Nikolov

    Optimizations for making use of load-modify-store operations in CISC-like
    instruction set architectures (such as x86)

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}

unit optloadmodifystore;

{$i fpcdefs.inc}

{$if defined(i386) or defined(x86_64) or defined(m68k)}
  {$define enable_shl_shr_assign_x_y}
{$endif}
{$if defined(i386) or defined(x86_64)}
  {$define enable_sar_assign_x_y}
{$endif}
{$if defined(i386) or defined(x86_64)}
  {$define enable_rox_assign_x_y}
{$endif}

  interface

    uses
      node;

    procedure do_optloadmodifystore(var rootnode : tnode);

  implementation

    uses
      globtype,verbose,nutils,compinnr,
      defutil,defcmp,htypechk,pass_1,constexp,
      nadd,ncal,ncon,ncnv,ninl,nld,nmat,
      symdef;

    function try_opt_assignmentnode(assignmentnode: tassignmentnode): tnode;
      var
        newinlinenodetype: tinlinenumber;
      begin
        result:=nil;
        with assignmentnode do
          begin
            { replace i:=succ/pred(i) by inc/dec(i)? }
            if (right.nodetype=inlinen) and
              ((tinlinenode(right).inlinenumber=in_succ_x) or (tinlinenode(right).inlinenumber=in_pred_x)) and
              (tinlinenode(right).left.isequal(left)) and
              ((localswitches*[cs_check_overflow,cs_check_range])=[]) and
              ((right.localswitches*[cs_check_overflow,cs_check_range])=[]) and
              valid_for_var(tinlinenode(right).left,false) and
              not(might_have_sideeffects(tinlinenode(right).left)) then
              begin
                if tinlinenode(right).inlinenumber=in_succ_x then
                  newinlinenodetype:=in_inc_x
                else
                  newinlinenodetype:=in_dec_x;
                result:=cinlinenode.createintern(
                  newinlinenodetype,false,ccallparanode.create(
                  tinlinenode(right).left,nil));
                result.localswitches:=localswitches;
                result.fileinfo:=fileinfo;
                result.verbosity:=verbosity;
                tinlinenode(right).left:=nil;
                exit;
              end;
            { replace i:=i+k by inc(i,k)
                      i:=i-k by dec(i,k)
                      i:=i and/or/xor k  by in_[and/or/xor]_assign_x_y(i,k)

              this handles the case, where there are no implicit type conversions }
            if (right.nodetype in [addn,subn,andn,orn,xorn]) and
              (taddnode(right).left.isequal(left)) and
              is_integer(taddnode(right).left.resultdef) and
              is_integer(taddnode(right).right.resultdef) and
              ((localswitches*[cs_check_overflow,cs_check_range])=[]) and
              ((right.localswitches*[cs_check_overflow,cs_check_range])=[]) and
              valid_for_var(taddnode(right).left,false) and
              not(might_have_sideeffects(taddnode(right).left)) then
              begin
                case right.nodetype of
                  addn:
                    newinlinenodetype:=in_inc_x;
                  subn:
                    newinlinenodetype:=in_dec_x;
                  andn:
                    newinlinenodetype:=in_and_assign_x_y;
                  orn:
                    newinlinenodetype:=in_or_assign_x_y;
                  xorn:
                    newinlinenodetype:=in_xor_assign_x_y;
                  else
                    internalerror(2017032901);
                end;
                if right.nodetype in [addn,subn] then
                  result:=cinlinenode.createintern(
                    newinlinenodetype,false,ccallparanode.create(
                    taddnode(right).left,ccallparanode.create(taddnode(right).right,nil)))
                else
                  result:=cinlinenode.createintern(
                    newinlinenodetype,false,ccallparanode.create(
                    taddnode(right).right,ccallparanode.create(taddnode(right).left,nil)));
                result.localswitches:=localswitches;
                result.fileinfo:=fileinfo;
                result.verbosity:=verbosity;
                taddnode(right).left:=nil;
                taddnode(right).right:=nil;
                exit;
              end;
            { replace i:=i+k by inc(i,k)
                      i:=i-k by dec(i,k)
                      i:=i and/or/xor k  by in_[and/or/xor]_assign_x_y(i,k)

              this handles the case with two conversions (outer and inner):
                   outer typeconv: right
               add/sub/and/or/xor: ttypeconvnode(right).left
                   inner typeconv: taddnode(ttypeconvnode(right).left).left
                   right side 'i': ttypeconvnode(taddnode(ttypeconvnode(right).left).left).left
                   right side 'k': taddnode(ttypeconvnode(right).left).right }
            if (right.nodetype=typeconvn) and
               (ttypeconvnode(right).convtype=tc_int_2_int) and
               (ttypeconvnode(right).left.nodetype in [addn,subn,andn,orn,xorn]) and
               is_integer(ttypeconvnode(right).left.resultdef) and
               (right.resultdef.size<=ttypeconvnode(right).left.resultdef.size) and
               (taddnode(ttypeconvnode(right).left).left.nodetype=typeconvn) and
               (ttypeconvnode(taddnode(ttypeconvnode(right).left).left).convtype=tc_int_2_int) and
               are_equal_ints(right.resultdef,ttypeconvnode(taddnode(ttypeconvnode(right).left).left).left.resultdef) and
               ttypeconvnode(taddnode(ttypeconvnode(right).left).left).left.isequal(left) and
               is_integer(taddnode(ttypeconvnode(right).left).left.resultdef) and
               is_integer(taddnode(ttypeconvnode(right).left).right.resultdef) and
               is_integer(ttypeconvnode(taddnode(ttypeconvnode(right).left).left).left.resultdef) and
               ((localswitches*[cs_check_overflow,cs_check_range])=[]) and
               ((right.localswitches*[cs_check_overflow,cs_check_range])=[]) and
               valid_for_var(ttypeconvnode(taddnode(ttypeconvnode(right).left).left).left,false) and
               not(might_have_sideeffects(ttypeconvnode(taddnode(ttypeconvnode(right).left).left).left)) then
              begin
                case ttypeconvnode(right).left.nodetype of
                  addn:
                    newinlinenodetype:=in_inc_x;
                  subn:
                    newinlinenodetype:=in_dec_x;
                  andn:
                    newinlinenodetype:=in_and_assign_x_y;
                  orn:
                    newinlinenodetype:=in_or_assign_x_y;
                  xorn:
                    newinlinenodetype:=in_xor_assign_x_y;
                  else
                    internalerror(2017032903);
                end;
                inserttypeconv_internal(taddnode(ttypeconvnode(right).left).right,left.resultdef);
                if ttypeconvnode(right).left.nodetype in [addn,subn] then
                  result:=cinlinenode.createintern(
                    newinlinenodetype,false,ccallparanode.create(
                    ttypeconvnode(taddnode(ttypeconvnode(right).left).left).left,ccallparanode.create(taddnode(ttypeconvnode(right).left).right,nil)))
                else
                  result:=cinlinenode.createintern(
                    newinlinenodetype,false,ccallparanode.create(
                    taddnode(ttypeconvnode(right).left).right,ccallparanode.create(ttypeconvnode(taddnode(ttypeconvnode(right).left).left).left,nil)));
                result.localswitches:=localswitches;
                result.fileinfo:=fileinfo;
                result.verbosity:=verbosity;
                ttypeconvnode(taddnode(ttypeconvnode(right).left).left).left:=nil;
                taddnode(ttypeconvnode(right).left).right:=nil;
                exit;
              end;
            { replace i:=k+i by inc(i,k)
                      i:=k and/or/xor i  by in_[and/or/xor]_assign_x_y(i,k)

              this handles the case, where there are no implicit type conversions }
            if (right.nodetype in [addn,andn,orn,xorn]) and
              (taddnode(right).right.isequal(left)) and
              is_integer(taddnode(right).left.resultdef) and
              is_integer(taddnode(right).right.resultdef) and
              ((localswitches*[cs_check_overflow,cs_check_range])=[]) and
              ((right.localswitches*[cs_check_overflow,cs_check_range])=[]) and
              valid_for_var(taddnode(right).right,false) and
              not(might_have_sideeffects(taddnode(right).right)) then
              begin
                case right.nodetype of
                  addn:
                    newinlinenodetype:=in_inc_x;
                  andn:
                    newinlinenodetype:=in_and_assign_x_y;
                  orn:
                    newinlinenodetype:=in_or_assign_x_y;
                  xorn:
                    newinlinenodetype:=in_xor_assign_x_y;
                  else
                    internalerror(2017032902);
                end;
                if right.nodetype=addn then
                  result:=cinlinenode.createintern(
                    newinlinenodetype,false,ccallparanode.create(
                    taddnode(right).right,ccallparanode.create(taddnode(right).left,nil)))
                else
                  result:=cinlinenode.createintern(
                    newinlinenodetype,false,ccallparanode.create(
                    taddnode(right).left,ccallparanode.create(taddnode(right).right,nil)));
                result.localswitches:=localswitches;
                result.fileinfo:=fileinfo;
                result.verbosity:=verbosity;
                taddnode(right).right:=nil;
                taddnode(right).left:=nil;
                exit;
              end;
            { replace i:=k+i by inc(i,k)
                      i:=k and/or/xor i  by in_[and/or/xor]_assign_x_y(i,k)

              this handles the case with two conversions (outer and inner):
                   outer typeconv: right
                   add/and/or/xor: ttypeconvnode(right).left
                   inner typeconv: taddnode(ttypeconvnode(right).left).right
                   right side 'i': ttypeconvnode(taddnode(ttypeconvnode(right).left).right).left
                   right side 'k': taddnode(ttypeconvnode(right).left).left }
            if (right.nodetype=typeconvn) and
               (ttypeconvnode(right).convtype=tc_int_2_int) and
               (ttypeconvnode(right).left.nodetype in [addn,andn,orn,xorn]) and
               is_integer(ttypeconvnode(right).left.resultdef) and
               (right.resultdef.size<=ttypeconvnode(right).left.resultdef.size) and
               (taddnode(ttypeconvnode(right).left).right.nodetype=typeconvn) and
               (ttypeconvnode(taddnode(ttypeconvnode(right).left).right).convtype=tc_int_2_int) and
               are_equal_ints(right.resultdef,ttypeconvnode(taddnode(ttypeconvnode(right).left).right).left.resultdef) and
               ttypeconvnode(taddnode(ttypeconvnode(right).left).right).left.isequal(left) and
               is_integer(taddnode(ttypeconvnode(right).left).left.resultdef) and
               is_integer(taddnode(ttypeconvnode(right).left).right.resultdef) and
               is_integer(ttypeconvnode(taddnode(ttypeconvnode(right).left).right).left.resultdef) and
               ((localswitches*[cs_check_overflow,cs_check_range])=[]) and
               ((right.localswitches*[cs_check_overflow,cs_check_range])=[]) and
               valid_for_var(ttypeconvnode(taddnode(ttypeconvnode(right).left).right).left,false) and
               not(might_have_sideeffects(ttypeconvnode(taddnode(ttypeconvnode(right).left).right).left)) then
              begin
                case ttypeconvnode(right).left.nodetype of
                  addn:
                    newinlinenodetype:=in_inc_x;
                  andn:
                    newinlinenodetype:=in_and_assign_x_y;
                  orn:
                    newinlinenodetype:=in_or_assign_x_y;
                  xorn:
                    newinlinenodetype:=in_xor_assign_x_y;
                  else
                    internalerror(2017051101);
                end;
                inserttypeconv_internal(taddnode(ttypeconvnode(right).left).left,left.resultdef);
                if ttypeconvnode(right).left.nodetype=addn then
                  result:=cinlinenode.createintern(
                    newinlinenodetype,false,ccallparanode.create(
                    ttypeconvnode(taddnode(ttypeconvnode(right).left).right).left,ccallparanode.create(taddnode(ttypeconvnode(right).left).left,nil)))
                else
                  result:=cinlinenode.createintern(
                    newinlinenodetype,false,ccallparanode.create(
                    taddnode(ttypeconvnode(right).left).left,ccallparanode.create(ttypeconvnode(taddnode(ttypeconvnode(right).left).right).left,nil)));
                result.localswitches:=localswitches;
                result.fileinfo:=fileinfo;
                result.verbosity:=verbosity;
                ttypeconvnode(taddnode(ttypeconvnode(right).left).right).left:=nil;
                taddnode(ttypeconvnode(right).left).left:=nil;
                exit;
              end;
{$ifdef enable_shl_shr_assign_x_y}
            { replace i:=i shl k by in_shl_assign_x_y(i,k)
                      i:=i shr k by in_shr_assign_x_y(i,k)

              this handles the case, where there are no implicit type conversions }
            if (right.nodetype in [shln,shrn]) and
              (tshlshrnode(right).left.isequal(left)) and
              is_integer(tshlshrnode(right).left.resultdef) and
              is_integer(tshlshrnode(right).right.resultdef) and
{$if not defined(cpu64bitalu) and not defined(cpucg64shiftsupport)}
              not(is_64bitint(tshlshrnode(right).left.resultdef)) and
{$endif}
              ((localswitches*[cs_check_overflow,cs_check_range])=[]) and
              ((right.localswitches*[cs_check_overflow,cs_check_range])=[]) and
              valid_for_var(tshlshrnode(right).left,false) and
              not(might_have_sideeffects(tshlshrnode(right).left)) then
              begin
                case right.nodetype of
                  shln:
                    newinlinenodetype:=in_shl_assign_x_y;
                  shrn:
                    newinlinenodetype:=in_shr_assign_x_y;
                  else
                    internalerror(2017051201);
                end;
                result:=cinlinenode.createintern(
                  newinlinenodetype,false,ccallparanode.create(
                  tshlshrnode(right).right,ccallparanode.create(tshlshrnode(right).left,nil)));
                result.localswitches:=localswitches;
                result.fileinfo:=fileinfo;
                result.verbosity:=verbosity;
                tshlshrnode(right).left:=nil;
                tshlshrnode(right).right:=nil;
                exit;
              end;
            { replace i:=i shl k by in_shl_assign_x_y(i,k)
                      i:=i shr k by in_shr_assign_x_y(i,k)

              this handles the case with two conversions (outer and inner):
                   outer typeconv: right
                          shl/shr: ttypeconvnode(right).left
                   inner typeconv: tshlshrnode(ttypeconvnode(right).left).left
                   right side 'i': ttypeconvnode(tshlshrnode(ttypeconvnode(right).left).left).left
                   right side 'k': tshlshrnode(ttypeconvnode(right).left).right }
            if (right.nodetype=typeconvn) and
               (ttypeconvnode(right).convtype=tc_int_2_int) and
               (ttypeconvnode(right).left.nodetype in [shln,shrn]) and
               is_integer(ttypeconvnode(right).left.resultdef) and
{$if not defined(cpu64bitalu) and not defined(cpucg64shiftsupport)}
               not(is_64bitint(ttypeconvnode(right).left.resultdef)) and
{$endif}
               (right.resultdef.size<=ttypeconvnode(right).left.resultdef.size) and
               (tshlshrnode(ttypeconvnode(right).left).left.nodetype=typeconvn) and
               (ttypeconvnode(tshlshrnode(ttypeconvnode(right).left).left).convtype=tc_int_2_int) and
               are_equal_ints(right.resultdef,ttypeconvnode(tshlshrnode(ttypeconvnode(right).left).left).left.resultdef) and
               ttypeconvnode(tshlshrnode(ttypeconvnode(right).left).left).left.isequal(left) and
               is_integer(tshlshrnode(ttypeconvnode(right).left).left.resultdef) and
               is_integer(tshlshrnode(ttypeconvnode(right).left).right.resultdef) and
               is_integer(ttypeconvnode(tshlshrnode(ttypeconvnode(right).left).left).left.resultdef) and
               ((localswitches*[cs_check_overflow,cs_check_range])=[]) and
               ((right.localswitches*[cs_check_overflow,cs_check_range])=[]) and
               valid_for_var(ttypeconvnode(tshlshrnode(ttypeconvnode(right).left).left).left,false) and
               not(might_have_sideeffects(ttypeconvnode(tshlshrnode(ttypeconvnode(right).left).left).left)) then
              begin
                case ttypeconvnode(right).left.nodetype of
                  shln:
                    newinlinenodetype:=in_shl_assign_x_y;
                  shrn:
                    newinlinenodetype:=in_shr_assign_x_y;
                  else
                    internalerror(2017051202);
                end;
                inserttypeconv_internal(tshlshrnode(ttypeconvnode(right).left).right,left.resultdef);
                result:=cinlinenode.createintern(
                  newinlinenodetype,false,ccallparanode.create(
                  tshlshrnode(ttypeconvnode(right).left).right,ccallparanode.create(ttypeconvnode(tshlshrnode(ttypeconvnode(right).left).left).left,nil)));
                result.localswitches:=localswitches;
                result.fileinfo:=fileinfo;
                result.verbosity:=verbosity;
                ttypeconvnode(tshlshrnode(ttypeconvnode(right).left).left).left:=nil;
                tshlshrnode(ttypeconvnode(right).left).right:=nil;
                exit;
              end;
{$endif enable_shl_shr_assign_x_y}
{$if defined(enable_sar_assign_x_y) or defined(enable_rox_assign_x_y)}
            { replace i:=sar(i) by in_sar_assign_x_y(i,1)
                      i:=rol(i) by in_rol_assign_x_y(i,1)
                      i:=ror(i) by in_ror_assign_x_y(i,1)

              this handles the case, where there are no implicit type conversions }
            if (right.nodetype=inlinen) and
               (tinlinenode(right).inlinenumber in [
{$ifdef enable_sar_assign_x_y}
                   in_sar_x{$ifdef enable_rox_assign_x_y},{$endif}
{$endif enable_sar_assign_x_y}
{$ifdef enable_rox_assign_x_y}
                   in_rol_x,in_ror_x
{$endif enable_rox_assign_x_y}
                 ]) and
               (tinlinenode(right).left.isequal(left)) and
               is_integer(tinlinenode(right).left.resultdef) and
{$if not defined(cpu64bitalu) and not defined(cpucg64shiftsupport)}
               not(is_64bitint(tinlinenode(right).left.resultdef)) and
{$endif}
               ((localswitches*[cs_check_overflow,cs_check_range])=[]) and
               ((right.localswitches*[cs_check_overflow,cs_check_range])=[]) and
               valid_for_var(tinlinenode(right).left,false) and
               not(might_have_sideeffects(tinlinenode(right).left)) then
              begin
                case tinlinenode(right).inlinenumber of
                  in_sar_x:
                    newinlinenodetype:=in_sar_assign_x_y;
                  in_rol_x:
                    newinlinenodetype:=in_rol_assign_x_y;
                  in_ror_x:
                    newinlinenodetype:=in_ror_assign_x_y;
                  else
                    internalerror(2017071701);
                end;
                result:=cinlinenode.createintern(
                  newinlinenodetype,false,ccallparanode.create(
                  cordconstnode.create(1,u8inttype,false),ccallparanode.create(tinlinenode(right).left,nil)));
                result.localswitches:=localswitches;
                result.fileinfo:=fileinfo;
                result.verbosity:=verbosity;
                tinlinenode(right).left:=nil;
                exit;
              end;
            { replace i:=sar(i) by in_sar_assign_x_y(i,1)
                      i:=rol(i) by in_rol_assign_x_y(i,1)
                      i:=ror(i) by in_ror_assign_x_y(i,1)

              this handles the case with type conversions:
                   outer typeconv: right
          sar/rol/ror inline node: ttypeconvnode(right).left
                   inner typeconv: tinlinenode(ttypeconvnode(right).left).left
                   right side 'i': ttypeconvnode(tinlinenode(ttypeconvnode(right).left).left).left }
            if (right.nodetype=typeconvn) and
               (ttypeconvnode(right).convtype=tc_int_2_int) and
               (ttypeconvnode(right).left.nodetype=inlinen) and
               (tinlinenode(ttypeconvnode(right).left).inlinenumber in [
{$ifdef enable_sar_assign_x_y}
                   in_sar_x{$ifdef enable_rox_assign_x_y},{$endif}
{$endif enable_sar_assign_x_y}
{$ifdef enable_rox_assign_x_y}
                   in_rol_x,in_ror_x
{$endif enable_rox_assign_x_y}
                 ]) and
               is_integer(ttypeconvnode(right).left.resultdef) and
               (right.resultdef.size=ttypeconvnode(right).left.resultdef.size) and
               (tinlinenode(ttypeconvnode(right).left).left.nodetype=typeconvn) and
               (ttypeconvnode(tinlinenode(ttypeconvnode(right).left).left).convtype=tc_int_2_int) and
               are_equal_ints(right.resultdef,ttypeconvnode(tinlinenode(ttypeconvnode(right).left).left).left.resultdef) and
               ttypeconvnode(tinlinenode(ttypeconvnode(right).left).left).left.isequal(left) and
               is_integer(ttypeconvnode(tinlinenode(ttypeconvnode(right).left).left).left.resultdef) and
               ((localswitches*[cs_check_overflow,cs_check_range])=[]) and
               ((right.localswitches*[cs_check_overflow,cs_check_range])=[]) and
               valid_for_var(ttypeconvnode(tinlinenode(ttypeconvnode(right).left).left).left,false) and
               not(might_have_sideeffects(ttypeconvnode(tinlinenode(ttypeconvnode(right).left).left).left)) then
              begin
                case tinlinenode(ttypeconvnode(right).left).inlinenumber of
                  in_sar_x:
                    newinlinenodetype:=in_sar_assign_x_y;
                  in_rol_x:
                    newinlinenodetype:=in_rol_assign_x_y;
                  in_ror_x:
                    newinlinenodetype:=in_ror_assign_x_y;
                  else
                    internalerror(2017071801);
                end;
                result:=cinlinenode.createintern(
                  newinlinenodetype,false,ccallparanode.create(
                  cordconstnode.create(1,u8inttype,false),ccallparanode.create(
                  ttypeconvnode(tinlinenode(ttypeconvnode(right).left).left).left,nil)));
                result.localswitches:=localswitches;
                result.fileinfo:=fileinfo;
                result.verbosity:=verbosity;
                ttypeconvnode(tinlinenode(ttypeconvnode(right).left).left).left:=nil;
                exit;
              end;
            { replace i:=sar(i,k) by in_sar_assign_x_y(i,k)
                      i:=rol(i,k) by in_rol_assign_x_y(i,k)
                      i:=ror(i,k) by in_ror_assign_x_y(i,k)

              this handles the case, where there are no implicit type conversions }
            if (right.nodetype=inlinen) and
               (tinlinenode(right).inlinenumber in [
{$ifdef enable_sar_assign_x_y}
                   in_sar_x_y{$ifdef enable_rox_assign_x_y},{$endif}
{$endif enable_sar_assign_x_y}
{$ifdef enable_rox_assign_x_y}
                   in_rol_x_y,in_ror_x_y
{$endif enable_rox_assign_x_y}
                 ]) and
               (tinlinenode(right).left.nodetype=callparan) and
               tcallparanode(tcallparanode(tinlinenode(right).left).right).left.isequal(left) and
               is_integer(tcallparanode(tcallparanode(tinlinenode(right).left).right).left.resultdef) and
{$if not defined(cpu64bitalu) and not defined(cpucg64shiftsupport)}
               not(is_64bitint(tcallparanode(tcallparanode(tinlinenode(right).left).right).left.resultdef)) and
{$endif}
               ((localswitches*[cs_check_overflow,cs_check_range])=[]) and
               ((right.localswitches*[cs_check_overflow,cs_check_range])=[]) and
               valid_for_var(tcallparanode(tcallparanode(tinlinenode(right).left).right).left,false) and
               not(might_have_sideeffects(tcallparanode(tcallparanode(tinlinenode(right).left).right).left)) then
              begin
                case tinlinenode(right).inlinenumber of
                  in_sar_x_y:
                    newinlinenodetype:=in_sar_assign_x_y;
                  in_rol_x_y:
                    newinlinenodetype:=in_rol_assign_x_y;
                  in_ror_x_y:
                    newinlinenodetype:=in_ror_assign_x_y;
                  else
                    internalerror(2017071702);
                end;
                result:=cinlinenode.createintern(
                  newinlinenodetype,false,ccallparanode.create(
                  tcallparanode(tinlinenode(right).left).left,
                  ccallparanode.create(tcallparanode(tcallparanode(tinlinenode(right).left).right).left,nil)));
                result.localswitches:=localswitches;
                result.fileinfo:=fileinfo;
                result.verbosity:=verbosity;
                tcallparanode(tinlinenode(right).left).left:=nil;
                tcallparanode(tcallparanode(tinlinenode(right).left).right).left:=nil;
                exit;
              end;
            { replace i:=sar(i,k) by in_sar_assign_x_y(i,k)
                      i:=rol(i,k) by in_rol_assign_x_y(i,k)
                      i:=ror(i,k) by in_ror_assign_x_y(i,k)

              this handles the case with two conversions (outer and inner):
                   outer typeconv: right
          sar/rol/ror inline node: ttypeconvnode(right).left
                   inner typeconv: tcallparanode(tcallparanode(tinlinenode(ttypeconvnode(right).left).left).right).left
                   right side 'i': ttypeconvnode(tcallparanode(tcallparanode(tinlinenode(ttypeconvnode(right).left).left).right).left).left
                   right side 'k': tcallparanode(tinlinenode(ttypeconvnode(right).left).left).left }
            if (right.nodetype=typeconvn) and
               (ttypeconvnode(right).convtype=tc_int_2_int) and
               (ttypeconvnode(right).left.nodetype=inlinen) and
               (tinlinenode(ttypeconvnode(right).left).inlinenumber in [
{$ifdef enable_sar_assign_x_y}
                   in_sar_x_y{$ifdef enable_rox_assign_x_y},{$endif}
{$endif enable_sar_assign_x_y}
{$ifdef enable_rox_assign_x_y}
                   in_rol_x_y,in_ror_x_y
{$endif enable_rox_assign_x_y}
                 ]) and
               is_integer(ttypeconvnode(right).left.resultdef) and
               (right.resultdef.size=ttypeconvnode(right).left.resultdef.size) and
               (tinlinenode(ttypeconvnode(right).left).left.nodetype=callparan) and
               (tcallparanode(tcallparanode(tinlinenode(ttypeconvnode(right).left).left).right).left.nodetype=typeconvn) and
               (ttypeconvnode(tcallparanode(tcallparanode(tinlinenode(ttypeconvnode(right).left).left).right).left).convtype=tc_int_2_int) and
               are_equal_ints(right.resultdef,ttypeconvnode(tcallparanode(tcallparanode(tinlinenode(ttypeconvnode(right).left).left).right).left).left.resultdef) and
               ttypeconvnode(tcallparanode(tcallparanode(tinlinenode(ttypeconvnode(right).left).left).right).left).left.isequal(left) and
               is_integer(ttypeconvnode(tcallparanode(tcallparanode(tinlinenode(ttypeconvnode(right).left).left).right).left).left.resultdef) and
               ((localswitches*[cs_check_overflow,cs_check_range])=[]) and
               ((right.localswitches*[cs_check_overflow,cs_check_range])=[]) and
               valid_for_var(ttypeconvnode(tcallparanode(tcallparanode(tinlinenode(ttypeconvnode(right).left).left).right).left).left,false) and
               not(might_have_sideeffects(ttypeconvnode(tcallparanode(tcallparanode(tinlinenode(ttypeconvnode(right).left).left).right).left).left)) then
              begin
                case tinlinenode(ttypeconvnode(right).left).inlinenumber of
                  in_sar_x_y:
                    newinlinenodetype:=in_sar_assign_x_y;
                  in_rol_x_y:
                    newinlinenodetype:=in_rol_assign_x_y;
                  in_ror_x_y:
                    newinlinenodetype:=in_ror_assign_x_y;
                  else
                    internalerror(2017072002);
                end;
                result:=cinlinenode.createintern(
                  newinlinenodetype,false,ccallparanode.create(
                  tcallparanode(tinlinenode(ttypeconvnode(right).left).left).left,
                  ccallparanode.create(ttypeconvnode(tcallparanode(tcallparanode(tinlinenode(ttypeconvnode(right).left).left).right).left).left,nil)));
                result.localswitches:=localswitches;
                result.fileinfo:=fileinfo;
                result.verbosity:=verbosity;
                tcallparanode(tinlinenode(ttypeconvnode(right).left).left).left:=nil;
                ttypeconvnode(tcallparanode(tcallparanode(tinlinenode(ttypeconvnode(right).left).left).right).left).left:=nil;
                exit;
              end;
{$endif enable_sar_assign_x_y or enable_rox_assign_x_y}
            { replace i:=not i  by in_not_assign_x(i)
                      i:=-i     by in_neg_assign_x(i)

              this handles the case, where there are no implicit type conversions }
            if (right.nodetype in [notn,unaryminusn]) and
              (tunarynode(right).left.isequal(left)) and
              is_integer(tunarynode(right).left.resultdef) and
              ((localswitches*[cs_check_overflow,cs_check_range])=[]) and
              ((right.localswitches*[cs_check_overflow,cs_check_range])=[]) and
              valid_for_var(tunarynode(right).left,false) and
              not(might_have_sideeffects(tunarynode(right).left)) then
              begin
                if right.nodetype=notn then
                  newinlinenodetype:=in_not_assign_x
                else
                  newinlinenodetype:=in_neg_assign_x;
                result:=cinlinenode.createintern(
                  newinlinenodetype,false,tunarynode(right).left);
                result.localswitches:=localswitches;
                result.fileinfo:=fileinfo;
                result.verbosity:=verbosity;
                tunarynode(right).left:=nil;
                exit;
              end;
            { replace i:=not i  by in_not_assign_x(i)
                      i:=-i     by in_neg_assign_x(i)

              this handles the case with type conversions:
                   outer typeconv: right
                          neg/not: ttypeconvnode(right).left
                   inner typeconv: tunarynode(ttypeconvnode(right).left).left
                   right side 'i': ttypeconvnode(tunarynode(ttypeconvnode(right).left).left).left }
            if (right.nodetype=typeconvn) and
               (ttypeconvnode(right).convtype=tc_int_2_int) and
               (ttypeconvnode(right).left.nodetype in [notn,unaryminusn]) and
               is_integer(ttypeconvnode(right).left.resultdef) and
               (right.resultdef.size<=ttypeconvnode(right).left.resultdef.size) and
               (tunarynode(ttypeconvnode(right).left).left.nodetype=typeconvn) and
               (ttypeconvnode(tunarynode(ttypeconvnode(right).left).left).convtype=tc_int_2_int) and
               are_equal_ints(right.resultdef,ttypeconvnode(tunarynode(ttypeconvnode(right).left).left).left.resultdef) and
               ttypeconvnode(tunarynode(ttypeconvnode(right).left).left).left.isequal(left) and
               is_integer(ttypeconvnode(tunarynode(ttypeconvnode(right).left).left).left.resultdef) and
               ((localswitches*[cs_check_overflow,cs_check_range])=[]) and
               ((right.localswitches*[cs_check_overflow,cs_check_range])=[]) and
               valid_for_var(ttypeconvnode(tunarynode(ttypeconvnode(right).left).left).left,false) and
               not(might_have_sideeffects(ttypeconvnode(tunarynode(ttypeconvnode(right).left).left).left)) then
              begin
                if ttypeconvnode(right).left.nodetype=notn then
                  newinlinenodetype:=in_not_assign_x
                else
                  newinlinenodetype:=in_neg_assign_x;
                result:=cinlinenode.createintern(
                  newinlinenodetype,false,ttypeconvnode(tunarynode(ttypeconvnode(right).left).left).left);
                result.localswitches:=localswitches;
                result.fileinfo:=fileinfo;
                result.verbosity:=verbosity;
                ttypeconvnode(tunarynode(ttypeconvnode(right).left).left).left:=nil;
                exit;
              end;
          end;
      end;

    function try_opt_node(var n: tnode; arg: pointer): foreachnoderesult;
      var
        hn : tnode;
      begin
        result:=fen_false;
        if n.nodetype=assignn then
          begin
            hn:=try_opt_assignmentnode(tassignmentnode(n));
            if assigned(hn) then
              begin
                n.free;
                n:=hn;
                typecheckpass(n);
                do_firstpass(n);
              end;
          end;
      end;


    procedure do_optloadmodifystore(var rootnode : tnode);
      begin
        foreachnodestatic(pm_postprocess,rootnode,@try_opt_node,nil);
      end;

end.

