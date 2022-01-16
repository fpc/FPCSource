{
    Copyright (c) 2019 by Jonas Maebe, member of the
    Free Pascal Compiler development team

    Dwarf Call Frame Information directives

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
unit aasmcfi;

{$i fpcdefs.inc}

  interface

    uses
      globtype,
      cgbase,
      aasmtai;

    type
      tcfikind =
        (cfi_startproc,
         cfi_endproc,
         cfi_personality,
         cfi_personality_id,
         cfi_fde_data,
         cfi_lsda_encoding,
         cfi_inline_lsda,
         cfi_def_cfa,
         cfi_def_cfa_register,
         cfi_def_cfa_offset,
         cfi_adjust_cfa_offset,
         cfi_offset,
         cfi_val_offset,
         cfi_rel_offset,
         cfi_register,
         cfi_restore,
         cfi_undefined,
         cfi_same_value,
         cfi_remember_state,
         cfi_restore_state,
         cfi_return_column,
         cfi_signal_frame,
         cfi_window_save,
         cfi_escape,
         cfi_val_encoded_addr
        );

{$push}
{$j-}
      const
        cfi2str: array[tcfikind] of string[length('.cfi_adjust_cfa_offset')] =
          ('.cfi_startproc',
           '.cfi_endproc',
           '.cfi_personality',
           '.cfi_personality_id',
           '.cfi_fde_data',
           '.cfi_lsda_encoding',
           '.cfi_inline_lsda',
           '.cfi_def_cfa',
           '.cfi_def_cfa_register',
           '.cfi_def_cfa_offset',
           '.cfi_adjust_cfa_offset',
           '.cfi_offset',
           '.cfi_val_offset',
           '.cfi_rel_offset',
           '.cfi_register',
           '.cfi_restore',
           '.cfi_undefined',
           '.cfi_same_value',
           '.cfi_remember_state',
           '.cfi_restore_state',
           '.cfi_return_column',
           '.cfi_signal_frame',
           '.cfi_window_save',
           '.cfi_escape',
           '.cfi_val_encoded_addr'
          );
{$pop}

    type
      tai_cfi_base = class abstract(tai)
        cfityp: tcfikind;
        constructor create(ctyp: tcfikind);
      end;

      tai_cfi_op_none = class(tai_cfi_base)
      end;

      tai_cfi_op_val = class(tai_cfi_base)
        val1: aint;
        constructor create(ctyp: tcfikind; const a: aint);
      end;

      tai_cfi_op_string = class(tai_cfi_base)
        s1: TSymStr;
        constructor create(ctyp: tcfikind; const str1: TSymStr);
      end;

      tai_cfi_op_val_string = class(tai_cfi_op_val)
        s: TSymStr;
        constructor create(ctyp: tcfikind; const a: aint; const str: TSymStr);
      end;

      tai_cfi_op_string_string = class(tai_cfi_op_string)
        s2: TSymStr;
        constructor create(ctyp: tcfikind; const str1, str2: TSymStr);
      end;

      tai_cfi_op_reg = class(tai_cfi_base)
        reg1: tregister;
        constructor create(ctyp: tcfikind; r: tregister);
      end;

      tai_cfi_op_reg_val = class(tai_cfi_op_reg)
        val: aint;
        constructor create(ctyp: tcfikind; r: tregister; a: aint);
      end;

      tai_cfi_op_reg_reg = class(tai_cfi_op_reg)
        reg2: tregister;
        constructor create(ctyp: tcfikind; r1, r2: tregister);
      end;


  implementation

    constructor tai_cfi_base.create(ctyp: tcfikind);
      begin
        typ:=ait_cfi;
        cfityp:=ctyp;
      end;


    constructor tai_cfi_op_val.create(ctyp: tcfikind; const a: aint);
      begin
        inherited create(ctyp);
        val1:=a;
      end;


    constructor tai_cfi_op_string.create(ctyp: tcfikind; const str1: TSymStr);
      begin
        inherited create(ctyp);
        s1:=str1;
      end;


    constructor tai_cfi_op_val_string.create(ctyp: tcfikind; const a: aint; const str: TSymStr);
      begin
        inherited create(ctyp,a);
        s:=str;
      end;


    constructor tai_cfi_op_string_string.create(ctyp: tcfikind; const str1, str2: TSymStr);
      begin
        inherited create(ctyp,str1);
        s2:=str2;
      end;


    constructor tai_cfi_op_reg.create(ctyp: tcfikind; r: tregister);
      begin
        inherited create(ctyp);
        reg1:=r;
      end;


    constructor tai_cfi_op_reg_val.create(ctyp: tcfikind; r: tregister; a: aint);
      begin
        inherited create(ctyp,r);
        val:=a;
      end;


    constructor tai_cfi_op_reg_reg.create(ctyp: tcfikind; r1, r2: tregister);
      begin
        inherited create(ctyp,r1);
        reg2:=r2;
      end;

end.

