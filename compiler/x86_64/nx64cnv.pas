{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate x86-64 assembler for type converting nodes

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
unit nx64cnv;

{$i fpcdefs.inc}

interface

    uses
      node,ncgcnv,defutil,defcmp,
      nx86cnv;

    type
       tx86_64typeconvnode = class(tx86typeconvnode)
         protected
         { procedure second_int_to_int;override; }
         { procedure second_string_to_string;override; }
         { procedure second_cstring_to_pchar;override; }
         { procedure second_string_to_chararray;override; }
         { procedure second_array_to_pointer;override; }
         { procedure second_pointer_to_array;override; }
         { procedure second_chararray_to_string;override; }
         { procedure second_char_to_string;override; }
         { function first_int_to_real: tnode; override; }
           procedure second_int_to_real;override;
         { procedure second_real_to_real;override; }
         { procedure second_cord_to_pointer;override; }
         { procedure second_proc_to_procvar;override; }
         { procedure second_bool_to_int;override; }
         { procedure second_int_to_bool;override; }
         { procedure second_load_smallset;override;  }
         { procedure second_ansistring_to_pchar;override; }
         { procedure second_pchar_to_string;override; }
         { procedure second_class_to_intf;override;  }
         { procedure second_char_to_char;override; }
           procedure second_call_helper(c : tconverttype);override;
       end;


implementation

   uses
      verbose,systems,globtype,
      symconst,symdef,aasmbase,aasmtai,aasmcpu,
      cginfo,cgbase,pass_2,
      ncon,ncal,ncnv,
      cpubase,
      cgobj,cga,tgobj,rgobj,rgcpu,ncgutil;

    procedure tx86_64typeconvnode.second_int_to_real;
      begin
         internalerror(200304305);
      end;


    procedure tx86_64typeconvnode.second_call_helper(c : tconverttype);
{$ifdef fpc}
      const
         secondconvert : array[tconverttype] of pointer = (
           @second_nothing, {equal}
           @second_nothing, {not_possible}
           @second_nothing, {second_string_to_string, handled in resulttype pass }
           @second_char_to_string,
           @second_nothing, {char_to_charray}
           @second_nothing, { pchar_to_string, handled in resulttype pass }
           @second_nothing, {cchar_to_pchar}
           @second_cstring_to_pchar,
           @second_ansistring_to_pchar,
           @second_string_to_chararray,
           @second_nothing, { chararray_to_string, handled in resulttype pass }
           @second_array_to_pointer,
           @second_pointer_to_array,
           @second_int_to_int,
           @second_int_to_bool,
           @second_bool_to_bool,
           @second_bool_to_int,
           @second_real_to_real,
           @second_int_to_real,
           @second_nothing, { real_to_currency, handled in resulttype pass }
           @second_proc_to_procvar,
           @second_nothing, { arrayconstructor_to_set }
           @second_nothing, { second_load_smallset, handled in first pass }
           @second_cord_to_pointer,
           @second_nothing, { interface 2 string }
           @second_nothing, { interface 2 guid   }
           @second_class_to_intf,
           @second_char_to_char,
           @second_nothing,  { normal_2_smallset }
           @second_nothing,  { dynarray_2_openarray }
           @second_nothing,  { pwchar_2_string }
           @second_nothing,  { variant_2_dynarray }
           @second_nothing   { dynarray_2_variant}
         );
      type
         tprocedureofobject = procedure of object;

      var
         r : packed record
                proc : pointer;
                obj : pointer;
             end;

      begin
         { this is a little bit dirty but it works }
         { and should be quite portable too        }
         r.proc:=secondconvert[c];
         r.obj:=self;
         tprocedureofobject(r)();
      end;
{$else fpc}
     begin
        case c of
          tc_equal,
          tc_not_possible,
          tc_string_2_string : second_nothing;
          tc_char_2_string : second_char_to_string;
          tc_char_2_chararray : second_nothing;
          tc_pchar_2_string : second_nothing;
          tc_cchar_2_pchar : second_nothing;
          tc_cstring_2_pchar : second_cstring_to_pchar;
          tc_ansistring_2_pchar : second_ansistring_to_pchar;
          tc_string_2_chararray : second_string_to_chararray;
          tc_chararray_2_string : second_nothing;
          tc_array_2_pointer : second_array_to_pointer;
          tc_pointer_2_array : second_pointer_to_array;
          tc_int_2_int : second_int_to_int;
          tc_int_2_bool : second_int_to_bool;
          tc_bool_2_bool : second_bool_to_bool;
          tc_bool_2_int : second_bool_to_int;
          tc_real_2_real : second_real_to_real;
          tc_int_2_real : second_int_to_real;
          tc_real_2_currency : second_nothing;
          tc_proc_2_procvar : second_proc_to_procvar;
          tc_arrayconstructor_2_set : second_nothing;
          tc_load_smallset : second_nothing;
          tc_cord_2_pointer : second_cord_to_pointer;
          tc_intf_2_string : second_nothing;
          tc_intf_2_guid : second_nothing;
          tc_class_2_intf : second_class_to_intf;
          tc_char_2_char : second_char_to_char;
          tc_normal_2_smallset : second_nothing;
          tc_dynarray_2_openarray : second_nothing;
          tc_pwchar_2_string : second_nothing;
          tc_variant_2_dynarray : second_nothing;
          tc_dynarray_2_variant : second_nothing;
          else internalerror(2002101101);
        end;
     end;
{$endif fpc}

begin
   ctypeconvnode:=tx86_64typeconvnode;
end.
{
  $Log$
  Revision 1.1  2003-04-30 20:53:32  florian
    * error when address of an abstract method is taken
    * fixed some x86-64 problems
    * merged some more x86-64 and i386 code
}
