{
    $Id$
    This file is part of the Free Pascal run time library and compiler.
    Copyright (c) 1993,98 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

const
   in_lo_word = 1;
   in_hi_word = 2;
   in_lo_long = 3;
   in_hi_long = 4;
   in_ord_x = 5;
   in_length_string = 6;
   in_chr_byte = 7;
{$ifdef VER0_99_5}   
   in_inc_byte = 8;
   in_inc_word = 9;
   in_inc_dword = 10;
   in_dec_byte = 11;
   in_dec_word = 12;
   in_dec_dword = 13;
{$endif}   
   in_write_x = 14;
   in_writeln_x = 15;
   in_read_x = 16;
   in_readln_x = 17;
   in_concat_x = 18;
   in_assigned_x = 19;
   in_str_x_string = 20;
   in_ofs_x = 21;
   in_sizeof_x = 22;
   in_typeof_x = 23;
   in_val_x = 24;
   in_reset_x = 25;
   in_rewrite_x = 26;
   in_low_x = 27;
   in_high_x = 28;
   in_seg_x = 29;
   in_pred_x = 30;
   in_succ_x = 31;
   in_reset_typedfile = 32;
   in_rewrite_typedfile = 33;
   in_settextbuf_file_x = 34;
   in_inc_x = 35;
   in_dec_x = 36;
   in_include_x_y = 37;
   in_exclude_x_y = 38;
   in_break = 39;
   in_continue = 40;
   in_assert_x = 41;

{
  $Log$
  Revision 1.6  1998-08-20 12:59:56  peter
    - removed obsolete in_*

}

