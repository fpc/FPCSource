{
    $Id$
    This file is part of the Free Pascal run time library and compiler.
    Copyright (c) 1998-2002 by the Free Pascal development team

    Internal Function/Constant Evaluator numbers

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

const
{ Internal functions }
   in_lo_word           = 1;
   in_hi_word           = 2;
   in_lo_long           = 3;
   in_hi_long           = 4;
   in_ord_x             = 5;
   in_length_x          = 6;
   in_chr_byte          = 7;
   in_write_x           = 14;
   in_writeln_x         = 15;
   in_read_x            = 16;
   in_readln_x          = 17;
   in_concat_x          = 18;
   in_assigned_x        = 19;
   in_str_x_string      = 20;
   in_ofs_x             = 21;
   in_sizeof_x          = 22;
   in_typeof_x          = 23;
   in_val_x             = 24;
   in_reset_x           = 25;
   in_rewrite_x         = 26;
   in_low_x             = 27;
   in_high_x            = 28;
   in_seg_x             = 29;
   in_pred_x            = 30;
   in_succ_x            = 31;
   in_reset_typedfile   = 32;
   in_rewrite_typedfile = 33;
   in_settextbuf_file_x = 34;
   in_inc_x             = 35;
   in_dec_x             = 36;
   in_include_x_y       = 37;
   in_exclude_x_y       = 38;
   in_break             = 39;
   in_continue          = 40;
   in_assert_x_y        = 41;
   in_addr_x            = 42;
   in_typeinfo_x        = 43;
   in_setlength_x       = 44;
   in_finalize_x        = 45;
   in_new_x             = 46;
   in_dispose_x         = 47;

{ Internal constant functions }
   in_const_trunc      = 100;
   in_const_round      = 101;
   in_const_frac       = 102;
   in_const_abs        = 103;
   in_const_int        = 104;
   in_const_sqr        = 105;
   in_const_odd        = 106;
   in_const_ptr        = 107;
   in_const_swap_word  = 108;
   in_const_swap_long  = 109;
   in_const_pi         = 110;
   in_const_sqrt       = 111;
   in_const_arctan     = 112;
   in_const_cos        = 113;
   in_const_exp        = 114;
   in_const_ln         = 115;
   in_const_sin        = 116;
   in_lo_qword         = 117;
   in_hi_qword         = 118;
   in_cos_extended     = 119;
   in_pi               = 121;
   in_abs_extended     = 122;
   in_sqr_extended     = 123;
   in_sqrt_extended    = 124;
   in_arctan_extended  = 125;
   in_ln_extended      = 126;
   in_sin_extended     = 127;

{ MMX functions }
{ these contants are used by the mmx unit }

   { MMX }
   in_mmx_pcmpeqb      = 200;
   in_mmx_pcmpeqw      = 201;
   in_mmx_pcmpeqd      = 202;
   in_mmx_pcmpgtb      = 203;
   in_mmx_pcmpgtw      = 204;
   in_mmx_pcmpgtd      = 205;

   { 3DNow }

   { SSE }

{
  $Log$
  Revision 1.5  2002-05-14 19:34:40  peter
    * removed old logs and updated copyright year

}
