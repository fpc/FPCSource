{ Copyright (C) 1995 DJ Delorie, see COPYING.DJ for details

 ( MvdV: GPL/LGPL, I can't exactly make out from that file what this is, and
   it doesn't matter for me, since FPC is also GPL/LGPL)

   FPC Pascal conversion by Marco van de Voort (C) 2001.
}

Unit coff;


interface

{$packrecords C}

type cushort =word;
     culong  =cardinal;
     cshort  =integer;
     cuchar  =byte;

{** coff information for Intel 386/486.  }

{********************* FILE HEADER *********************}

TYPE External_FileHDR  = Record
                        f_magic  : cushort;  { magic number                     }
                        f_nscns  : cushort;  { number of sections               }
                        f_timdat : culong;  { time & date stamp         }
                        f_symptr : culong;  { file pointer to symtab    }
                        f_nsyms  : culong;  { number of symtab entries  }
                        f_opthdr : cushort;  { sizeof(optional hdr)             }
                        f_flags  : cushort;  { flags                    }
                        end;


{ Bits for f_flags:
 *      F_RELFLG        relocation info stripped from file
 *      F_EXEC          file is executable (no unresolved external references)
 *      F_LNNO          line numbers stripped from file
 *      F_LSYMS         local symbols stripped from file
 *      F_AR32WR        file has byte ordering of an AR32WR machine (e.g. vax)
 }

        FILHDR   = external_filehdr;


Const
        F_RELFLG        =00001;
        F_EXEC          =00002;
        F_LNNO          =00004;
        F_LSYMS         =00008;


        I386MAGIC       =$14c;
        I386AIXMAGIC    =$175;
//        I386BADMAG(x) (((x).f_magic!=I386MAGIC) && (x).f_magic!=I386AIXMAGIC)


        FILHSZ  = sizeof(FILHDR);


{********************* AOUT "OPTIONAL HEADER" *********************}


TYPE AOUTHDR = Record

                magic      : cushort;           { type of file                          }
                vstamp     : cushort;           { version stamp                 }
                tsize      : culong;            { text size in bytes, padded to FW bdry}
                dsize      : culong;            { initialized data "  "         }
                bsize      : culong;            { uninitialized data "   "              }
                entry      : culong;            { entry pt.                             }
                text_start : culong;    { base of text used for this file }
                data_start : culong;    { base of data used for this file }
               end;

   gnu_aout = Record
                info,
                tsize,
                dsize,
                bsize,
                symsize,
                entry,
                txrel,
                dtrel     : culong;
        end;

const
    AOUTSZ =sizeof(AOUTHDR);

        OMAGIC          =0404;    { object files, eg as output }
        ZMAGIC          =0413;    { demand load format, eg normal ld output }
        STMAGIC         =0401;  { target shlib }
        SHMAGIC         =0443;  { host   shlib }


{********************* SECTION HEADER *********************}


type  external_scnhdr = Record
                        s_name    : array[0..7] OF CHAR;        { section name                  }
                        s_paddr,                { physical address, aliased s_nlib }
                        s_vaddr,                { virtual address               }
                        s_size,                 { section size                  }
                        s_scnptr,               { file ptr to raw data for section }
                        s_relptr,               { file ptr to relocation        }
                        s_lnnoptr : culong;     { file ptr to line numbers      }
                        s_nreloc,               { number of relocation entries  }
                        s_nlnno   : cushort;    { number of line number entries}
                        s_flags   : culong;     { flags                 }
                                end;
        SCNHDR = external_scnhdr;

const SCNHSZ=sizeof(SCNHDR);


{
 * names of "special" sections
 }

CONST
        special_TEXT    ='.text';
        special_DATA    ='.data';
        special_BSS     ='.bss';
        special_COMMENT ='.comment';
        special_LIB     ='.lib';

{
 * s_flags "type"
 }
        STYP_TEXT        =$20;  { section contains text only }
        STYP_DATA        =$40;  { section contains data only }
        STYP_BSS         =$80;  { section contains bss only }

{********************* LINE NUMBERS *********************}

{ 1 line number entry for every "breakpointable" source line in a section.
 * Line numbers are grouped on a per function basis; first entry in a function
 * grouping will have l_lnno = 0 and in place of physical address will be the
 * symbol table index of the function name.
 }

type aux_lineno = Packed Record
                 case boolean of
                   false:( l_symndx:culong); { function name symbol index, iff l_lnno == 0 }
                   true:(  l_paddr :culong); { (physical) address of line number }
                end;

     external_lineno  = packed record                   {should be 6 bytes, check!}
                 l_addr : aux_lineno;
                 l_lnno : cushort;              { line number }
                 end;

     lineno=external_lineno;


const   LINESZ  =sizeof(LINENO);


{********************* SYMBOLS *********************}

        E_SYMNMLEN      =8;     { # characters in a symbol name }
        E_FILNMLEN      =14;    { # characters in a file name           }
        E_DIMNUM        =4;     { # array dimensions in auxiliary entry }



type aux_aux_external_syment = packed record
                                 e_zeroes : culong;
                                 e_offset : culong;
                                end;
     aux_external_syment = packed record
                             case boolean of
                                false: (e_name : array[0..E_SYMNMLEN-1] OF Char);
                                true : (e:aux_aux_external_syment);
                             end;

     external_syment     = packed record                {packed, check if necessary!}
                             e          : aux_external_syment;
                             e_value    : culong;
                             e_scnum    : cshort;
                             e_type     : cushort;
                             e_sclass   : cuchar;
                             e_numaux   : cuchar;
                            end;


CONST
        N_BTMASK        =$F;
        N_TMASK         =$30;
        N_BTSHFT        =$4;
        N_TSHIFT        =$2;


type tx_lnsz        = packed record
                                 x_lnno,
                                 x_size   :  cushort;
                                end;

     tx_misc        = packed record
                                case boolean of
                                 false    : (x_lnsz  : tx_lnsz);
                                 true     : (x_fsize : culong);
                                end;

     tx_fcn         = packed record
                                 x_lnnoptr,
                                 x_endndx   : culong;
                                end;

     tx_ary         = packed record
                                 x_dimen  : array[0..E_DIMNUM-1] OF cushort;
                                end;

     tx_fcnary      = packed record
                                case boolean of
                                 false : ( x_fcn : tx_fcn);
                                 true  : ( x_ary : tx_ary);
                                end;



     tx_sym         = packed record
                               x_tagndx  : culong;
                               x_misc    : tx_misc;
                               x_fcnary  : tx_fcnary;
                               x_tvndx   : cushort;
                              end;

     tx_n           = packed record
                                x_zeroes,
                                x_offset : culong;
                                end;
     tx_file        = packed record
                              case boolean of
                                false: ( x_fname : array [0..E_FILNMLEN-1] of char);
                                true : (x_n : tx_n);
                               end;
     tx_scn         = packed record
                               x_scnlen : culong;
                               x_nreloc,
                               x_nlinno : cushort;
                              end;
     tx_tv          = packed record
                               x_tvfill  : culong;
                               x_tvlen   : cushort;
                               x_tvran   : array[0..1] of cushort;
                              end;

     external_auxent = packed record
                          case byte of
                           0: (x_sym : tx_sym);
                           1: (x_file: tx_file);
                           2: (x_scn : tx_scn);
                           3: (x_tv  : tx_tv);
                          end;
   SYMENT= external_syment;
   AUXENT= external_auxent;

const
   SYMESZ = SIZEOF(SYMENT);
   AUXESZ = SIZEOF(AUXENT);

{       define _ETEXT   "etext"}

{ Relocatable symbols have number of the section in which they are defined,
   or one of the following: }

        N_UNDEF         = 0;    { undefined symbol }
        N_ABS           =-1;    { value of symbol is absolute }
        N_DEBUG         =-2;    { debugging symbol -- value is meaningless }
        N_TV            =-3;    { indicates symbol needs preload transfer vector }
        P_TV            =-4;    { indicates symbol needs postload transfer vector}

{
 * Type of a symbol, in low N bits of the word
 }
        T_NULL          =0;
        T_VOID          =1;     { function argument (only used by compiler) }
        T_CHAR          =2;     { character             }
        T_SHORT         =3;     { short integer }
        T_INT           =4;     { integer               }
        T_LONG          =5;     { long integer          }
        T_FLOAT         =6;     { floating point        }
        T_DOUBLE        =7;     { double word           }
        T_STRUCT        =8;     { structure             }
        T_UNION         =9;     { union                 }
        T_ENUM          =10;    { enumeration           }
        T_MOE           =11;    { member of enumeration}
        T_UCHAR         =12;    { unsigned character    }
        T_USHORT        =13;    { unsigned short        }
        T_UINT          =14;    { unsigned integer      }
        T_ULONG         =15;    { unsigned long }
        T_LNGDBL        =16;    { long double           }

{
 * derived types, in n_type
}
        DT_NON          =0;     { no derived type }
        DT_PTR          =1;     { pointer }
        DT_FCN          =2;     { function }
        DT_ARY          =3;     { array }

{
\\        BTYPE(x)      ((x) & N_BTMASK)

\\        ISPTR(x)      (((x) & N_TMASK) == (DT_PTR << N_BTSHFT))
\\        ISFCN(x)      (((x) & N_TMASK) == (DT_FCN << N_BTSHFT))
\\        ISARY(x)      (((x) & N_TMASK) == (DT_ARY << N_BTSHFT))
\\        ISTAG(x)      ((x)==C_STRTAG||(x)==C_UNTAG||(x)==C_ENTAG)
\\        DECREF(x) ((((x)>>N_TSHIFT)&~N_BTMASK)|((x)&N_BTMASK))
}
{********************* STORAGE CLASSES *********************}

{ This used to be defined as -1, but now n_sclass is unsigned.  }
        C_EFCN          =$ff;   { physical end of function      }
        C_NULL          =0;
        C_AUTO          =1;     { automatic variable            }
        C_EXT           =2;     { external symbol               }
        C_STAT          =3;     { static                        }
        C_REG           =4;     { register variable             }
        C_EXTDEF        =5;     { external definition           }
        C_LABEL         =6;     { label                 }
        C_ULABEL        =7;     { undefined label               }
        C_MOS           =8;     { member of structure           }
        C_ARG           =9;     { function argument             }
        C_STRTAG        =10;    { structure tag         }
        C_MOU           =11;    { member of union               }
        C_UNTAG         =12;    { union tag                     }
        C_TPDEF         =13;    { type definition               }
        C_USTATIC       =14;    { undefined static              }
        C_ENTAG         =15;    { enumeration tag               }
        C_MOE           =16;    { member of enumeration }
        C_REGPARM       =17;    { register parameter            }
        C_FIELD         =18;    { bit field                     }
        C_AUTOARG       =19;    { auto argument         }
        C_LASTENT       =20;    { dummy entry (end of block)    }
        C_BLOCK         =100;   { ".bb" or ".eb"                }
        C_FCN           =101;   { ".bf" or ".ef"                }
        C_EOS           =102;   { end of structure              }
        C_FILE          =103;   { file name                     }
        C_LINE          =104;   { line # reformatted as symbol table entry }
        C_ALIAS         =105;   { duplicate tag         }
        C_HIDDEN        =106;   { ext symbol in dmert public lib }

{********************* RELOCATION DIRECTIVES *********************}


type external_reloc = packed record
                         r_vaddr,
                         r_symndx : culong;
                         r_type   : cushort;
                        end;



        RELOC = external_reloc;
        PRELOC= ^RELOC;                 {not in original header}
const
        RELSZ =sizeof(RELOC);

        RELOC_REL32     =20;    { 32-bit PC-relative address }
        RELOC_ADDR32    =6;     { 32-bit absolute address }

        DEFAULT_DATA_SECTION_ALIGNMENT =4;
        DEFAULT_BSS_SECTION_ALIGNMENT =4;
        DEFAULT_TEXT_SECTION_ALIGNMENT =4;

{ For new sections we havn't heard of before }

        DEFAULT_SECTION_ALIGNMENT =4;

Implementation

end.
