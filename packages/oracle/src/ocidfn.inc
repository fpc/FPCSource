
{
  Automatically converted by H2Pas 1.0.0 from ocidfn.h
  The following command line parameters were used:
    -p
    -D
    -l
    jojo.so
    ocidfn.h
}

{$PACKRECORDS C}

  {
   * $Header: ocidfn.h 25-nov-2002.11:03:03 srseshad Exp $ 
    }
  { Copyright (c) 1991, 2002, Oracle Corporation.  All rights reserved.   }
  { Copyright (c) 1991, 2002, Oracle Corporation.  All rights reserved.   }
  {
     NAME
       ocidfn.h - OCI Definations
     NOTES
       Shipped to users.
     MODIFIED   (MM/DD/YY)
      srseshad   11/25/02 - change binary float/double codes
      srseshad   11/14/02 - Add SQLT_IBFLOAT, SQLT_IBDOUBLE
      mxyang     09/17/02 - grabtrans 'mmorsi_obj_float'
      srseshad   09/06/02 - Add binary float/double
      aahluwal   06/04/02 - bug 2360115
      kmuthukk   05/02/00 - add SQLT_PNTY
      amangal    07/30/99 - Merge into 8.1.6 : Bug 879031
      tnbui      07/28/99 - Remove SQLT_TIMESTAMP_ITZ                            
      tnbui      07/21/99 - SQLT_TIMESTAMP_LTZ                                   
      tnbui      06/16/99 - TIMESTAMP WITH IMPLICIT TIME ZONE                    
      whe        04/07/99 - bug#810075
      whe        03/19/99 - lrg 32079 etc.: move HDA def from ocidem.h to ocidfn.
      skmishra   05/10/98 -
      vyanaman   04/16/98 - update sql92 datatypes
      khnguyen   01/16/98 -
      khnguyen   12/23/97 - SQLT* for datetimes and intervals
      tanguyen   08/19/97 -
      dchatter   03/18/97 -  porting exception 390897
      dchatter   05/02/97 -   merge porting exception
      dalpern    12/04/96 -   SQLCS_LIT_NULL added
      cxcheng    11/14/96 -   add SQLT_BFILE/SQLT_CFILE to fix compile prob
      cxcheng    11/12/96 -   add SQLT_NCO for named collection
      lchidamb   10/17/96 -   add SQLT_VST and SQLT_ODT
      sgollapu   10/14/96 -   Mutual exclusion of ocidfn and sqldef
      sgollapu   10/07/96 -   OCI Simplification
      aroy       09/09/96 -   add SQLCS* definitions
      slari      08/07/96 -  add SQLT_RDD, rowid descriptor
      slari      06/12/96 -  remove SQLT_TTBL
      dchatter   04/21/96 -  prepare for merge into main
      slari      08/24/95 -  b299432, define CDA_SIZE
      zwalcott   02/28/96 -  add SQLT_BFILEE and SQLT_CFILEE.
      lchidamb   02/22/96 -  make dtys consistent with dtydef.h
      lchidamb   02/16/96 -  add SQLT_BFILEE and SQLT_CFILEE
      lchidamb   01/30/96 -  rename new datatypes for v8
      lchidamb   09/06/95 -  add new datatypes
      slari      05/11/95 -  add OCI_EV_DEF and OCI_EV_TSF
      dchatter   04/06/95 -  add ifdef flags around OCI_flags
      dchatter   03/08/95 -  piece values
      dchatter   03/06/95 -  merge changes from branch 1.2.720.3
      jfbrown    02/17/95 -  merge changes from branch 1.2.720.2
      dchatter   02/08/95 -  olog call modes
      jfbrown    02/03/95 -  remove non-printable characters
      lchidamb   12/06/94 -  merge changes from branch 1.2.720.1
      lchidamb   10/04/94 -  added field chk to cda_head, cda_def
      dchatter   07/05/94 -  SQLT_CUR added
      rkooi2     11/27/92 -  Changing e* datatypes to s* 
      rkooi2     10/26/92 -  More portability mods 
      rkooi2     10/22/92 -  Added #ifndef ORATYPES ... 
      rkooi2     10/18/92 -  Changes to make it portable. 
      sjain      03/16/92 -  Creation 
   }
  {
   *  ocidfn.h
   *
   *  Common header file for OCI C sample programs.
   *  This header declares the cursor and logon data area structure.
   *  The types used are defined in <oratypes.h>.
   *
    }
  { The cda_head struct is strictly PRIVATE.  It is used
     internally only. Do not use this struct in OCI programs.  }
  type
     Pdvoid  = pointer;
     Ppdvoid  = ^Pdvoid;
     PPpdvoid  = ^PPdvoid;

  type
     Pcda_head = ^cda_head;
     cda_head = record
          v2_rc : sb2;
          ft : ub2;
          rpc : ub4;
          peo : ub2;
          fc : ub1;
          rcs1 : ub1;
          rc : ub2;
          wrn : ub1;
          rcs2 : ub1;
          rcs3 : sword;
          rid : record
               rd : record
                    rcs4 : ub4;
                    rcs5 : ub2;
                    rcs6 : ub1;
                 end;
               rcs7 : ub4;
               rcs8 : ub2;
            end;
          ose : sword;
          chk : ub1;
          rcsp : Pdvoid;
       end;

  {
  ** Size of HDA area:
  ** 512 for 64 bit arquitectures
  ** 256 for 32 bit arquitectures
   }
{ $if defined(SS_64BIT_SERVER) || defined(__64BIT__)}

//  const
//     HDA_SIZE = 512;
{ $else}

  const
     HDA_SIZE = 256;
{ $endif}
{ $if defined(SS_64BIT_SERVER) || defined(__64BIT__)}

//  const
//     CDA_SIZE = 88;
{ $else}

  const
     CDA_SIZE = 64;     
{ $endif}
  { the real CDA, padded to CDA_SIZE bytes in size  }
  { V2 return code  }
  { SQL function type  }
  { rows processed count  }
  { parse error offset  }
  { OCI function code  }
  { filler area  }
  { V7 return code  }
  { warning flags  }
  { reserved  }
  { reserved  }
  { rowid structure  }
  { OSD dependent error  }
  { pointer to reserved area  }
  {  ub1          rcs9[CDA_SIZE - sizeof (struct cda_head)];        filler  }

  type
     Pcda_def = ^cda_def;
     cda_def = record
          v2_rc : sb2;
          ft : ub2;
          rpc : ub4;
          peo : ub2;
          fc : ub1;
          rcs1 : ub1;
          rc : ub2;
          wrn : ub1;
          rcs2 : ub1;
          rcs3 : sword;
          rid : record
               rd : record
                    rcs4 : ub4;
                    rcs5 : ub2;
                    rcs6 : ub1;
                 end;
               rcs7 : ub4;
               rcs8 : ub2;
            end;
          ose : sword;
          chk : ub1;
          rcsp : Pdvoid;
       end;

  { the logon data area (LDA)
     is the same shape as the CDA  }
     Lda_def = cda_Def;
     PLda_def = ^Lda_def;
  { OCI Environment Modes for opinit call  }
  { default single-threaded environment  }

  const
     OCI_EV_DEF = 0;     
  { thread-safe environment  }
     OCI_EV_TSF = 1;     
  { OCI Logon Modes for olog call  }
  { default login  }
     OCI_LM_DEF = 0;     
  { non-blocking logon  }
     OCI_LM_NBL = 1;     
  { 
   * since sqllib uses both ocidef and ocidfn the following defines
   * need to be guarded
    }

  { OCI_*_PIECE defines the piece types that are returned or set
   }
  { there or this is the only piece  }

  const
     OCI_ONE_PIECE = 0;     
  { the first of many pieces  }
     OCI_FIRST_PIECE = 1;     
  { the next of many pieces  }
     OCI_NEXT_PIECE = 2;     
  { the last piece of this column  }
     OCI_LAST_PIECE = 3;     

  { input data types  }
  { (ORANET TYPE) character string  }

  const
     SQLT_CHR = 1;     
  { (ORANET TYPE) oracle numeric  }
     SQLT_NUM = 2;     
  { (ORANET TYPE) integer  }
     SQLT_INT = 3;     
  { (ORANET TYPE) Floating point number  }
     SQLT_FLT = 4;     
  { zero terminated string  }
     SQLT_STR = 5;     
  { NUM with preceding length byte  }
     SQLT_VNU = 6;     
  { (ORANET TYPE) Packed Decimal Numeric  }
     SQLT_PDN = 7;     
  { long  }
     SQLT_LNG = 8;     
  { Variable character string  }
     SQLT_VCS = 9;     
  { Null/empty PCC Descriptor entry  }
     SQLT_NON = 10;     
  { rowid  }
     SQLT_RID = 11;     
  { date in oracle format  }
     SQLT_DAT = 12;     
  { binary in VCS format  }
     SQLT_VBI = 15;     
  { Native Binary float }
     SQLT_BFLOAT = 21;     
  { NAtive binary double  }
     SQLT_BDOUBLE = 22;     
  { binary data(DTYBIN)  }
     SQLT_BIN = 23;     
  { long binary  }
     SQLT_LBI = 24;     
  { unsigned integer  }
     SQLT_UIN = 68;     
  { Display sign leading separate  }
     SQLT_SLS = 91;     
  { Longer longs (char)  }
     SQLT_LVC = 94;     
  { Longer long binary  }
     SQLT_LVB = 95;     
  { Ansi fixed char  }
     SQLT_AFC = 96;     
  { Ansi Var char  }
     SQLT_AVC = 97;     
  { binary float canonical  }
     SQLT_IBFLOAT = 100;     
  { binary double canonical  }
     SQLT_IBDOUBLE = 101;     
  { cursor  type  }
     SQLT_CUR = 102;     
  { rowid descriptor  }
     SQLT_RDD = 104;     
  { label type  }
     SQLT_LAB = 105;     
  { oslabel type  }
     SQLT_OSL = 106;     
  { named object type  }
     SQLT_NTY = 108;     
  { ref type  }
     SQLT_REF = 110;     
  { character lob  }
     SQLT_CLOB = 112;     
  { binary lob  }
     SQLT_BLOB = 113;     
  { binary file lob  }
     SQLT_BFILEE = 114;     
  { character file lob  }
     SQLT_CFILEE = 115;     
  { result set type  }
     SQLT_RSET = 116;     
  { named collection type (varray or nested table)  }
     SQLT_NCO = 122;     
  { OCIString type  }
     SQLT_VST = 155;     
  { OCIDate type  }
     SQLT_ODT = 156;     
  { datetimes and intervals  }
  { ANSI Date  }
     SQLT_DATE = 184;     
  { TIME  }
     SQLT_TIME = 185;     
  { TIME WITH TIME ZONE  }
     SQLT_TIME_TZ = 186;     
  { TIMESTAMP  }
     SQLT_TIMESTAMP = 187;     
  { TIMESTAMP WITH TIME ZONE  }
     SQLT_TIMESTAMP_TZ = 188;     
  { INTERVAL YEAR TO MONTH  }
     SQLT_INTERVAL_YM = 189;     
  { INTERVAL DAY TO SECOND  }
     SQLT_INTERVAL_DS = 190;     
  { TIMESTAMP WITH LOCAL TZ  }
     SQLT_TIMESTAMP_LTZ = 232;     
  { pl/sql representation of named types  }
     SQLT_PNTY = 241;     
  { cxcheng: this has been added for backward compatibility -
     it needs to be here because ocidfn.h can get included ahead of sqldef.h  }
  { binary file lob  }
     SQLT_FILE = SQLT_BFILEE;     
     SQLT_CFILE = SQLT_CFILEE;     
     SQLT_BFILE = SQLT_BFILEE;     
  { CHAR/NCHAR/VARCHAR2/NVARCHAR2/CLOB/NCLOB char set "form" information  }
  { for CHAR, VARCHAR2, CLOB w/o a specified set  }
     SQLCS_IMPLICIT = 1;     
  { for NCHAR, NCHAR VARYING, NCLOB  }
     SQLCS_NCHAR = 2;     
  { for CHAR, etc, with "CHARACTER SET ..." syntax  }
     SQLCS_EXPLICIT = 3;     
  { for PL/SQL "flexible" parameters  }
     SQLCS_FLEXIBLE = 4;     
  { for typecheck of NULL and empty_clob() lits  }
     SQLCS_LIT_NULL = 5;     


