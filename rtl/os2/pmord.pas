{****************************************************************************

                   Copyright (c) 1998-2000 by Florian Kl„mpfl
                  
 ****************************************************************************}
unit pmord;

  interface
  
    uses
       os2def,pmgpi;

{$PACKRECORDS 1}

    type
       RECT1S = record
          xLeft : SHORT;
          yBottom : SHORT;
          xRight : SHORT;
          yTop : SHORT;
       end;

       ODPOINT = record
          dx : CHAR;
          dy : CHAR;
       end;

       SIZES = record
          cx : SHORT;
          cy : SHORT;
       end;

       SWPUSHORT = record
          HiByte : UCHAR;
          LoByte : UCHAR;
       end;

    const
       OCODE_GNOP1 = $00;
       OCODE_GESD = $FF;
       OCODE2_1 = $80;
       OCODE2_2 = $88;

    type
       ORDER = record
          idCode : UCHAR;
          uchData : UCHAR;
       end;

    const
       OCODE_GBAR = $68;
       OCODE_GCFIG = $7D;
       OCODE_GEEL = $49;
       OCODE_GEPTH = $7F;
       OCODE_GEPROL = $3E;
       OCODE_GPOP = $3F;
       OCODE_GSBMX = $0D;
       OCODE_GPSBMX = $4D;
       OCODE_GSCD = $3A;
       OCODE_GPSCD = $7A;
       OCODE_GSCR = $39;
       OCODE_GPSCR = $79;
       OCODE_GSCS = $38;
       OCODE_GPSCS = $78;
       OCODE_GSCOL = $0A;
       OCODE_GPSCOL = $4A;
       OCODE_GSLE = $1A;
       OCODE_GPSLE = $5A;
       OCODE_GSLJ = $1B;
       OCODE_GPSLJ = $5B;
       OCODE_GSLT = $18;
       OCODE_GPSLT = $58;
       OCODE_GSLW = $19;
       OCODE_GPSLW = $59;
       OCODE_GSMP = $3B;
       OCODE_GPSMP = $7B;
       OCODE_GSMS = $3C;
       OCODE_GPSMS = $7C;
       OCODE_GSMT = $29;
       OCODE_GPSMT = $69;
       OCODE_GSMX = $0C;
       OCODE_GPSMX = $4C;
       OCODE_GSPS = $08;
       OCODE_GPSPS = $48;
       OCODE_GSPT = $28;
       OCODE_GPSPT = $09;
       GBAR_RESERVED = $80;
       GBAR_BOUNDARY = $C0;
       GBAR_NOBOUNDARY = $80;
       GBAR_WINDING = $A0;
       GBAR_ALTERNATE = $80;
       GSCR_PRECISION = $0F;
       OCODE_VLONG = $FE;
       LORDER_ML = 253;

    type
       LORDER = record
          idCode : UCHAR;
          uchLength : UCHAR;
          uchData : array[0..LORDER_ML-1] of UCHAR;
       end;

    const
       OCODE_GEAR = $60;
       OCODE_GEIMG = $93;
       OCODE_GCCHST = $83;
       GCCHST_MC = 255;
       OCODE_GCHST = $C3;
       GCHST_SMC = 251;
       GCHST_LMC = 247;
       OCODE_GCCHSTM = $B1;
       GCCHSTM_MC = 255;
       OCODE_GCHSTM = $F1;
       GCHSTM_SMC = 251;
       GCHSTM_LMC = 247;
       OCODE_GCOMT = $01;
       GCOMT_ML = 255;
       OCODE_GIMD = $92;
       GIMD_ML = 255;
       OCODE_GCFARC = $87;
       OCODE_GFARC = $C7;
       OCODE_GLABL = $D3;
       OCODE_GSCP = $21;
       OCODE_GPSCP = $61;
       OCODE_GCBEZ = $A5;
       GCBEZ_SMB = 21;
       GCBEZ_LMB = 10;
       OCODE_GBEZ = $E5;
       GBEZ_SMB = 20;
       GBEZ_LMB = 10;
       OCODE_GCFLT = $85;
       GCFLT_SMP = 63;
       GCFLT_LMP = 31;
       OCODE_GFLT = $C5;
       GFLT_SMP = 62;
       GFLT_LMP = 30;
       OCODE_GCLINE = $81;
       GCLINE_SMP = 63;
       GCLINE_LMP = 31;
       OCODE_GLINE = $C1;
       GLINE_SMP = 62;
       GLINE_LMP = 30;
       OCODE_GCMRK = $82;
       GCMRK_SMP = 63;
       GCMRK_LMP = 31;
       OCODE_GMRK = $C2;
       GMRK_SMP = 62;
       GMRK_LMP = 30;
       OCODE_GCRLINE = $A1;
       GCRLINE_MP = 127;
       OCODE_GRLINE = $E1;
       GRLINE_SMP = 125;
       GRLINE_LMP = 123;
       OCODE_GSBCOL = $25;
       OCODE_GPSBCOL = $65;
       OCODE_GSECOL = $26;
       OCODE_GPSECOL = $66;
       SECOL_DEFAULT0 = $0000;
       SECOL_DEFAULT1 = $FF00;
       SECOL_NEUTRAL = $FF07;
       SECOL_RESET = $FF08;
       OCODE_GSCA = $34;
       OCODE_GPSCA = $74;
       OCODE_GSCH = $35;
       OCODE_GPSCH = $75;
       OCODE_GSFLW = $11;
       OCODE_GPSFLW = $51;
       OCODE_GSPIK = $43;
       OCODE_GPSPIK = $23;
       OCODE_GCARC = $86;
       OCODE_GARC = $C6;

    type
       ORDERS_GCARC = record
          ptInter : POINTS;
          ptEnd : POINTS;
       end;

       ORDERL_GCARC = record
          ptInter : POINTL;
          ptEnd : POINTL;
       end;

    const
       OCODE_GBEL = $D2;
       GBEL_DL = 251;

    type
       ORDER_GBEL = record
          lElementType : LONG;
          achDesc : array[0..GBEL_DL-1] of CHAR;
       end;

    const
       OCODE_GCBIMG = $91;
       OCODE_GBIMG = $D1;

    type
       ORDER_GCBIMG = record
          uchFormat : UCHAR;
          uchReserved : UCHAR;
          cx : SWPUSHORT;
          cy : SWPUSHORT;
       end;

    const
       OCODE_GBPTH = $D0;

    type
       ORDER_GBPTH = record
          usReserved : USHORT;
          idPath : LONG;
       end;

    const
       OCODE_GCBOX = $80;
       OCODE_GBOX = $C0;

    type
       ORDERS_GCBOX = record
          fbFlags : UCHAR;
          uchReserved : UCHAR;
          ptCorner : POINTS;
          hAxis : SHORT;
          vAxis : SHORT;
       end;

       ORDERL_GCBOX = record
          fbFlags : UCHAR;
          uchReserved : UCHAR;
          ptCorner : POINTL;
          hAxis : LONG;
          vAxis : LONG;
       end;

    const
       GCBOX_FILL = $40;
       GCBOX_BOUNDARY = $20;
       OCODE_GCALLS = $07;

    type
       ORDER_GCALLS = record
          sReserved : USHORT;
          idSegment : LONG;
       end;

    const
       OCODE_GFPTH = $D7;

    type
       ORDER_GFPTH = record
          fbFlags : UCHAR;
          uchReserved : UCHAR;
          idPath : LONG;
       end;

    const
       GFPTH_ALTERNATE = $00;
       GFPTH_WINDING = $40;
       GFPTH_MODIFY = $20;
       OCODE_GOPTH = $D4;

    type
       ORDER_GOPTH = record
          fbFlags : UCHAR;
          uchReserved : UCHAR;
          idPath : LONG;
       end;

    const
       OCODE_GMPTH = $D8;

    type
       ORDER_GMPTH = record
          uchMode : UCHAR;
          uchReserved : UCHAR;
          idPath : LONG;
       end;

    const
       GMPTH_STROKE = $06;
       OCODE_GCPARC = $A3;
       OCODE_GPARC = $E3;

    type
       ORDERS_GCPARC = record
          ptCenter : POINTS;
          ufx88Multiplier : FIXED88;
          usStartAngle : LONG;
          usSweepAngle : LONG;
       end;

       ORDERL_GCPARC = record
          ptCenter : POINTL;
          ufxMultiplier : FIXED;
          usStartAngle : LONG;
          usSweepAngle : LONG;
       end;

    const
       OCODE_GSCPTH = $B4;

    type
       ORDER_GSCPTH = record
          fbFlags : UCHAR;
          uchReserved : UCHAR;
          idPath : LONG;
       end;

    const
       GSCPTH_ALTERNATE = $00;
       GSCPTH_WINDING = $40;
       GSCPTH_RESET = $00;
       GSCPTH_INTERSECT = $20;
       OCODE_GSAP = $22;
       OCODE_GPSAP = $62;

    type
       ORDERS_GSAP = record
          p : SHORT;
          q : SHORT;
          r : SHORT;
          s : SHORT;
       end;

       ORDERL_GSAP = record
          p : LONG;
          q : LONG;
          r : LONG;
          s : LONG;
       end;

    const
       OCODE_GSBICOL = $A7;
       OCODE_GPSBICOL = $E7;
       OCODE_GSICOL = $A6;
       OCODE_GPSICOL = $E6;

    type
       ORDER_GSBICOL = record
          fbFlags : UCHAR;
          auchColor : array[0..3-1] of UCHAR;
       end;

    const
       SICOL_SPECIFY = $00;
       SICOL_SPECIAL = $40;
       SICOL_DEFAULT = $80;
       SICOL_BLACK = 1;
       SICOL_WHITE = 2;
       SICOL_ONES = 4;
       SICOL_ZEROES = 5;
       OCODE_GSCC = $33;
       OCODE_GPSCC = $03;

    type
       ORDERS_GSCC = record
          cxInt : SHORT;
          cyInt : SHORT;
          cxFract : USHORT;
          cyFract : USHORT;
          fbFlags : UCHAR;
          uchReserved : UCHAR;
       end;

       ORDERL_GSCC = record
          cxInt : LONG;
          cyInt : LONG;
          cxFract : USHORT;
          cyFract : USHORT;
          fbFlags : UCHAR;
          uchReserved : UCHAR;
       end;

    const
       GSCC_ZERODEF = $00;
       GSCC_ZEROZERO = $80;
       OCODE_GSMC = $37;
       OCODE_GPSMC = $77;

    type
       ORDERS_GSMC = record
          cx : SHORT;
          cy : SHORT;
          fbFlags : UCHAR;
          uchReserved : UCHAR;
       end;

       ORDERL_GSMC = record
          cx : LONG;
          cy : LONG;
          fbFlags : UCHAR;
          uchReserved : UCHAR;
       end;

    const
       GSMC_ZERODEF = $00;
       GSMC_ZEROZERO = $80;
       OCODE_GSPRP = $A0;
       OCODE_GPSPRP = $E0;

    type
       ORDERS_GSPRP = record
          fbFlags : UCHAR;
          uchReserved : UCHAR;
          ptPos : POINTS;
       end;

       ORDERL_GSPRP = record
          fbFlags : UCHAR;
          uchReserved : UCHAR;
          ptPos : POINTL;
       end;

    const
       GSPRP_DEFAULT = $80;
       GSPRP_SPECIFY = $00;
       OCODE_GSIA = $14;
       OCODE_GPSIA = $54;
       GSIA_VL = 3;

    type
       ORDER_GSIA = record
          uchAttrType : UCHAR;
          uchPrimType : UCHAR;
          fbFlags : UCHAR;
          auchValue : array[0..GSIA_VL-1] of UCHAR;
       end;

    const
       GSIA_COLOR = $01;
       GSIA_BCOLOR = $02;
       GSIA_MIX = $03;
       GSIA_BMIX = $04;
       GSIA_LINE = $01;
       GSIA_CHAR = $02;
       GSIA_MARKER = $03;
       GSIA_PATTERN = $04;
       GSIA_IMAGE = $05;
       GSIA_SPECIFY = $00;
       GSIA_SPECIAL = $40;
       GSIA_DEFAULT = $80;
       GSIA_BLACK = 1;
       GSIA_WHITE = 2;
       GSIA_ONES = 4;
       GSIA_ZEROES = 5;
       OCODE_GSTM = $24;
       OCODE_GPSTM = $64;
       OCODE_GSTV = $31;
       GSTM_ML = 16;

    type
       ORDERS_GSTM = record
          uchReserved : UCHAR;
          fbFlags : UCHAR;
          fsMask : USHORT;
          asMatrix : array[0..GSTM_ML-1] of SHORT;
       end;

       ORDERL_GSTM = record
          uchReserved : UCHAR;
          fbFlags : UCHAR;
          fsMask : USHORT;
          alMatrix : array[0..GSTM_ML-1] of LONG;
       end;

    const
       GSTM_M11 = $8000;
       GSTM_M12 = $4000;
       GSTM_M13 = $2000;
       GSTM_M14 = $1000;
       GSTM_M21 = $0800;
       GSTM_M22 = $0400;
       GSTM_M23 = $0200;
       GSTM_M24 = $0100;
       GSTM_M31 = $0080;
       GSTM_M32 = $0040;
       GSTM_M33 = $0020;
       GSTM_M34 = $0010;
       GSTM_M41 = $0008;
       GSTM_M42 = $0004;
       GSTM_M43 = $0002;
       GSTM_M44 = $0001;
       GSTM_UNITY = $00;
       GSTM_AFTER = $01;
       GSTM_BEFORE = $02;
       GSTM_OVERWRITE = $03;
       GSTV_OVERWRITE = $00;
       GSTV_AFTER = $04;
       OCODE_GSSB = $32;
       OCODE_GSVW = $27;
       OCODE_GPSVW = $67;
       GSSB_ML = 4;

    type
       ORDERS_GSSB = record
          fbFlags : UCHAR;
          fbMask : UCHAR;
          alMatrix : array[0..GSSB_ML-1] of SHORT;
       end;

       ORDERL_GSSB = record
          fbFLags : UCHAR;
          fbMask : UCHAR;
          alMatrix : array[0..GSSB_ML-1] of LONG;
       end;

    const
       GSSB_XLEFT = $20;
       GSSB_XRIGHT = $10;
       GSSB_YBOTTOM = $08;
       GSSB_YTOP = $04;
       GSVW_INTERSECT = $00;
       GSVW_REPLACE = $80;
       OCODE_GSGCH = $04;
       GSGCH_ML = 254;

    type
       ORDER_GSGCH = record
          uchIdent : UCHAR;
          auchData : array[0..GSGCH_ML-1] of UCHAR;
       end;

    const
       OCODE_GSSLW = $15;
       OCODE_GPSSLW = $55;

    type
       ORDERS_GSSLW = record
          fbFlags : UCHAR;
          uchReserved : UCHAR;
          LineWidth : SHORT;
       end;

       ORDERL_GSSLW = record
          fbFlags : UCHAR;
          uchReserved : UCHAR;
          LineWidth : LONG;
       end;

    const
       GSSLW_DEFAULT = $80;
       GSSLW_SPECIFY = $00;
       OCODE_GCSFLT = $A4;
       OCODE_GSFLT = $E4;
       GCSFLT_SMF = 21;
       GSFLT_SMF = 20;
       GCSFLT_SMF2 = GCSFLT_SMF*2;

    type
       ORDERS_GCSFLT = record
          apt : array[0..GCSFLT_SMF2-1] of POINTS;
          afxSharpness : array[0..GCSFLT_SMF-1] of FIXED;
       end;

    const
       GCSFLT_LMF = 12;
       GSFLT_LMF = 12;
       GCSFLT_LMF2 = GCSFLT_LMF*2;

    type
       ORDERL_GCSFLT = record
          apt : array[0..GCSFLT_LMF2-1] of POINTL;
          afxSharpness : array[0..GCSFLT_LMF-1] of FIXED;
       end;

    const
       OCODE_GBBLT = $D6;

    type
       ORDERS_GBBLT = record
          fsFlags : USHORT;
          usMix : USHORT;
          hbmSrc : HBITMAP;
          lOptions : LONG;
          rcsTargetRect : RECT1S;
          rclSourceRect : RECTL;
       end;

       ORDERL_GBBLT = record
          fsFlags : USHORT;
          usMix : USHORT;
          hbmSrc : HBITMAP;
          lOptions : LONG;
          rclTargetRect : RECTL;
          rclSourceRect : RECTL;
       end;

    const
       OCODE_GSCE = $17;
       OCODE_GPSCE = $57;
       OCODE_GSCBE = $05;
       OCODE_GPSCBE = $45;

    type
       ORDER_GSCBE = record
          fbFlags : UCHAR;
          uchReserved : UCHAR;
          ufxextra : FIXED;
       end;

    const
       OCODE_GESCP = $D5;
       GESCP_ML = 253;

    type
       ORDER_GESCP = record
          uchType : UCHAR;
          uchIdent : UCHAR;
          auchData : array[0..GESCP_ML-1] of UCHAR;
       end;

    const
       GESCP_REG = $80;
       GEBB_REGID = $02;
       ETYPE_GEBB = $800200D5;
       GEBB_LMP = 29;

    type
       ORDERL_GEBB = record
          fbFlags : UCHAR;
          usMix : USHORT;
          cPoints : UCHAR;
          hbmSrc : HBITMAP;
          lReserved : LONG;
          lOptions : LONG;
          aptPoints : array[0..GEBB_LMP-1] of POINTL;
       end;

    const
       GEPEL_REGID = $01;
       ETYPE_GEPEL = $800100D5;
       GEDB_REGID = $04;
       ETYPE_GEDB = $800400D5;

    type
       ORDERL_GEDB = record
          fsFlags : USHORT;
          usMix : USHORT;
          pBits : PVOID;
          pbmi : PBITMAPINFO2;
          lOptions : LONG;
          rclTargetRect : RECTL;
          rclSourceRect : RECTL;
       end;

    const
       GEFF_REGID = $03;
       ETYPE_GEFF = $800300D5;

    type
       ORDERL_GEFF = record
          fsFlags : UCHAR;
          auchColor : array[0..3-1] of UCHAR;
       end;

    const
       ETYPE_LINEBUNDLE = $0000FD01;
       ETYPE_CHARBUNDLE = $0000FD02;
       ETYPE_MARKERBUNDLE = $0000FD03;
       ETYPE_AREABUNDLE = $0000FD04;
       ETYPE_IMAGEBUNDLE = $0000FD05;
       VORDER_ML = 65531;

    type
       VORDER = record
          idCode : UCHAR;
          uchQualifier : UCHAR;
          uchLength : SWPUSHORT;
          uchData : array[0..VORDER_ML-1] of UCHAR;
       end;

    const
       OCODEQ_GCCHSTE = $B0;
       OCODEQ_GCHSTE = $F0;
       ETYPE_GCCHSTE = $0000FEB0;
       ETYPE_GCHSTE = $0000FEF0;

    type
       ORDERS_GCCHSTE = record
          fbFlags : UCHAR;
          uchReserved : UCHAR;
          ptRect : array[0..2-1] of POINTS;
          cchString : SWPUSHORT;
          achString : array[0..1-1] of CHAR;
          adx : array[0..1-1] of SHORT;
       end;

       ORDERL_GCCHSTE = record
          fbFlags : UCHAR;
          uchReserved : UCHAR;
          ptRect : array[0..2-1] of POINTL;
          cchString : SWPUSHORT;
          achString : array[0..1-1] of CHAR;
          adx : array[0..1-1] of LONG;
       end;

    const
       GCCHSTE_DRAWRECT = $80;
       GCCHSTE_NORECT = $00;
       GCCHSTE_CLIP = $40;
       GCCHSTE_NOCLIP = $00;
       GCCHSTE_DEEMPHASIZE = $20;
       GCCHSTE_NODEEMPHASIZE = $00;
       GCCHSTE_LEAVEPOS = $10;
       GCCHSTE_MOVEPOS = $00;
       GCCHSTE_UNDERSCORE = $08;
       GCCHSTE_NOUNDERSCORE = $00;
       GCCHSTE_STRIKEOUT = $04;
       GCCHSTE_NOSTRIKEOUT = $00;
       OCODEQ_GEESCP = $D5;
       GEESCP_ML = 65533;

    type
       ORDER_GEESCP = record
          uchType : UCHAR;
          uchIdent : UCHAR;
          auchData : array[0..GEESCP_ML-1] of UCHAR;
       end;

{$PACKRECORDS NORMAL}

  implementation

end.
