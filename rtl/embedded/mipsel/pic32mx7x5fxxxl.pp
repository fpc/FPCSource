unit
{$goto on}
  pic32mx7x5fxxxl;
{$L startup.o}
interface
{$PACKRECORDS 2}
var
  DefaultInterrupt: record end; external name '_DefaultInterrupt';
(*-------------------------------------------------------------------------
 * PIC32MX795F512L processor header
 *
 * This software is developed by Microchip Technology Inc. and its
 * subsidiaries ("Microchip").
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are 
 * met:
 * 
 * 1.      Redistributions of source code must retain the above copyright
 *         notice, this list of conditions and the following disclaimer.
 * 2.      Redistributions in binary form must reproduce the above 
 *         copyright notice, this list of conditions and the following 
 *         disclaimer in the documentation and/or other materials provided 
 *         with the distribution.
 * 3.      Microchip's name may not be used to endorse or promote products
 *         derived from this software without specific prior written 
 *         permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY MICROCHIP "AS IS" AND ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR PURPOSE ARE DISCLAIMED. IN NO EVENT 
 * SHALL MICROCHIP BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING BUT NOT LIMITED TO
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA OR PROFITS;
 * OR BUSINESS INTERRUPTION) HOWSOEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR 
 * OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
 * ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * 
 *-------------------------------------------------------------------------*)
var
  WDTCON : longWord absolute ($BF800000);
type
  TWDTCONbits = bitpacked record
  case integer of
  0 : (
    WDTCLR : 0..1;
    RESERVED0 : 0..1;
    SWDTPS : 0..31;
    RESERVED1 : 0..255;
    ON : 0..1;
  );
  1 : (
    RESERVED2 : 0..3;
    SWDTPS0 : 0..1;
    SWDTPS1 : 0..1;
    SWDTPS2 : 0..1;
    SWDTPS3 : 0..1;
    SWDTPS4 : 0..1;
  );
  2 : (
    RESERVED3 : 0..3;
    WDTPSTA : 0..31;
  );
  3 : (
    RESERVED4 : 0..3;
    WDTPS : 0..31;
  );
  4 : (
    w : 0..4294967295;
  );
  end;
var
  WDTCONbits: TWDTCONbits absolute ($BF800000);
  WDTCONCLR : longWord absolute ($BF800004);
  WDTCONSET : longWord absolute ($BF800008);
  WDTCONINV : longWord absolute ($BF80000C);
  RTCCON : longWord absolute ($BF800200);
type
  TRTCCONbits = bitpacked record
  case integer of
  0 : (
    RTCOE : 0..1;
    HALFSEC : 0..1;
    RTCSYNC : 0..1;
    RTCWREN : 0..1;
    RESERVED0 : 0..3;
    RTCCLKON : 0..1;
    RTSECSEL : 0..1;
    RESERVED1 : 0..31;
    SIDL : 0..1;
    RESERVED2 : 0..1;
    ON : 0..1;
    CAL : 0..1023;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  RTCCONbits: TRTCCONbits absolute ($BF800200);
  RTCCONCLR : longWord absolute ($BF800204);
  RTCCONSET : longWord absolute ($BF800208);
  RTCCONINV : longWord absolute ($BF80020C);
  RTCALRM : longWord absolute ($BF800210);
type
  TRTCALRMbits = bitpacked record
  case integer of
  0 : (
    ARPT : 0..255;
    AMASK : 0..15;
    ALRMSYNC : 0..1;
    PIV : 0..1;
    CHIME : 0..1;
    ALRMEN : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  RTCALRMbits: TRTCALRMbits absolute ($BF800210);
  RTCALRMCLR : longWord absolute ($BF800214);
  RTCALRMSET : longWord absolute ($BF800218);
  RTCALRMINV : longWord absolute ($BF80021C);
  RTCTIME : longWord absolute ($BF800220);
type
  TRTCTIMEbits = bitpacked record
  case integer of
  0 : (
    RESERVED0 : 0..255;
    SEC01 : 0..15;
    SEC10 : 0..15;
    MIN01 : 0..15;
    MIN10 : 0..15;
    HR01 : 0..15;
    HR10 : 0..15;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  RTCTIMEbits: TRTCTIMEbits absolute ($BF800220);
  RTCTIMECLR : longWord absolute ($BF800224);
  RTCTIMESET : longWord absolute ($BF800228);
  RTCTIMEINV : longWord absolute ($BF80022C);
  RTCDATE : longWord absolute ($BF800230);
type
  TRTCDATEbits = bitpacked record
  case integer of
  0 : (
    WDAY01 : 0..15;
    RESERVED0 : 0..15;
    DAY01 : 0..15;
    DAY10 : 0..15;
    MONTH01 : 0..15;
    MONTH10 : 0..15;
    YEAR01 : 0..15;
    YEAR10 : 0..15;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  RTCDATEbits: TRTCDATEbits absolute ($BF800230);
  RTCDATECLR : longWord absolute ($BF800234);
  RTCDATESET : longWord absolute ($BF800238);
  RTCDATEINV : longWord absolute ($BF80023C);
  ALRMTIME : longWord absolute ($BF800240);
type
  TALRMTIMEbits = bitpacked record
  case integer of
  0 : (
    RESERVED0 : 0..255;
    SEC01 : 0..15;
    SEC10 : 0..15;
    MIN01 : 0..15;
    MIN10 : 0..15;
    HR01 : 0..15;
    HR10 : 0..15;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  ALRMTIMEbits: TALRMTIMEbits absolute ($BF800240);
  ALRMTIMECLR : longWord absolute ($BF800244);
  ALRMTIMESET : longWord absolute ($BF800248);
  ALRMTIMEINV : longWord absolute ($BF80024C);
  ALRMDATE : longWord absolute ($BF800250);
type
  TALRMDATEbits = bitpacked record
  case integer of
  0 : (
    WDAY01 : 0..15;
    RESERVED0 : 0..15;
    DAY01 : 0..15;
    DAY10 : 0..15;
    MONTH01 : 0..15;
    MONTH10 : 0..15;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  ALRMDATEbits: TALRMDATEbits absolute ($BF800250);
  ALRMDATECLR : longWord absolute ($BF800254);
  ALRMDATESET : longWord absolute ($BF800258);
  ALRMDATEINV : longWord absolute ($BF80025C);
  T1CON : longWord absolute ($BF800600);
type
  TT1CONbits = bitpacked record
  case integer of
  0 : (
    RESERVED0 : 0..1;
    TCS : 0..1;
    TSYNC : 0..1;
    RESERVED1 : 0..1;
    TCKPS : 0..3;
    RESERVED2 : 0..1;
    TGATE : 0..1;
    RESERVED3 : 0..7;
    TWIP : 0..1;
    TWDIS : 0..1;
    SIDL : 0..1;
    RESERVED4 : 0..1;
    ON : 0..1;
  );
  1 : (
    RESERVED5 : 0..15;
    TCKPS0 : 0..1;
    TCKPS1 : 0..1;
  );
  2 : (
    RESERVED6 : 0..8191;
    TSIDL : 0..1;
    RESERVED7 : 0..1;
    TON : 0..1;
  );
  3 : (
    w : 0..4294967295;
  );
  end;
var
  T1CONbits: TT1CONbits absolute ($BF800600);
  T1CONCLR : longWord absolute ($BF800604);
  T1CONSET : longWord absolute ($BF800608);
  T1CONINV : longWord absolute ($BF80060C);
  TMR1 : longWord absolute ($BF800610);
  TMR1CLR : longWord absolute ($BF800614);
  TMR1SET : longWord absolute ($BF800618);
  TMR1INV : longWord absolute ($BF80061C);
  PR1 : longWord absolute ($BF800620);
  PR1CLR : longWord absolute ($BF800624);
  PR1SET : longWord absolute ($BF800628);
  PR1INV : longWord absolute ($BF80062C);
  T2CON : longWord absolute ($BF800800);
type
  TT2CONbits = bitpacked record
  case integer of
  0 : (
    RESERVED0 : 0..1;
    TCS : 0..1;
    RESERVED1 : 0..1;
    T32 : 0..1;
    TCKPS : 0..7;
    TGATE : 0..1;
    RESERVED2 : 0..31;
    SIDL : 0..1;
    RESERVED3 : 0..1;
    ON : 0..1;
  );
  1 : (
    RESERVED4 : 0..15;
    TCKPS0 : 0..1;
    TCKPS1 : 0..1;
    TCKPS2 : 0..1;
  );
  2 : (
    RESERVED5 : 0..8191;
    TSIDL : 0..1;
    RESERVED6 : 0..1;
    TON : 0..1;
  );
  3 : (
    w : 0..4294967295;
  );
  end;
var
  T2CONbits: TT2CONbits absolute ($BF800800);
  T2CONCLR : longWord absolute ($BF800804);
  T2CONSET : longWord absolute ($BF800808);
  T2CONINV : longWord absolute ($BF80080C);
  TMR2 : longWord absolute ($BF800810);
  TMR2CLR : longWord absolute ($BF800814);
  TMR2SET : longWord absolute ($BF800818);
  TMR2INV : longWord absolute ($BF80081C);
  PR2 : longWord absolute ($BF800820);
  PR2CLR : longWord absolute ($BF800824);
  PR2SET : longWord absolute ($BF800828);
  PR2INV : longWord absolute ($BF80082C);
  T3CON : longWord absolute ($BF800A00);
type
  TT3CONbits = bitpacked record
  case integer of
  0 : (
    RESERVED0 : 0..1;
    TCS : 0..1;
    RESERVED1 : 0..3;
    TCKPS : 0..7;
    TGATE : 0..1;
    RESERVED2 : 0..31;
    SIDL : 0..1;
    RESERVED3 : 0..1;
    ON : 0..1;
  );
  1 : (
    RESERVED4 : 0..15;
    TCKPS0 : 0..1;
    TCKPS1 : 0..1;
    TCKPS2 : 0..1;
  );
  2 : (
    RESERVED5 : 0..8191;
    TSIDL : 0..1;
    RESERVED6 : 0..1;
    TON : 0..1;
  );
  3 : (
    w : 0..4294967295;
  );
  end;
var
  T3CONbits: TT3CONbits absolute ($BF800A00);
  T3CONCLR : longWord absolute ($BF800A04);
  T3CONSET : longWord absolute ($BF800A08);
  T3CONINV : longWord absolute ($BF800A0C);
  TMR3 : longWord absolute ($BF800A10);
  TMR3CLR : longWord absolute ($BF800A14);
  TMR3SET : longWord absolute ($BF800A18);
  TMR3INV : longWord absolute ($BF800A1C);
  PR3 : longWord absolute ($BF800A20);
  PR3CLR : longWord absolute ($BF800A24);
  PR3SET : longWord absolute ($BF800A28);
  PR3INV : longWord absolute ($BF800A2C);
  T4CON : longWord absolute ($BF800C00);
type
  TT4CONbits = bitpacked record
  case integer of
  0 : (
    RESERVED0 : 0..1;
    TCS : 0..1;
    RESERVED1 : 0..1;
    T32 : 0..1;
    TCKPS : 0..7;
    TGATE : 0..1;
    RESERVED2 : 0..31;
    SIDL : 0..1;
    RESERVED3 : 0..1;
    ON : 0..1;
  );
  1 : (
    RESERVED4 : 0..15;
    TCKPS0 : 0..1;
    TCKPS1 : 0..1;
    TCKPS2 : 0..1;
  );
  2 : (
    RESERVED5 : 0..8191;
    TSIDL : 0..1;
    RESERVED6 : 0..1;
    TON : 0..1;
  );
  3 : (
    w : 0..4294967295;
  );
  end;
var
  T4CONbits: TT4CONbits absolute ($BF800C00);
  T4CONCLR : longWord absolute ($BF800C04);
  T4CONSET : longWord absolute ($BF800C08);
  T4CONINV : longWord absolute ($BF800C0C);
  TMR4 : longWord absolute ($BF800C10);
  TMR4CLR : longWord absolute ($BF800C14);
  TMR4SET : longWord absolute ($BF800C18);
  TMR4INV : longWord absolute ($BF800C1C);
  PR4 : longWord absolute ($BF800C20);
  PR4CLR : longWord absolute ($BF800C24);
  PR4SET : longWord absolute ($BF800C28);
  PR4INV : longWord absolute ($BF800C2C);
  T5CON : longWord absolute ($BF800E00);
type
  TT5CONbits = bitpacked record
  case integer of
  0 : (
    RESERVED0 : 0..1;
    TCS : 0..1;
    RESERVED1 : 0..3;
    TCKPS : 0..7;
    TGATE : 0..1;
    RESERVED2 : 0..31;
    SIDL : 0..1;
    RESERVED3 : 0..1;
    ON : 0..1;
  );
  1 : (
    RESERVED4 : 0..15;
    TCKPS0 : 0..1;
    TCKPS1 : 0..1;
    TCKPS2 : 0..1;
  );
  2 : (
    RESERVED5 : 0..8191;
    TSIDL : 0..1;
    RESERVED6 : 0..1;
    TON : 0..1;
  );
  3 : (
    w : 0..4294967295;
  );
  end;
var
  T5CONbits: TT5CONbits absolute ($BF800E00);
  T5CONCLR : longWord absolute ($BF800E04);
  T5CONSET : longWord absolute ($BF800E08);
  T5CONINV : longWord absolute ($BF800E0C);
  TMR5 : longWord absolute ($BF800E10);
  TMR5CLR : longWord absolute ($BF800E14);
  TMR5SET : longWord absolute ($BF800E18);
  TMR5INV : longWord absolute ($BF800E1C);
  PR5 : longWord absolute ($BF800E20);
  PR5CLR : longWord absolute ($BF800E24);
  PR5SET : longWord absolute ($BF800E28);
  PR5INV : longWord absolute ($BF800E2C);
  IC1CON : longWord absolute ($BF802000);
type
  TIC1CONbits = bitpacked record
  case integer of
  0 : (
    ICM : 0..7;
    ICBNE : 0..1;
    ICOV : 0..1;
    ICI : 0..3;
    ICTMR : 0..1;
    C32 : 0..1;
    FEDGE : 0..1;
    RESERVED0 : 0..7;
    SIDL : 0..1;
    RESERVED1 : 0..1;
    ON : 0..1;
  );
  1 : (
    ICM0 : 0..1;
    ICM1 : 0..1;
    ICM2 : 0..1;
    RESERVED2 : 0..3;
    ICI0 : 0..1;
    ICI1 : 0..1;
  );
  2 : (
    RESERVED3 : 0..8191;
    ICSIDL : 0..1;
  );
  3 : (
    w : 0..4294967295;
  );
  end;
var
  IC1CONbits: TIC1CONbits absolute ($BF802000);
  IC1CONCLR : longWord absolute ($BF802004);
  IC1CONSET : longWord absolute ($BF802008);
  IC1CONINV : longWord absolute ($BF80200C);
  IC1BUF : longWord absolute ($BF802010);
  IC2CON : longWord absolute ($BF802200);
type
  TIC2CONbits = bitpacked record
  case integer of
  0 : (
    ICM : 0..7;
    ICBNE : 0..1;
    ICOV : 0..1;
    ICI : 0..3;
    ICTMR : 0..1;
    C32 : 0..1;
    FEDGE : 0..1;
    RESERVED0 : 0..7;
    SIDL : 0..1;
    RESERVED1 : 0..1;
    ON : 0..1;
  );
  1 : (
    ICM0 : 0..1;
    ICM1 : 0..1;
    ICM2 : 0..1;
    RESERVED2 : 0..3;
    ICI0 : 0..1;
    ICI1 : 0..1;
  );
  2 : (
    RESERVED3 : 0..8191;
    ICSIDL : 0..1;
  );
  3 : (
    w : 0..4294967295;
  );
  end;
var
  IC2CONbits: TIC2CONbits absolute ($BF802200);
  IC2CONCLR : longWord absolute ($BF802204);
  IC2CONSET : longWord absolute ($BF802208);
  IC2CONINV : longWord absolute ($BF80220C);
  IC2BUF : longWord absolute ($BF802210);
  IC3CON : longWord absolute ($BF802400);
type
  TIC3CONbits = bitpacked record
  case integer of
  0 : (
    ICM : 0..7;
    ICBNE : 0..1;
    ICOV : 0..1;
    ICI : 0..3;
    ICTMR : 0..1;
    C32 : 0..1;
    FEDGE : 0..1;
    RESERVED0 : 0..7;
    SIDL : 0..1;
    RESERVED1 : 0..1;
    ON : 0..1;
  );
  1 : (
    ICM0 : 0..1;
    ICM1 : 0..1;
    ICM2 : 0..1;
    RESERVED2 : 0..3;
    ICI0 : 0..1;
    ICI1 : 0..1;
  );
  2 : (
    RESERVED3 : 0..8191;
    ICSIDL : 0..1;
  );
  3 : (
    w : 0..4294967295;
  );
  end;
var
  IC3CONbits: TIC3CONbits absolute ($BF802400);
  IC3CONCLR : longWord absolute ($BF802404);
  IC3CONSET : longWord absolute ($BF802408);
  IC3CONINV : longWord absolute ($BF80240C);
  IC3BUF : longWord absolute ($BF802410);
  IC4CON : longWord absolute ($BF802600);
type
  TIC4CONbits = bitpacked record
  case integer of
  0 : (
    ICM : 0..7;
    ICBNE : 0..1;
    ICOV : 0..1;
    ICI : 0..3;
    ICTMR : 0..1;
    C32 : 0..1;
    FEDGE : 0..1;
    RESERVED0 : 0..7;
    SIDL : 0..1;
    RESERVED1 : 0..1;
    ON : 0..1;
  );
  1 : (
    ICM0 : 0..1;
    ICM1 : 0..1;
    ICM2 : 0..1;
    RESERVED2 : 0..3;
    ICI0 : 0..1;
    ICI1 : 0..1;
  );
  2 : (
    RESERVED3 : 0..8191;
    ICSIDL : 0..1;
  );
  3 : (
    w : 0..4294967295;
  );
  end;
var
  IC4CONbits: TIC4CONbits absolute ($BF802600);
  IC4CONCLR : longWord absolute ($BF802604);
  IC4CONSET : longWord absolute ($BF802608);
  IC4CONINV : longWord absolute ($BF80260C);
  IC4BUF : longWord absolute ($BF802610);
  IC5CON : longWord absolute ($BF802800);
type
  TIC5CONbits = bitpacked record
  case integer of
  0 : (
    ICM : 0..7;
    ICBNE : 0..1;
    ICOV : 0..1;
    ICI : 0..3;
    ICTMR : 0..1;
    C32 : 0..1;
    FEDGE : 0..1;
    RESERVED0 : 0..7;
    SIDL : 0..1;
    RESERVED1 : 0..1;
    ON : 0..1;
  );
  1 : (
    ICM0 : 0..1;
    ICM1 : 0..1;
    ICM2 : 0..1;
    RESERVED2 : 0..3;
    ICI0 : 0..1;
    ICI1 : 0..1;
  );
  2 : (
    RESERVED3 : 0..8191;
    ICSIDL : 0..1;
  );
  3 : (
    w : 0..4294967295;
  );
  end;
var
  IC5CONbits: TIC5CONbits absolute ($BF802800);
  IC5CONCLR : longWord absolute ($BF802804);
  IC5CONSET : longWord absolute ($BF802808);
  IC5CONINV : longWord absolute ($BF80280C);
  IC5BUF : longWord absolute ($BF802810);
  OC1CON : longWord absolute ($BF803000);
type
  TOC1CONbits = bitpacked record
  case integer of
  0 : (
    OCM : 0..7;
    OCTSEL : 0..1;
    OCFLT : 0..1;
    OC32 : 0..1;
    RESERVED0 : 0..127;
    SIDL : 0..1;
    RESERVED1 : 0..1;
    ON : 0..1;
  );
  1 : (
    OCM0 : 0..1;
    OCM1 : 0..1;
    OCM2 : 0..1;
  );
  2 : (
    RESERVED2 : 0..8191;
    OCSIDL : 0..1;
  );
  3 : (
    w : 0..4294967295;
  );
  end;
var
  OC1CONbits: TOC1CONbits absolute ($BF803000);
  OC1CONCLR : longWord absolute ($BF803004);
  OC1CONSET : longWord absolute ($BF803008);
  OC1CONINV : longWord absolute ($BF80300C);
  OC1R : longWord absolute ($BF803010);
  OC1RCLR : longWord absolute ($BF803014);
  OC1RSET : longWord absolute ($BF803018);
  OC1RINV : longWord absolute ($BF80301C);
  OC1RS : longWord absolute ($BF803020);
  OC1RSCLR : longWord absolute ($BF803024);
  OC1RSSET : longWord absolute ($BF803028);
  OC1RSINV : longWord absolute ($BF80302C);
  OC2CON : longWord absolute ($BF803200);
type
  TOC2CONbits = bitpacked record
  case integer of
  0 : (
    OCM : 0..7;
    OCTSEL : 0..1;
    OCFLT : 0..1;
    OC32 : 0..1;
    RESERVED0 : 0..127;
    SIDL : 0..1;
    RESERVED1 : 0..1;
    ON : 0..1;
  );
  1 : (
    OCM0 : 0..1;
    OCM1 : 0..1;
    OCM2 : 0..1;
  );
  2 : (
    RESERVED2 : 0..8191;
    OCSIDL : 0..1;
  );
  3 : (
    w : 0..4294967295;
  );
  end;
var
  OC2CONbits: TOC2CONbits absolute ($BF803200);
  OC2CONCLR : longWord absolute ($BF803204);
  OC2CONSET : longWord absolute ($BF803208);
  OC2CONINV : longWord absolute ($BF80320C);
  OC2R : longWord absolute ($BF803210);
  OC2RCLR : longWord absolute ($BF803214);
  OC2RSET : longWord absolute ($BF803218);
  OC2RINV : longWord absolute ($BF80321C);
  OC2RS : longWord absolute ($BF803220);
  OC2RSCLR : longWord absolute ($BF803224);
  OC2RSSET : longWord absolute ($BF803228);
  OC2RSINV : longWord absolute ($BF80322C);
  OC3CON : longWord absolute ($BF803400);
type
  TOC3CONbits = bitpacked record
  case integer of
  0 : (
    OCM : 0..7;
    OCTSEL : 0..1;
    OCFLT : 0..1;
    OC32 : 0..1;
    RESERVED0 : 0..127;
    SIDL : 0..1;
    RESERVED1 : 0..1;
    ON : 0..1;
  );
  1 : (
    OCM0 : 0..1;
    OCM1 : 0..1;
    OCM2 : 0..1;
  );
  2 : (
    RESERVED2 : 0..8191;
    OCSIDL : 0..1;
  );
  3 : (
    w : 0..4294967295;
  );
  end;
var
  OC3CONbits: TOC3CONbits absolute ($BF803400);
  OC3CONCLR : longWord absolute ($BF803404);
  OC3CONSET : longWord absolute ($BF803408);
  OC3CONINV : longWord absolute ($BF80340C);
  OC3R : longWord absolute ($BF803410);
  OC3RCLR : longWord absolute ($BF803414);
  OC3RSET : longWord absolute ($BF803418);
  OC3RINV : longWord absolute ($BF80341C);
  OC3RS : longWord absolute ($BF803420);
  OC3RSCLR : longWord absolute ($BF803424);
  OC3RSSET : longWord absolute ($BF803428);
  OC3RSINV : longWord absolute ($BF80342C);
  OC4CON : longWord absolute ($BF803600);
type
  TOC4CONbits = bitpacked record
  case integer of
  0 : (
    OCM : 0..7;
    OCTSEL : 0..1;
    OCFLT : 0..1;
    OC32 : 0..1;
    RESERVED0 : 0..127;
    SIDL : 0..1;
    RESERVED1 : 0..1;
    ON : 0..1;
  );
  1 : (
    OCM0 : 0..1;
    OCM1 : 0..1;
    OCM2 : 0..1;
  );
  2 : (
    RESERVED2 : 0..8191;
    OCSIDL : 0..1;
  );
  3 : (
    w : 0..4294967295;
  );
  end;
var
  OC4CONbits: TOC4CONbits absolute ($BF803600);
  OC4CONCLR : longWord absolute ($BF803604);
  OC4CONSET : longWord absolute ($BF803608);
  OC4CONINV : longWord absolute ($BF80360C);
  OC4R : longWord absolute ($BF803610);
  OC4RCLR : longWord absolute ($BF803614);
  OC4RSET : longWord absolute ($BF803618);
  OC4RINV : longWord absolute ($BF80361C);
  OC4RS : longWord absolute ($BF803620);
  OC4RSCLR : longWord absolute ($BF803624);
  OC4RSSET : longWord absolute ($BF803628);
  OC4RSINV : longWord absolute ($BF80362C);
  OC5CON : longWord absolute ($BF803800);
type
  TOC5CONbits = bitpacked record
  case integer of
  0 : (
    OCM : 0..7;
    OCTSEL : 0..1;
    OCFLT : 0..1;
    OC32 : 0..1;
    RESERVED0 : 0..127;
    SIDL : 0..1;
    RESERVED1 : 0..1;
    ON : 0..1;
  );
  1 : (
    OCM0 : 0..1;
    OCM1 : 0..1;
    OCM2 : 0..1;
  );
  2 : (
    RESERVED2 : 0..8191;
    OCSIDL : 0..1;
  );
  3 : (
    w : 0..4294967295;
  );
  end;
var
  OC5CONbits: TOC5CONbits absolute ($BF803800);
  OC5CONCLR : longWord absolute ($BF803804);
  OC5CONSET : longWord absolute ($BF803808);
  OC5CONINV : longWord absolute ($BF80380C);
  OC5R : longWord absolute ($BF803810);
  OC5RCLR : longWord absolute ($BF803814);
  OC5RSET : longWord absolute ($BF803818);
  OC5RINV : longWord absolute ($BF80381C);
  OC5RS : longWord absolute ($BF803820);
  OC5RSCLR : longWord absolute ($BF803824);
  OC5RSSET : longWord absolute ($BF803828);
  OC5RSINV : longWord absolute ($BF80382C);
  I2C1ACON : longWord absolute ($BF805000);
type
  TI2C1ACONbits = bitpacked record
  case integer of
  0 : (
    SEN : 0..1;
    RSEN : 0..1;
    PEN : 0..1;
    RCEN : 0..1;
    ACKEN : 0..1;
    ACKDT : 0..1;
    STREN : 0..1;
    GCEN : 0..1;
    SMEN : 0..1;
    DISSLW : 0..1;
    A10M : 0..1;
    STRICT : 0..1;
    SCLREL : 0..1;
    SIDL : 0..1;
    RESERVED0 : 0..1;
    ON : 0..1;
  );
  1 : (
    RESERVED1 : 0..2047;
    IPMIEN : 0..1;
    RESERVED2 : 0..1;
    I2CSIDL : 0..1;
    RESERVED3 : 0..1;
    I2CEN : 0..1;
  );
  2 : (
    w : 0..4294967295;
  );
  end;
var
  I2C1ACONbits: TI2C1ACONbits absolute ($BF805000);
  I2C3CON : longWord absolute ($BF805000);
type
  TI2C3CONbits = bitpacked record
  case integer of
  0 : (
    SEN : 0..1;
    RSEN : 0..1;
    PEN : 0..1;
    RCEN : 0..1;
    ACKEN : 0..1;
    ACKDT : 0..1;
    STREN : 0..1;
    GCEN : 0..1;
    SMEN : 0..1;
    DISSLW : 0..1;
    A10M : 0..1;
    STRICT : 0..1;
    SCLREL : 0..1;
    SIDL : 0..1;
    RESERVED0 : 0..1;
    ON : 0..1;
  );
  1 : (
    RESERVED1 : 0..2047;
    IPMIEN : 0..1;
    RESERVED2 : 0..1;
    I2CSIDL : 0..1;
    RESERVED3 : 0..1;
    I2CEN : 0..1;
  );
  2 : (
    w : 0..4294967295;
  );
  end;
var
  I2C3CONbits: TI2C3CONbits absolute ($BF805000);
  I2C1ACONCLR : longWord absolute ($BF805004);
  I2C3CONCLR : longWord absolute ($BF805004);
  I2C1ACONSET : longWord absolute ($BF805008);
  I2C3CONSET : longWord absolute ($BF805008);
  I2C1ACONINV : longWord absolute ($BF80500C);
  I2C3CONINV : longWord absolute ($BF80500C);
  I2C1ASTAT : longWord absolute ($BF805010);
type
  TI2C1ASTATbits = bitpacked record
  case integer of
  0 : (
    TBF : 0..1;
    RBF : 0..1;
    R_W : 0..1;
    S : 0..1;
    P : 0..1;
    D_A : 0..1;
    I2COV : 0..1;
    IWCOL : 0..1;
    ADD10 : 0..1;
    GCSTAT : 0..1;
    BCL : 0..1;
    RESERVED0 : 0..7;
    TRSTAT : 0..1;
    ACKSTAT : 0..1;
  );
  1 : (
    RESERVED1 : 0..63;
    I2CPOV : 0..1;
  );
  2 : (
    w : 0..4294967295;
  );
  end;
var
  I2C1ASTATbits: TI2C1ASTATbits absolute ($BF805010);
  I2C3STAT : longWord absolute ($BF805010);
type
  TI2C3STATbits = bitpacked record
  case integer of
  0 : (
    TBF : 0..1;
    RBF : 0..1;
    R_W : 0..1;
    S : 0..1;
    P : 0..1;
    D_A : 0..1;
    I2COV : 0..1;
    IWCOL : 0..1;
    ADD10 : 0..1;
    GCSTAT : 0..1;
    BCL : 0..1;
    RESERVED0 : 0..7;
    TRSTAT : 0..1;
    ACKSTAT : 0..1;
  );
  1 : (
    RESERVED1 : 0..63;
    I2CPOV : 0..1;
  );
  2 : (
    w : 0..4294967295;
  );
  end;
var
  I2C3STATbits: TI2C3STATbits absolute ($BF805010);
  I2C1ASTATCLR : longWord absolute ($BF805014);
  I2C3STATCLR : longWord absolute ($BF805014);
  I2C1ASTATSET : longWord absolute ($BF805018);
  I2C3STATSET : longWord absolute ($BF805018);
  I2C1ASTATINV : longWord absolute ($BF80501C);
  I2C3STATINV : longWord absolute ($BF80501C);
  I2C1AADD : longWord absolute ($BF805020);
  I2C3ADD : longWord absolute ($BF805020);
  I2C1AADDCLR : longWord absolute ($BF805024);
  I2C3ADDCLR : longWord absolute ($BF805024);
  I2C1AADDSET : longWord absolute ($BF805028);
  I2C3ADDSET : longWord absolute ($BF805028);
  I2C1AADDINV : longWord absolute ($BF80502C);
  I2C3ADDINV : longWord absolute ($BF80502C);
  I2C1AMSK : longWord absolute ($BF805030);
  I2C3MSK : longWord absolute ($BF805030);
  I2C1AMSKCLR : longWord absolute ($BF805034);
  I2C3MSKCLR : longWord absolute ($BF805034);
  I2C1AMSKSET : longWord absolute ($BF805038);
  I2C3MSKSET : longWord absolute ($BF805038);
  I2C1AMSKINV : longWord absolute ($BF80503C);
  I2C3MSKINV : longWord absolute ($BF80503C);
  I2C1ABRG : longWord absolute ($BF805040);
  I2C3BRG : longWord absolute ($BF805040);
  I2C1ABRGCLR : longWord absolute ($BF805044);
  I2C3BRGCLR : longWord absolute ($BF805044);
  I2C1ABRGSET : longWord absolute ($BF805048);
  I2C3BRGSET : longWord absolute ($BF805048);
  I2C1ABRGINV : longWord absolute ($BF80504C);
  I2C3BRGINV : longWord absolute ($BF80504C);
  I2C1ATRN : longWord absolute ($BF805050);
  I2C3TRN : longWord absolute ($BF805050);
  I2C1ATRNCLR : longWord absolute ($BF805054);
  I2C3TRNCLR : longWord absolute ($BF805054);
  I2C1ATRNSET : longWord absolute ($BF805058);
  I2C3TRNSET : longWord absolute ($BF805058);
  I2C1ATRNINV : longWord absolute ($BF80505C);
  I2C3TRNINV : longWord absolute ($BF80505C);
  I2C1ARCV : longWord absolute ($BF805060);
  I2C3RCV : longWord absolute ($BF805060);
  I2C2ACON : longWord absolute ($BF805100);
type
  TI2C2ACONbits = bitpacked record
  case integer of
  0 : (
    SEN : 0..1;
    RSEN : 0..1;
    PEN : 0..1;
    RCEN : 0..1;
    ACKEN : 0..1;
    ACKDT : 0..1;
    STREN : 0..1;
    GCEN : 0..1;
    SMEN : 0..1;
    DISSLW : 0..1;
    A10M : 0..1;
    STRICT : 0..1;
    SCLREL : 0..1;
    SIDL : 0..1;
    RESERVED0 : 0..1;
    ON : 0..1;
  );
  1 : (
    RESERVED1 : 0..2047;
    IPMIEN : 0..1;
    RESERVED2 : 0..1;
    I2CSIDL : 0..1;
    RESERVED3 : 0..1;
    I2CEN : 0..1;
  );
  2 : (
    w : 0..4294967295;
  );
  end;
var
  I2C2ACONbits: TI2C2ACONbits absolute ($BF805100);
  I2C4CON : longWord absolute ($BF805100);
type
  TI2C4CONbits = bitpacked record
  case integer of
  0 : (
    SEN : 0..1;
    RSEN : 0..1;
    PEN : 0..1;
    RCEN : 0..1;
    ACKEN : 0..1;
    ACKDT : 0..1;
    STREN : 0..1;
    GCEN : 0..1;
    SMEN : 0..1;
    DISSLW : 0..1;
    A10M : 0..1;
    STRICT : 0..1;
    SCLREL : 0..1;
    SIDL : 0..1;
    RESERVED0 : 0..1;
    ON : 0..1;
  );
  1 : (
    RESERVED1 : 0..2047;
    IPMIEN : 0..1;
    RESERVED2 : 0..1;
    I2CSIDL : 0..1;
    RESERVED3 : 0..1;
    I2CEN : 0..1;
  );
  2 : (
    w : 0..4294967295;
  );
  end;
var
  I2C4CONbits: TI2C4CONbits absolute ($BF805100);
  I2C2ACONCLR : longWord absolute ($BF805104);
  I2C4CONCLR : longWord absolute ($BF805104);
  I2C2ACONSET : longWord absolute ($BF805108);
  I2C4CONSET : longWord absolute ($BF805108);
  I2C2ACONINV : longWord absolute ($BF80510C);
  I2C4CONINV : longWord absolute ($BF80510C);
  I2C2ASTAT : longWord absolute ($BF805110);
type
  TI2C2ASTATbits = bitpacked record
  case integer of
  0 : (
    TBF : 0..1;
    RBF : 0..1;
    R_W : 0..1;
    S : 0..1;
    P : 0..1;
    D_A : 0..1;
    I2COV : 0..1;
    IWCOL : 0..1;
    ADD10 : 0..1;
    GCSTAT : 0..1;
    BCL : 0..1;
    RESERVED0 : 0..7;
    TRSTAT : 0..1;
    ACKSTAT : 0..1;
  );
  1 : (
    RESERVED1 : 0..63;
    I2CPOV : 0..1;
  );
  2 : (
    w : 0..4294967295;
  );
  end;
var
  I2C2ASTATbits: TI2C2ASTATbits absolute ($BF805110);
  I2C4STAT : longWord absolute ($BF805110);
type
  TI2C4STATbits = bitpacked record
  case integer of
  0 : (
    TBF : 0..1;
    RBF : 0..1;
    R_W : 0..1;
    S : 0..1;
    P : 0..1;
    D_A : 0..1;
    I2COV : 0..1;
    IWCOL : 0..1;
    ADD10 : 0..1;
    GCSTAT : 0..1;
    BCL : 0..1;
    RESERVED0 : 0..7;
    TRSTAT : 0..1;
    ACKSTAT : 0..1;
  );
  1 : (
    RESERVED1 : 0..63;
    I2CPOV : 0..1;
  );
  2 : (
    w : 0..4294967295;
  );
  end;
var
  I2C4STATbits: TI2C4STATbits absolute ($BF805110);
  I2C2ASTATCLR : longWord absolute ($BF805114);
  I2C4STATCLR : longWord absolute ($BF805114);
  I2C2ASTATSET : longWord absolute ($BF805118);
  I2C4STATSET : longWord absolute ($BF805118);
  I2C2ASTATINV : longWord absolute ($BF80511C);
  I2C4STATINV : longWord absolute ($BF80511C);
  I2C2AADD : longWord absolute ($BF805120);
  I2C4ADD : longWord absolute ($BF805120);
  I2C2AADDCLR : longWord absolute ($BF805124);
  I2C4ADDCLR : longWord absolute ($BF805124);
  I2C2AADDSET : longWord absolute ($BF805128);
  I2C4ADDSET : longWord absolute ($BF805128);
  I2C2AADDINV : longWord absolute ($BF80512C);
  I2C4ADDINV : longWord absolute ($BF80512C);
  I2C2AMSK : longWord absolute ($BF805130);
  I2C4MSK : longWord absolute ($BF805130);
  I2C2AMSKCLR : longWord absolute ($BF805134);
  I2C4MSKCLR : longWord absolute ($BF805134);
  I2C2AMSKSET : longWord absolute ($BF805138);
  I2C4MSKSET : longWord absolute ($BF805138);
  I2C2AMSKINV : longWord absolute ($BF80513C);
  I2C4MSKINV : longWord absolute ($BF80513C);
  I2C2ABRG : longWord absolute ($BF805140);
  I2C4BRG : longWord absolute ($BF805140);
  I2C2ABRGCLR : longWord absolute ($BF805144);
  I2C4BRGCLR : longWord absolute ($BF805144);
  I2C2ABRGSET : longWord absolute ($BF805148);
  I2C4BRGSET : longWord absolute ($BF805148);
  I2C2ABRGINV : longWord absolute ($BF80514C);
  I2C4BRGINV : longWord absolute ($BF80514C);
  I2C2ATRN : longWord absolute ($BF805150);
  I2C4TRN : longWord absolute ($BF805150);
  I2C2ATRNCLR : longWord absolute ($BF805154);
  I2C4TRNCLR : longWord absolute ($BF805154);
  I2C2ATRNSET : longWord absolute ($BF805158);
  I2C4TRNSET : longWord absolute ($BF805158);
  I2C2ATRNINV : longWord absolute ($BF80515C);
  I2C4TRNINV : longWord absolute ($BF80515C);
  I2C2ARCV : longWord absolute ($BF805160);
  I2C4RCV : longWord absolute ($BF805160);
  I2C3ACON : longWord absolute ($BF805200);
type
  TI2C3ACONbits = bitpacked record
  case integer of
  0 : (
    SEN : 0..1;
    RSEN : 0..1;
    PEN : 0..1;
    RCEN : 0..1;
    ACKEN : 0..1;
    ACKDT : 0..1;
    STREN : 0..1;
    GCEN : 0..1;
    SMEN : 0..1;
    DISSLW : 0..1;
    A10M : 0..1;
    STRICT : 0..1;
    SCLREL : 0..1;
    SIDL : 0..1;
    RESERVED0 : 0..1;
    ON : 0..1;
  );
  1 : (
    RESERVED1 : 0..2047;
    IPMIEN : 0..1;
    RESERVED2 : 0..1;
    I2CSIDL : 0..1;
    RESERVED3 : 0..1;
    I2CEN : 0..1;
  );
  2 : (
    w : 0..4294967295;
  );
  end;
var
  I2C3ACONbits: TI2C3ACONbits absolute ($BF805200);
  I2C5CON : longWord absolute ($BF805200);
type
  TI2C5CONbits = bitpacked record
  case integer of
  0 : (
    SEN : 0..1;
    RSEN : 0..1;
    PEN : 0..1;
    RCEN : 0..1;
    ACKEN : 0..1;
    ACKDT : 0..1;
    STREN : 0..1;
    GCEN : 0..1;
    SMEN : 0..1;
    DISSLW : 0..1;
    A10M : 0..1;
    STRICT : 0..1;
    SCLREL : 0..1;
    SIDL : 0..1;
    RESERVED0 : 0..1;
    ON : 0..1;
  );
  1 : (
    RESERVED1 : 0..2047;
    IPMIEN : 0..1;
    RESERVED2 : 0..1;
    I2CSIDL : 0..1;
    RESERVED3 : 0..1;
    I2CEN : 0..1;
  );
  2 : (
    w : 0..4294967295;
  );
  end;
var
  I2C5CONbits: TI2C5CONbits absolute ($BF805200);
  I2C3ACONCLR : longWord absolute ($BF805204);
  I2C5CONCLR : longWord absolute ($BF805204);
  I2C3ACONSET : longWord absolute ($BF805208);
  I2C5CONSET : longWord absolute ($BF805208);
  I2C3ACONINV : longWord absolute ($BF80520C);
  I2C5CONINV : longWord absolute ($BF80520C);
  I2C3ASTAT : longWord absolute ($BF805210);
type
  TI2C3ASTATbits = bitpacked record
  case integer of
  0 : (
    TBF : 0..1;
    RBF : 0..1;
    R_W : 0..1;
    S : 0..1;
    P : 0..1;
    D_A : 0..1;
    I2COV : 0..1;
    IWCOL : 0..1;
    ADD10 : 0..1;
    GCSTAT : 0..1;
    BCL : 0..1;
    RESERVED0 : 0..7;
    TRSTAT : 0..1;
    ACKSTAT : 0..1;
  );
  1 : (
    RESERVED1 : 0..63;
    I2CPOV : 0..1;
  );
  2 : (
    w : 0..4294967295;
  );
  end;
var
  I2C3ASTATbits: TI2C3ASTATbits absolute ($BF805210);
  I2C5STAT : longWord absolute ($BF805210);
type
  TI2C5STATbits = bitpacked record
  case integer of
  0 : (
    TBF : 0..1;
    RBF : 0..1;
    R_W : 0..1;
    S : 0..1;
    P : 0..1;
    D_A : 0..1;
    I2COV : 0..1;
    IWCOL : 0..1;
    ADD10 : 0..1;
    GCSTAT : 0..1;
    BCL : 0..1;
    RESERVED0 : 0..7;
    TRSTAT : 0..1;
    ACKSTAT : 0..1;
  );
  1 : (
    RESERVED1 : 0..63;
    I2CPOV : 0..1;
  );
  2 : (
    w : 0..4294967295;
  );
  end;
var
  I2C5STATbits: TI2C5STATbits absolute ($BF805210);
  I2C3ASTATCLR : longWord absolute ($BF805214);
  I2C5STATCLR : longWord absolute ($BF805214);
  I2C3ASTATSET : longWord absolute ($BF805218);
  I2C5STATSET : longWord absolute ($BF805218);
  I2C3ASTATINV : longWord absolute ($BF80521C);
  I2C5STATINV : longWord absolute ($BF80521C);
  I2C3AADD : longWord absolute ($BF805220);
  I2C5ADD : longWord absolute ($BF805220);
  I2C3AADDCLR : longWord absolute ($BF805224);
  I2C5ADDCLR : longWord absolute ($BF805224);
  I2C3AADDSET : longWord absolute ($BF805228);
  I2C5ADDSET : longWord absolute ($BF805228);
  I2C3AADDINV : longWord absolute ($BF80522C);
  I2C5ADDINV : longWord absolute ($BF80522C);
  I2C3AMSK : longWord absolute ($BF805230);
  I2C5MSK : longWord absolute ($BF805230);
  I2C3AMSKCLR : longWord absolute ($BF805234);
  I2C5MSKCLR : longWord absolute ($BF805234);
  I2C3AMSKSET : longWord absolute ($BF805238);
  I2C5MSKSET : longWord absolute ($BF805238);
  I2C3AMSKINV : longWord absolute ($BF80523C);
  I2C5MSKINV : longWord absolute ($BF80523C);
  I2C3ABRG : longWord absolute ($BF805240);
  I2C5BRG : longWord absolute ($BF805240);
  I2C3ABRGCLR : longWord absolute ($BF805244);
  I2C5BRGCLR : longWord absolute ($BF805244);
  I2C3ABRGSET : longWord absolute ($BF805248);
  I2C5BRGSET : longWord absolute ($BF805248);
  I2C3ABRGINV : longWord absolute ($BF80524C);
  I2C5BRGINV : longWord absolute ($BF80524C);
  I2C3ATRN : longWord absolute ($BF805250);
  I2C5TRN : longWord absolute ($BF805250);
  I2C3ATRNCLR : longWord absolute ($BF805254);
  I2C5TRNCLR : longWord absolute ($BF805254);
  I2C3ATRNSET : longWord absolute ($BF805258);
  I2C5TRNSET : longWord absolute ($BF805258);
  I2C3ATRNINV : longWord absolute ($BF80525C);
  I2C5TRNINV : longWord absolute ($BF80525C);
  I2C3ARCV : longWord absolute ($BF805260);
  I2C5RCV : longWord absolute ($BF805260);
  I2C1CON : longWord absolute ($BF805300);
type
  TI2C1CONbits = bitpacked record
  case integer of
  0 : (
    SEN : 0..1;
    RSEN : 0..1;
    PEN : 0..1;
    RCEN : 0..1;
    ACKEN : 0..1;
    ACKDT : 0..1;
    STREN : 0..1;
    GCEN : 0..1;
    SMEN : 0..1;
    DISSLW : 0..1;
    A10M : 0..1;
    STRICT : 0..1;
    SCLREL : 0..1;
    SIDL : 0..1;
    RESERVED0 : 0..1;
    ON : 0..1;
  );
  1 : (
    RESERVED1 : 0..2047;
    IPMIEN : 0..1;
    RESERVED2 : 0..1;
    I2CSIDL : 0..1;
    RESERVED3 : 0..1;
    I2CEN : 0..1;
  );
  2 : (
    w : 0..4294967295;
  );
  end;
var
  I2C1CONbits: TI2C1CONbits absolute ($BF805300);
  I2C1CONCLR : longWord absolute ($BF805304);
  I2C1CONSET : longWord absolute ($BF805308);
  I2C1CONINV : longWord absolute ($BF80530C);
  I2C1STAT : longWord absolute ($BF805310);
type
  TI2C1STATbits = bitpacked record
  case integer of
  0 : (
    TBF : 0..1;
    RBF : 0..1;
    R_W : 0..1;
    S : 0..1;
    P : 0..1;
    D_A : 0..1;
    I2COV : 0..1;
    IWCOL : 0..1;
    ADD10 : 0..1;
    GCSTAT : 0..1;
    BCL : 0..1;
    RESERVED0 : 0..7;
    TRSTAT : 0..1;
    ACKSTAT : 0..1;
  );
  1 : (
    RESERVED1 : 0..63;
    I2CPOV : 0..1;
  );
  2 : (
    w : 0..4294967295;
  );
  end;
var
  I2C1STATbits: TI2C1STATbits absolute ($BF805310);
  I2C1STATCLR : longWord absolute ($BF805314);
  I2C1STATSET : longWord absolute ($BF805318);
  I2C1STATINV : longWord absolute ($BF80531C);
  I2C1ADD : longWord absolute ($BF805320);
  I2C1ADDCLR : longWord absolute ($BF805324);
  I2C1ADDSET : longWord absolute ($BF805328);
  I2C1ADDINV : longWord absolute ($BF80532C);
  I2C1MSK : longWord absolute ($BF805330);
  I2C1MSKCLR : longWord absolute ($BF805334);
  I2C1MSKSET : longWord absolute ($BF805338);
  I2C1MSKINV : longWord absolute ($BF80533C);
  I2C1BRG : longWord absolute ($BF805340);
  I2C1BRGCLR : longWord absolute ($BF805344);
  I2C1BRGSET : longWord absolute ($BF805348);
  I2C1BRGINV : longWord absolute ($BF80534C);
  I2C1TRN : longWord absolute ($BF805350);
  I2C1TRNCLR : longWord absolute ($BF805354);
  I2C1TRNSET : longWord absolute ($BF805358);
  I2C1TRNINV : longWord absolute ($BF80535C);
  I2C1RCV : longWord absolute ($BF805360);
  I2C2CON : longWord absolute ($BF805400);
type
  TI2C2CONbits = bitpacked record
  case integer of
  0 : (
    SEN : 0..1;
    RSEN : 0..1;
    PEN : 0..1;
    RCEN : 0..1;
    ACKEN : 0..1;
    ACKDT : 0..1;
    STREN : 0..1;
    GCEN : 0..1;
    SMEN : 0..1;
    DISSLW : 0..1;
    A10M : 0..1;
    STRICT : 0..1;
    SCLREL : 0..1;
    SIDL : 0..1;
    RESERVED0 : 0..1;
    ON : 0..1;
  );
  1 : (
    RESERVED1 : 0..2047;
    IPMIEN : 0..1;
    RESERVED2 : 0..1;
    I2CSIDL : 0..1;
    RESERVED3 : 0..1;
    I2CEN : 0..1;
  );
  2 : (
    w : 0..4294967295;
  );
  end;
var
  I2C2CONbits: TI2C2CONbits absolute ($BF805400);
  I2C2CONCLR : longWord absolute ($BF805404);
  I2C2CONSET : longWord absolute ($BF805408);
  I2C2CONINV : longWord absolute ($BF80540C);
  I2C2STAT : longWord absolute ($BF805410);
type
  TI2C2STATbits = bitpacked record
  case integer of
  0 : (
    TBF : 0..1;
    RBF : 0..1;
    R_W : 0..1;
    S : 0..1;
    P : 0..1;
    D_A : 0..1;
    I2COV : 0..1;
    IWCOL : 0..1;
    ADD10 : 0..1;
    GCSTAT : 0..1;
    BCL : 0..1;
    RESERVED0 : 0..7;
    TRSTAT : 0..1;
    ACKSTAT : 0..1;
  );
  1 : (
    RESERVED1 : 0..63;
    I2CPOV : 0..1;
  );
  2 : (
    w : 0..4294967295;
  );
  end;
var
  I2C2STATbits: TI2C2STATbits absolute ($BF805410);
  I2C2STATCLR : longWord absolute ($BF805414);
  I2C2STATSET : longWord absolute ($BF805418);
  I2C2STATINV : longWord absolute ($BF80541C);
  I2C2ADD : longWord absolute ($BF805420);
  I2C2ADDCLR : longWord absolute ($BF805424);
  I2C2ADDSET : longWord absolute ($BF805428);
  I2C2ADDINV : longWord absolute ($BF80542C);
  I2C2MSK : longWord absolute ($BF805430);
  I2C2MSKCLR : longWord absolute ($BF805434);
  I2C2MSKSET : longWord absolute ($BF805438);
  I2C2MSKINV : longWord absolute ($BF80543C);
  I2C2BRG : longWord absolute ($BF805440);
  I2C2BRGCLR : longWord absolute ($BF805444);
  I2C2BRGSET : longWord absolute ($BF805448);
  I2C2BRGINV : longWord absolute ($BF80544C);
  I2C2TRN : longWord absolute ($BF805450);
  I2C2TRNCLR : longWord absolute ($BF805454);
  I2C2TRNSET : longWord absolute ($BF805458);
  I2C2TRNINV : longWord absolute ($BF80545C);
  I2C2RCV : longWord absolute ($BF805460);
  SPI1ACON : longWord absolute ($BF805800);
type
  TSPI1ACONbits = bitpacked record
  case integer of
  0 : (
    SRXISEL : 0..3;
    STXISEL : 0..3;
    RESERVED0 : 0..1;
    MSTEN : 0..1;
    CKP : 0..1;
    SSEN : 0..1;
    CKE : 0..1;
    SMP : 0..1;
    MODE16 : 0..1;
    MODE32 : 0..1;
    DISSDO : 0..1;
    SIDL : 0..1;
    RESERVED1 : 0..1;
    ON : 0..1;
    ENHBUF : 0..1;
    SPIFE : 0..1;
    RESERVED2 : 0..63;
    FRMCNT : 0..7;
    FRMSYPW : 0..1;
    MSSEN : 0..1;
    FRMPOL : 0..1;
    FRMSYNC : 0..1;
    FRMEN : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  SPI1ACONbits: TSPI1ACONbits absolute ($BF805800);
  SPI3CON : longWord absolute ($BF805800);
type
  TSPI3CONbits = bitpacked record
  case integer of
  0 : (
    SRXISEL : 0..3;
    STXISEL : 0..3;
    RESERVED0 : 0..1;
    MSTEN : 0..1;
    CKP : 0..1;
    SSEN : 0..1;
    CKE : 0..1;
    SMP : 0..1;
    MODE16 : 0..1;
    MODE32 : 0..1;
    DISSDO : 0..1;
    SIDL : 0..1;
    RESERVED1 : 0..1;
    ON : 0..1;
    ENHBUF : 0..1;
    SPIFE : 0..1;
    RESERVED2 : 0..63;
    FRMCNT : 0..7;
    FRMSYPW : 0..1;
    MSSEN : 0..1;
    FRMPOL : 0..1;
    FRMSYNC : 0..1;
    FRMEN : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  SPI3CONbits: TSPI3CONbits absolute ($BF805800);
  SPI1ACONCLR : longWord absolute ($BF805804);
  SPI3CONCLR : longWord absolute ($BF805804);
  SPI1ACONSET : longWord absolute ($BF805808);
  SPI3CONSET : longWord absolute ($BF805808);
  SPI1ACONINV : longWord absolute ($BF80580C);
  SPI3CONINV : longWord absolute ($BF80580C);
  SPI1ASTAT : longWord absolute ($BF805810);
type
  TSPI1ASTATbits = bitpacked record
  case integer of
  0 : (
    SPIRBF : 0..1;
    SPITBF : 0..1;
    RESERVED0 : 0..1;
    SPITBE : 0..1;
    RESERVED1 : 0..1;
    SPIRBE : 0..1;
    SPIROV : 0..1;
    SRMT : 0..1;
    SPITUR : 0..1;
    RESERVED2 : 0..3;
    SPIBUSY : 0..1;
    RESERVED3 : 0..15;
    TXBUFELM : 0..31;
    RESERVED4 : 0..7;
    RXBUFELM : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  SPI1ASTATbits: TSPI1ASTATbits absolute ($BF805810);
  SPI3STAT : longWord absolute ($BF805810);
type
  TSPI3STATbits = bitpacked record
  case integer of
  0 : (
    SPIRBF : 0..1;
    SPITBF : 0..1;
    RESERVED0 : 0..1;
    SPITBE : 0..1;
    RESERVED1 : 0..1;
    SPIRBE : 0..1;
    SPIROV : 0..1;
    SRMT : 0..1;
    SPITUR : 0..1;
    RESERVED2 : 0..3;
    SPIBUSY : 0..1;
    RESERVED3 : 0..15;
    TXBUFELM : 0..31;
    RESERVED4 : 0..7;
    RXBUFELM : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  SPI3STATbits: TSPI3STATbits absolute ($BF805810);
  SPI1ASTATCLR : longWord absolute ($BF805814);
  SPI3STATCLR : longWord absolute ($BF805814);
  SPI1ASTATSET : longWord absolute ($BF805818);
  SPI3STATSET : longWord absolute ($BF805818);
  SPI1ASTATINV : longWord absolute ($BF80581C);
  SPI3STATINV : longWord absolute ($BF80581C);
  SPI1ABUF : longWord absolute ($BF805820);
  SPI3BUF : longWord absolute ($BF805820);
  SPI1ABRG : longWord absolute ($BF805830);
  SPI3BRG : longWord absolute ($BF805830);
  SPI1ABRGCLR : longWord absolute ($BF805834);
  SPI3BRGCLR : longWord absolute ($BF805834);
  SPI1ABRGSET : longWord absolute ($BF805838);
  SPI3BRGSET : longWord absolute ($BF805838);
  SPI1ABRGINV : longWord absolute ($BF80583C);
  SPI3BRGINV : longWord absolute ($BF80583C);
  SPI2ACON : longWord absolute ($BF805A00);
type
  TSPI2ACONbits = bitpacked record
  case integer of
  0 : (
    SRXISEL : 0..3;
    STXISEL : 0..3;
    RESERVED0 : 0..1;
    MSTEN : 0..1;
    CKP : 0..1;
    SSEN : 0..1;
    CKE : 0..1;
    SMP : 0..1;
    MODE16 : 0..1;
    MODE32 : 0..1;
    DISSDO : 0..1;
    SIDL : 0..1;
    RESERVED1 : 0..1;
    ON : 0..1;
    ENHBUF : 0..1;
    SPIFE : 0..1;
    RESERVED2 : 0..63;
    FRMCNT : 0..7;
    FRMSYPW : 0..1;
    MSSEN : 0..1;
    FRMPOL : 0..1;
    FRMSYNC : 0..1;
    FRMEN : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  SPI2ACONbits: TSPI2ACONbits absolute ($BF805A00);
  SPI2CON : longWord absolute ($BF805A00);
type
  TSPI2CONbits = bitpacked record
  case integer of
  0 : (
    SRXISEL : 0..3;
    STXISEL : 0..3;
    RESERVED0 : 0..1;
    MSTEN : 0..1;
    CKP : 0..1;
    SSEN : 0..1;
    CKE : 0..1;
    SMP : 0..1;
    MODE16 : 0..1;
    MODE32 : 0..1;
    DISSDO : 0..1;
    SIDL : 0..1;
    RESERVED1 : 0..1;
    ON : 0..1;
    ENHBUF : 0..1;
    SPIFE : 0..1;
    RESERVED2 : 0..63;
    FRMCNT : 0..7;
    FRMSYPW : 0..1;
    MSSEN : 0..1;
    FRMPOL : 0..1;
    FRMSYNC : 0..1;
    FRMEN : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  SPI2CONbits: TSPI2CONbits absolute ($BF805A00);
  SPI2ACONCLR : longWord absolute ($BF805A04);
  SPI2CONCLR : longWord absolute ($BF805A04);
  SPI2ACONSET : longWord absolute ($BF805A08);
  SPI2CONSET : longWord absolute ($BF805A08);
  SPI2ACONINV : longWord absolute ($BF805A0C);
  SPI2CONINV : longWord absolute ($BF805A0C);
  SPI2ASTAT : longWord absolute ($BF805A10);
type
  TSPI2ASTATbits = bitpacked record
  case integer of
  0 : (
    SPIRBF : 0..1;
    SPITBF : 0..1;
    RESERVED0 : 0..1;
    SPITBE : 0..1;
    RESERVED1 : 0..1;
    SPIRBE : 0..1;
    SPIROV : 0..1;
    SRMT : 0..1;
    SPITUR : 0..1;
    RESERVED2 : 0..3;
    SPIBUSY : 0..1;
    RESERVED3 : 0..15;
    TXBUFELM : 0..31;
    RESERVED4 : 0..7;
    RXBUFELM : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  SPI2ASTATbits: TSPI2ASTATbits absolute ($BF805A10);
  SPI2STAT : longWord absolute ($BF805A10);
type
  TSPI2STATbits = bitpacked record
  case integer of
  0 : (
    SPIRBF : 0..1;
    SPITBF : 0..1;
    RESERVED0 : 0..1;
    SPITBE : 0..1;
    RESERVED1 : 0..1;
    SPIRBE : 0..1;
    SPIROV : 0..1;
    SRMT : 0..1;
    SPITUR : 0..1;
    RESERVED2 : 0..3;
    SPIBUSY : 0..1;
    RESERVED3 : 0..15;
    TXBUFELM : 0..31;
    RESERVED4 : 0..7;
    RXBUFELM : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  SPI2STATbits: TSPI2STATbits absolute ($BF805A10);
  SPI2ASTATCLR : longWord absolute ($BF805A14);
  SPI2STATCLR : longWord absolute ($BF805A14);
  SPI2ASTATSET : longWord absolute ($BF805A18);
  SPI2STATSET : longWord absolute ($BF805A18);
  SPI2ASTATINV : longWord absolute ($BF805A1C);
  SPI2STATINV : longWord absolute ($BF805A1C);
  SPI2ABUF : longWord absolute ($BF805A20);
  SPI2BUF : longWord absolute ($BF805A20);
  SPI2ABRG : longWord absolute ($BF805A30);
  SPI2BRG : longWord absolute ($BF805A30);
  SPI2ABRGCLR : longWord absolute ($BF805A34);
  SPI2BRGCLR : longWord absolute ($BF805A34);
  SPI2ABRGSET : longWord absolute ($BF805A38);
  SPI2BRGSET : longWord absolute ($BF805A38);
  SPI2ABRGINV : longWord absolute ($BF805A3C);
  SPI2BRGINV : longWord absolute ($BF805A3C);
  SPI3ACON : longWord absolute ($BF805C00);
type
  TSPI3ACONbits = bitpacked record
  case integer of
  0 : (
    SRXISEL : 0..3;
    STXISEL : 0..3;
    RESERVED0 : 0..1;
    MSTEN : 0..1;
    CKP : 0..1;
    SSEN : 0..1;
    CKE : 0..1;
    SMP : 0..1;
    MODE16 : 0..1;
    MODE32 : 0..1;
    DISSDO : 0..1;
    SIDL : 0..1;
    RESERVED1 : 0..1;
    ON : 0..1;
    ENHBUF : 0..1;
    SPIFE : 0..1;
    RESERVED2 : 0..63;
    FRMCNT : 0..7;
    FRMSYPW : 0..1;
    MSSEN : 0..1;
    FRMPOL : 0..1;
    FRMSYNC : 0..1;
    FRMEN : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  SPI3ACONbits: TSPI3ACONbits absolute ($BF805C00);
  SPI4CON : longWord absolute ($BF805C00);
type
  TSPI4CONbits = bitpacked record
  case integer of
  0 : (
    SRXISEL : 0..3;
    STXISEL : 0..3;
    RESERVED0 : 0..1;
    MSTEN : 0..1;
    CKP : 0..1;
    SSEN : 0..1;
    CKE : 0..1;
    SMP : 0..1;
    MODE16 : 0..1;
    MODE32 : 0..1;
    DISSDO : 0..1;
    SIDL : 0..1;
    RESERVED1 : 0..1;
    ON : 0..1;
    ENHBUF : 0..1;
    SPIFE : 0..1;
    RESERVED2 : 0..63;
    FRMCNT : 0..7;
    FRMSYPW : 0..1;
    MSSEN : 0..1;
    FRMPOL : 0..1;
    FRMSYNC : 0..1;
    FRMEN : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  SPI4CONbits: TSPI4CONbits absolute ($BF805C00);
  SPI3ACONCLR : longWord absolute ($BF805C04);
  SPI4CONCLR : longWord absolute ($BF805C04);
  SPI3ACONSET : longWord absolute ($BF805C08);
  SPI4CONSET : longWord absolute ($BF805C08);
  SPI3ACONINV : longWord absolute ($BF805C0C);
  SPI4CONINV : longWord absolute ($BF805C0C);
  SPI3ASTAT : longWord absolute ($BF805C10);
type
  TSPI3ASTATbits = bitpacked record
  case integer of
  0 : (
    SPIRBF : 0..1;
    SPITBF : 0..1;
    RESERVED0 : 0..1;
    SPITBE : 0..1;
    RESERVED1 : 0..1;
    SPIRBE : 0..1;
    SPIROV : 0..1;
    SRMT : 0..1;
    SPITUR : 0..1;
    RESERVED2 : 0..3;
    SPIBUSY : 0..1;
    RESERVED3 : 0..15;
    TXBUFELM : 0..31;
    RESERVED4 : 0..7;
    RXBUFELM : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  SPI3ASTATbits: TSPI3ASTATbits absolute ($BF805C10);
  SPI4STAT : longWord absolute ($BF805C10);
type
  TSPI4STATbits = bitpacked record
  case integer of
  0 : (
    SPIRBF : 0..1;
    SPITBF : 0..1;
    RESERVED0 : 0..1;
    SPITBE : 0..1;
    RESERVED1 : 0..1;
    SPIRBE : 0..1;
    SPIROV : 0..1;
    SRMT : 0..1;
    SPITUR : 0..1;
    RESERVED2 : 0..3;
    SPIBUSY : 0..1;
    RESERVED3 : 0..15;
    TXBUFELM : 0..31;
    RESERVED4 : 0..7;
    RXBUFELM : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  SPI4STATbits: TSPI4STATbits absolute ($BF805C10);
  SPI3ASTATCLR : longWord absolute ($BF805C14);
  SPI4STATCLR : longWord absolute ($BF805C14);
  SPI3ASTATSET : longWord absolute ($BF805C18);
  SPI4STATSET : longWord absolute ($BF805C18);
  SPI3ASTATINV : longWord absolute ($BF805C1C);
  SPI4STATINV : longWord absolute ($BF805C1C);
  SPI3ABUF : longWord absolute ($BF805C20);
  SPI4BUF : longWord absolute ($BF805C20);
  SPI3ABRG : longWord absolute ($BF805C30);
  SPI4BRG : longWord absolute ($BF805C30);
  SPI3ABRGCLR : longWord absolute ($BF805C34);
  SPI4BRGCLR : longWord absolute ($BF805C34);
  SPI3ABRGSET : longWord absolute ($BF805C38);
  SPI4BRGSET : longWord absolute ($BF805C38);
  SPI3ABRGINV : longWord absolute ($BF805C3C);
  SPI4BRGINV : longWord absolute ($BF805C3C);
  SPI1CON : longWord absolute ($BF805E00);
type
  TSPI1CONbits = bitpacked record
  case integer of
  0 : (
    SRXISEL : 0..3;
    STXISEL : 0..3;
    RESERVED0 : 0..1;
    MSTEN : 0..1;
    CKP : 0..1;
    SSEN : 0..1;
    CKE : 0..1;
    SMP : 0..1;
    MODE16 : 0..1;
    MODE32 : 0..1;
    DISSDO : 0..1;
    SIDL : 0..1;
    RESERVED1 : 0..1;
    ON : 0..1;
    ENHBUF : 0..1;
    SPIFE : 0..1;
    RESERVED2 : 0..63;
    FRMCNT : 0..7;
    FRMSYPW : 0..1;
    MSSEN : 0..1;
    FRMPOL : 0..1;
    FRMSYNC : 0..1;
    FRMEN : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  SPI1CONbits: TSPI1CONbits absolute ($BF805E00);
  SPI1CONCLR : longWord absolute ($BF805E04);
  SPI1CONSET : longWord absolute ($BF805E08);
  SPI1CONINV : longWord absolute ($BF805E0C);
  SPI1STAT : longWord absolute ($BF805E10);
type
  TSPI1STATbits = bitpacked record
  case integer of
  0 : (
    SPIRBF : 0..1;
    SPITBF : 0..1;
    RESERVED0 : 0..1;
    SPITBE : 0..1;
    RESERVED1 : 0..1;
    SPIRBE : 0..1;
    SPIROV : 0..1;
    SRMT : 0..1;
    SPITUR : 0..1;
    RESERVED2 : 0..3;
    SPIBUSY : 0..1;
    RESERVED3 : 0..15;
    TXBUFELM : 0..31;
    RESERVED4 : 0..7;
    RXBUFELM : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  SPI1STATbits: TSPI1STATbits absolute ($BF805E10);
  SPI1STATCLR : longWord absolute ($BF805E14);
  SPI1STATSET : longWord absolute ($BF805E18);
  SPI1STATINV : longWord absolute ($BF805E1C);
  SPI1BUF : longWord absolute ($BF805E20);
  SPI1BRG : longWord absolute ($BF805E30);
  SPI1BRGCLR : longWord absolute ($BF805E34);
  SPI1BRGSET : longWord absolute ($BF805E38);
  SPI1BRGINV : longWord absolute ($BF805E3C);
  U1AMODE : longWord absolute ($BF806000);
type
  TU1AMODEbits = bitpacked record
  case integer of
  0 : (
    STSEL : 0..1;
    PDSEL : 0..3;
    BRGH : 0..1;
    RXINV : 0..1;
    ABAUD : 0..1;
    LPBACK : 0..1;
    WAKE : 0..1;
    UEN : 0..3;
    RESERVED0 : 0..1;
    RTSMD : 0..1;
    IREN : 0..1;
    SIDL : 0..1;
    RESERVED1 : 0..1;
    ON : 0..1;
  );
  1 : (
    RESERVED2 : 0..1;
    PDSEL0 : 0..1;
    PDSEL1 : 0..1;
    RESERVED3 : 0..31;
    UEN0 : 0..1;
    UEN1 : 0..1;
  );
  2 : (
    RESERVED4 : 0..8191;
    USIDL : 0..1;
    RESERVED5 : 0..1;
    UARTEN : 0..1;
  );
  3 : (
    w : 0..4294967295;
  );
  end;
var
  U1AMODEbits: TU1AMODEbits absolute ($BF806000);
  U1MODE : longWord absolute ($BF806000);
type
  TU1MODEbits = bitpacked record
  case integer of
  0 : (
    STSEL : 0..1;
    PDSEL : 0..3;
    BRGH : 0..1;
    RXINV : 0..1;
    ABAUD : 0..1;
    LPBACK : 0..1;
    WAKE : 0..1;
    UEN : 0..3;
    RESERVED0 : 0..1;
    RTSMD : 0..1;
    IREN : 0..1;
    SIDL : 0..1;
    RESERVED1 : 0..1;
    ON : 0..1;
  );
  1 : (
    RESERVED2 : 0..1;
    PDSEL0 : 0..1;
    PDSEL1 : 0..1;
    RESERVED3 : 0..31;
    UEN0 : 0..1;
    UEN1 : 0..1;
  );
  2 : (
    RESERVED4 : 0..8191;
    USIDL : 0..1;
    RESERVED5 : 0..1;
    UARTEN : 0..1;
  );
  3 : (
    w : 0..4294967295;
  );
  end;
var
  U1MODEbits: TU1MODEbits absolute ($BF806000);
  U1AMODECLR : longWord absolute ($BF806004);
  U1MODECLR : longWord absolute ($BF806004);
  U1AMODESET : longWord absolute ($BF806008);
  U1MODESET : longWord absolute ($BF806008);
  U1AMODEINV : longWord absolute ($BF80600C);
  U1MODEINV : longWord absolute ($BF80600C);
  U1ASTA : longWord absolute ($BF806010);
type
  TU1ASTAbits = bitpacked record
  case integer of
  0 : (
    URXDA : 0..1;
    OERR : 0..1;
    FERR : 0..1;
    PERR : 0..1;
    RIDLE : 0..1;
    ADDEN : 0..1;
    URXISEL : 0..3;
    TRMT : 0..1;
    UTXBF : 0..1;
    UTXEN : 0..1;
    UTXBRK : 0..1;
    URXEN : 0..1;
    UTXINV : 0..1;
    UTXISEL : 0..3;
    ADDR : 0..255;
    ADM_EN : 0..1;
  );
  1 : (
    RESERVED0 : 0..63;
    URXISEL0 : 0..1;
    URXISEL1 : 0..1;
    RESERVED1 : 0..63;
    UTXISEL0 : 0..1;
    UTXISEL1 : 0..1;
  );
  2 : (
    RESERVED2 : 0..16383;
    UTXSEL : 0..3;
  );
  3 : (
    w : 0..4294967295;
  );
  end;
var
  U1ASTAbits: TU1ASTAbits absolute ($BF806010);
  U1STA : longWord absolute ($BF806010);
type
  TU1STAbits = bitpacked record
  case integer of
  0 : (
    URXDA : 0..1;
    OERR : 0..1;
    FERR : 0..1;
    PERR : 0..1;
    RIDLE : 0..1;
    ADDEN : 0..1;
    URXISEL : 0..3;
    TRMT : 0..1;
    UTXBF : 0..1;
    UTXEN : 0..1;
    UTXBRK : 0..1;
    URXEN : 0..1;
    UTXINV : 0..1;
    UTXISEL : 0..3;
    ADDR : 0..255;
    ADM_EN : 0..1;
  );
  1 : (
    RESERVED0 : 0..63;
    URXISEL0 : 0..1;
    URXISEL1 : 0..1;
    RESERVED1 : 0..63;
    UTXISEL0 : 0..1;
    UTXISEL1 : 0..1;
  );
  2 : (
    RESERVED2 : 0..16383;
    UTXSEL : 0..3;
  );
  3 : (
    w : 0..4294967295;
  );
  end;
var
  U1STAbits: TU1STAbits absolute ($BF806010);
  U1ASTACLR : longWord absolute ($BF806014);
  U1STACLR : longWord absolute ($BF806014);
  U1ASTASET : longWord absolute ($BF806018);
  U1STASET : longWord absolute ($BF806018);
  U1ASTAINV : longWord absolute ($BF80601C);
  U1STAINV : longWord absolute ($BF80601C);
  U1ATXREG : longWord absolute ($BF806020);
  U1TXREG : longWord absolute ($BF806020);
  U1ARXREG : longWord absolute ($BF806030);
  U1RXREG : longWord absolute ($BF806030);
  U1ABRG : longWord absolute ($BF806040);
  U1BRG : longWord absolute ($BF806040);
  U1ABRGCLR : longWord absolute ($BF806044);
  U1BRGCLR : longWord absolute ($BF806044);
  U1ABRGSET : longWord absolute ($BF806048);
  U1BRGSET : longWord absolute ($BF806048);
  U1ABRGINV : longWord absolute ($BF80604C);
  U1BRGINV : longWord absolute ($BF80604C);
  U1BMODE : longWord absolute ($BF806200);
type
  TU1BMODEbits = bitpacked record
  case integer of
  0 : (
    STSEL : 0..1;
    PDSEL : 0..3;
    BRGH : 0..1;
    RXINV : 0..1;
    ABAUD : 0..1;
    LPBACK : 0..1;
    WAKE : 0..1;
    RESERVED0 : 0..15;
    IREN : 0..1;
    SIDL : 0..1;
    RESERVED1 : 0..1;
    ON : 0..1;
  );
  1 : (
    RESERVED2 : 0..1;
    PDSEL0 : 0..1;
    PDSEL1 : 0..1;
  );
  2 : (
    RESERVED3 : 0..8191;
    USIDL : 0..1;
    RESERVED4 : 0..1;
    UARTEN : 0..1;
  );
  3 : (
    w : 0..4294967295;
  );
  end;
var
  U1BMODEbits: TU1BMODEbits absolute ($BF806200);
  U4MODE : longWord absolute ($BF806200);
type
  TU4MODEbits = bitpacked record
  case integer of
  0 : (
    STSEL : 0..1;
    PDSEL : 0..3;
    BRGH : 0..1;
    RXINV : 0..1;
    ABAUD : 0..1;
    LPBACK : 0..1;
    WAKE : 0..1;
    RESERVED0 : 0..15;
    IREN : 0..1;
    SIDL : 0..1;
    RESERVED1 : 0..1;
    ON : 0..1;
  );
  1 : (
    RESERVED2 : 0..1;
    PDSEL0 : 0..1;
    PDSEL1 : 0..1;
  );
  2 : (
    RESERVED3 : 0..8191;
    USIDL : 0..1;
    RESERVED4 : 0..1;
    UARTEN : 0..1;
  );
  3 : (
    w : 0..4294967295;
  );
  end;
var
  U4MODEbits: TU4MODEbits absolute ($BF806200);
  U1BMODECLR : longWord absolute ($BF806204);
  U4MODECLR : longWord absolute ($BF806204);
  U1BMODESET : longWord absolute ($BF806208);
  U4MODESET : longWord absolute ($BF806208);
  U1BMODEINV : longWord absolute ($BF80620C);
  U4MODEINV : longWord absolute ($BF80620C);
  U1BSTA : longWord absolute ($BF806210);
type
  TU1BSTAbits = bitpacked record
  case integer of
  0 : (
    URXDA : 0..1;
    OERR : 0..1;
    FERR : 0..1;
    PERR : 0..1;
    RIDLE : 0..1;
    ADDEN : 0..1;
    URXISEL : 0..3;
    TRMT : 0..1;
    UTXBF : 0..1;
    UTXEN : 0..1;
    UTXBRK : 0..1;
    URXEN : 0..1;
    UTXINV : 0..1;
    UTXISEL : 0..3;
    ADDR : 0..255;
    ADM_EN : 0..1;
  );
  1 : (
    RESERVED0 : 0..63;
    URXISEL0 : 0..1;
    URXISEL1 : 0..1;
    RESERVED1 : 0..63;
    UTXISEL0 : 0..1;
    UTXISEL1 : 0..1;
  );
  2 : (
    RESERVED2 : 0..16383;
    UTXSEL : 0..3;
  );
  3 : (
    w : 0..4294967295;
  );
  end;
var
  U1BSTAbits: TU1BSTAbits absolute ($BF806210);
  U4STA : longWord absolute ($BF806210);
type
  TU4STAbits = bitpacked record
  case integer of
  0 : (
    URXDA : 0..1;
    OERR : 0..1;
    FERR : 0..1;
    PERR : 0..1;
    RIDLE : 0..1;
    ADDEN : 0..1;
    URXISEL : 0..3;
    TRMT : 0..1;
    UTXBF : 0..1;
    UTXEN : 0..1;
    UTXBRK : 0..1;
    URXEN : 0..1;
    UTXINV : 0..1;
    UTXISEL : 0..3;
    ADDR : 0..255;
    ADM_EN : 0..1;
  );
  1 : (
    RESERVED0 : 0..63;
    URXISEL0 : 0..1;
    URXISEL1 : 0..1;
    RESERVED1 : 0..63;
    UTXISEL0 : 0..1;
    UTXISEL1 : 0..1;
  );
  2 : (
    RESERVED2 : 0..16383;
    UTXSEL : 0..3;
  );
  3 : (
    w : 0..4294967295;
  );
  end;
var
  U4STAbits: TU4STAbits absolute ($BF806210);
  U1BSTACLR : longWord absolute ($BF806214);
  U4STACLR : longWord absolute ($BF806214);
  U1BSTASET : longWord absolute ($BF806218);
  U4STASET : longWord absolute ($BF806218);
  U1BSTAINV : longWord absolute ($BF80621C);
  U4STAINV : longWord absolute ($BF80621C);
  U1BTXREG : longWord absolute ($BF806220);
  U4TXREG : longWord absolute ($BF806220);
  U1BRXREG : longWord absolute ($BF806230);
  U4RXREG : longWord absolute ($BF806230);
  U1BBRG : longWord absolute ($BF806240);
  U4BRG : longWord absolute ($BF806240);
  U1BBRGCLR : longWord absolute ($BF806244);
  U4BRGCLR : longWord absolute ($BF806244);
  U1BBRGSET : longWord absolute ($BF806248);
  U4BRGSET : longWord absolute ($BF806248);
  U1BBRGINV : longWord absolute ($BF80624C);
  U4BRGINV : longWord absolute ($BF80624C);
  U2AMODE : longWord absolute ($BF806400);
type
  TU2AMODEbits = bitpacked record
  case integer of
  0 : (
    STSEL : 0..1;
    PDSEL : 0..3;
    BRGH : 0..1;
    RXINV : 0..1;
    ABAUD : 0..1;
    LPBACK : 0..1;
    WAKE : 0..1;
    UEN : 0..3;
    RESERVED0 : 0..1;
    RTSMD : 0..1;
    IREN : 0..1;
    SIDL : 0..1;
    RESERVED1 : 0..1;
    ON : 0..1;
  );
  1 : (
    RESERVED2 : 0..1;
    PDSEL0 : 0..1;
    PDSEL1 : 0..1;
    RESERVED3 : 0..31;
    UEN0 : 0..1;
    UEN1 : 0..1;
  );
  2 : (
    RESERVED4 : 0..8191;
    USIDL : 0..1;
    RESERVED5 : 0..1;
    UARTEN : 0..1;
  );
  3 : (
    w : 0..4294967295;
  );
  end;
var
  U2AMODEbits: TU2AMODEbits absolute ($BF806400);
  U3MODE : longWord absolute ($BF806400);
type
  TU3MODEbits = bitpacked record
  case integer of
  0 : (
    STSEL : 0..1;
    PDSEL : 0..3;
    BRGH : 0..1;
    RXINV : 0..1;
    ABAUD : 0..1;
    LPBACK : 0..1;
    WAKE : 0..1;
    UEN : 0..3;
    RESERVED0 : 0..1;
    RTSMD : 0..1;
    IREN : 0..1;
    SIDL : 0..1;
    RESERVED1 : 0..1;
    ON : 0..1;
  );
  1 : (
    RESERVED2 : 0..1;
    PDSEL0 : 0..1;
    PDSEL1 : 0..1;
    RESERVED3 : 0..31;
    UEN0 : 0..1;
    UEN1 : 0..1;
  );
  2 : (
    RESERVED4 : 0..8191;
    USIDL : 0..1;
    RESERVED5 : 0..1;
    UARTEN : 0..1;
  );
  3 : (
    w : 0..4294967295;
  );
  end;
var
  U3MODEbits: TU3MODEbits absolute ($BF806400);
  U2AMODECLR : longWord absolute ($BF806404);
  U3MODECLR : longWord absolute ($BF806404);
  U2AMODESET : longWord absolute ($BF806408);
  U3MODESET : longWord absolute ($BF806408);
  U2AMODEINV : longWord absolute ($BF80640C);
  U3MODEINV : longWord absolute ($BF80640C);
  U2ASTA : longWord absolute ($BF806410);
type
  TU2ASTAbits = bitpacked record
  case integer of
  0 : (
    URXDA : 0..1;
    OERR : 0..1;
    FERR : 0..1;
    PERR : 0..1;
    RIDLE : 0..1;
    ADDEN : 0..1;
    URXISEL : 0..3;
    TRMT : 0..1;
    UTXBF : 0..1;
    UTXEN : 0..1;
    UTXBRK : 0..1;
    URXEN : 0..1;
    UTXINV : 0..1;
    UTXISEL : 0..3;
    ADDR : 0..255;
    ADM_EN : 0..1;
  );
  1 : (
    RESERVED0 : 0..63;
    URXISEL0 : 0..1;
    URXISEL1 : 0..1;
    RESERVED1 : 0..63;
    UTXISEL0 : 0..1;
    UTXISEL1 : 0..1;
  );
  2 : (
    RESERVED2 : 0..16383;
    UTXSEL : 0..3;
  );
  3 : (
    w : 0..4294967295;
  );
  end;
var
  U2ASTAbits: TU2ASTAbits absolute ($BF806410);
  U3STA : longWord absolute ($BF806410);
type
  TU3STAbits = bitpacked record
  case integer of
  0 : (
    URXDA : 0..1;
    OERR : 0..1;
    FERR : 0..1;
    PERR : 0..1;
    RIDLE : 0..1;
    ADDEN : 0..1;
    URXISEL : 0..3;
    TRMT : 0..1;
    UTXBF : 0..1;
    UTXEN : 0..1;
    UTXBRK : 0..1;
    URXEN : 0..1;
    UTXINV : 0..1;
    UTXISEL : 0..3;
    ADDR : 0..255;
    ADM_EN : 0..1;
  );
  1 : (
    RESERVED0 : 0..63;
    URXISEL0 : 0..1;
    URXISEL1 : 0..1;
    RESERVED1 : 0..63;
    UTXISEL0 : 0..1;
    UTXISEL1 : 0..1;
  );
  2 : (
    RESERVED2 : 0..16383;
    UTXSEL : 0..3;
  );
  3 : (
    w : 0..4294967295;
  );
  end;
var
  U3STAbits: TU3STAbits absolute ($BF806410);
  U2ASTACLR : longWord absolute ($BF806414);
  U3STACLR : longWord absolute ($BF806414);
  U2ASTASET : longWord absolute ($BF806418);
  U3STASET : longWord absolute ($BF806418);
  U2ASTAINV : longWord absolute ($BF80641C);
  U3STAINV : longWord absolute ($BF80641C);
  U2ATXREG : longWord absolute ($BF806420);
  U3TXREG : longWord absolute ($BF806420);
  U2ARXREG : longWord absolute ($BF806430);
  U3RXREG : longWord absolute ($BF806430);
  U2ABRG : longWord absolute ($BF806440);
  U3BRG : longWord absolute ($BF806440);
  U2ABRGCLR : longWord absolute ($BF806444);
  U3BRGCLR : longWord absolute ($BF806444);
  U2ABRGSET : longWord absolute ($BF806448);
  U3BRGSET : longWord absolute ($BF806448);
  U2ABRGINV : longWord absolute ($BF80644C);
  U3BRGINV : longWord absolute ($BF80644C);
  U2BMODE : longWord absolute ($BF806600);
type
  TU2BMODEbits = bitpacked record
  case integer of
  0 : (
    STSEL : 0..1;
    PDSEL : 0..3;
    BRGH : 0..1;
    RXINV : 0..1;
    ABAUD : 0..1;
    LPBACK : 0..1;
    WAKE : 0..1;
    RESERVED0 : 0..15;
    IREN : 0..1;
    SIDL : 0..1;
    RESERVED1 : 0..1;
    ON : 0..1;
  );
  1 : (
    RESERVED2 : 0..1;
    PDSEL0 : 0..1;
    PDSEL1 : 0..1;
  );
  2 : (
    RESERVED3 : 0..8191;
    USIDL : 0..1;
    RESERVED4 : 0..1;
    UARTEN : 0..1;
  );
  3 : (
    w : 0..4294967295;
  );
  end;
var
  U2BMODEbits: TU2BMODEbits absolute ($BF806600);
  U6MODE : longWord absolute ($BF806600);
type
  TU6MODEbits = bitpacked record
  case integer of
  0 : (
    STSEL : 0..1;
    PDSEL : 0..3;
    BRGH : 0..1;
    RXINV : 0..1;
    ABAUD : 0..1;
    LPBACK : 0..1;
    WAKE : 0..1;
    RESERVED0 : 0..15;
    IREN : 0..1;
    SIDL : 0..1;
    RESERVED1 : 0..1;
    ON : 0..1;
  );
  1 : (
    RESERVED2 : 0..1;
    PDSEL0 : 0..1;
    PDSEL1 : 0..1;
  );
  2 : (
    RESERVED3 : 0..8191;
    USIDL : 0..1;
    RESERVED4 : 0..1;
    UARTEN : 0..1;
  );
  3 : (
    w : 0..4294967295;
  );
  end;
var
  U6MODEbits: TU6MODEbits absolute ($BF806600);
  U2BMODECLR : longWord absolute ($BF806604);
  U6MODECLR : longWord absolute ($BF806604);
  U2BMODESET : longWord absolute ($BF806608);
  U6MODESET : longWord absolute ($BF806608);
  U2BMODEINV : longWord absolute ($BF80660C);
  U6MODEINV : longWord absolute ($BF80660C);
  U2BSTA : longWord absolute ($BF806610);
type
  TU2BSTAbits = bitpacked record
  case integer of
  0 : (
    URXDA : 0..1;
    OERR : 0..1;
    FERR : 0..1;
    PERR : 0..1;
    RIDLE : 0..1;
    ADDEN : 0..1;
    URXISEL : 0..3;
    TRMT : 0..1;
    UTXBF : 0..1;
    UTXEN : 0..1;
    UTXBRK : 0..1;
    URXEN : 0..1;
    UTXINV : 0..1;
    UTXISEL : 0..3;
    ADDR : 0..255;
    ADM_EN : 0..1;
  );
  1 : (
    RESERVED0 : 0..63;
    URXISEL0 : 0..1;
    URXISEL1 : 0..1;
    RESERVED1 : 0..63;
    UTXISEL0 : 0..1;
    UTXISEL1 : 0..1;
  );
  2 : (
    RESERVED2 : 0..16383;
    UTXSEL : 0..3;
  );
  3 : (
    w : 0..4294967295;
  );
  end;
var
  U2BSTAbits: TU2BSTAbits absolute ($BF806610);
  U6STA : longWord absolute ($BF806610);
type
  TU6STAbits = bitpacked record
  case integer of
  0 : (
    URXDA : 0..1;
    OERR : 0..1;
    FERR : 0..1;
    PERR : 0..1;
    RIDLE : 0..1;
    ADDEN : 0..1;
    URXISEL : 0..3;
    TRMT : 0..1;
    UTXBF : 0..1;
    UTXEN : 0..1;
    UTXBRK : 0..1;
    URXEN : 0..1;
    UTXINV : 0..1;
    UTXISEL : 0..3;
    ADDR : 0..255;
    ADM_EN : 0..1;
  );
  1 : (
    RESERVED0 : 0..63;
    URXISEL0 : 0..1;
    URXISEL1 : 0..1;
    RESERVED1 : 0..63;
    UTXISEL0 : 0..1;
    UTXISEL1 : 0..1;
  );
  2 : (
    RESERVED2 : 0..16383;
    UTXSEL : 0..3;
  );
  3 : (
    w : 0..4294967295;
  );
  end;
var
  U6STAbits: TU6STAbits absolute ($BF806610);
  U2BSTACLR : longWord absolute ($BF806614);
  U6STACLR : longWord absolute ($BF806614);
  U2BSTASET : longWord absolute ($BF806618);
  U6STASET : longWord absolute ($BF806618);
  U2BSTAINV : longWord absolute ($BF80661C);
  U6STAINV : longWord absolute ($BF80661C);
  U2BTXREG : longWord absolute ($BF806620);
  U6TXREG : longWord absolute ($BF806620);
  U2BRXREG : longWord absolute ($BF806630);
  U6RXREG : longWord absolute ($BF806630);
  U2BBRG : longWord absolute ($BF806640);
  U6BRG : longWord absolute ($BF806640);
  U2BBRGCLR : longWord absolute ($BF806644);
  U6BRGCLR : longWord absolute ($BF806644);
  U2BBRGSET : longWord absolute ($BF806648);
  U6BRGSET : longWord absolute ($BF806648);
  U2BBRGINV : longWord absolute ($BF80664C);
  U6BRGINV : longWord absolute ($BF80664C);
  U2MODE : longWord absolute ($BF806800);
type
  TU2MODEbits = bitpacked record
  case integer of
  0 : (
    STSEL : 0..1;
    PDSEL : 0..3;
    BRGH : 0..1;
    RXINV : 0..1;
    ABAUD : 0..1;
    LPBACK : 0..1;
    WAKE : 0..1;
    UEN : 0..3;
    RESERVED0 : 0..1;
    RTSMD : 0..1;
    IREN : 0..1;
    SIDL : 0..1;
    RESERVED1 : 0..1;
    ON : 0..1;
  );
  1 : (
    RESERVED2 : 0..1;
    PDSEL0 : 0..1;
    PDSEL1 : 0..1;
    RESERVED3 : 0..31;
    UEN0 : 0..1;
    UEN1 : 0..1;
  );
  2 : (
    RESERVED4 : 0..8191;
    USIDL : 0..1;
    RESERVED5 : 0..1;
    UARTEN : 0..1;
  );
  3 : (
    w : 0..4294967295;
  );
  end;
var
  U2MODEbits: TU2MODEbits absolute ($BF806800);
  U3AMODE : longWord absolute ($BF806800);
type
  TU3AMODEbits = bitpacked record
  case integer of
  0 : (
    STSEL : 0..1;
    PDSEL : 0..3;
    BRGH : 0..1;
    RXINV : 0..1;
    ABAUD : 0..1;
    LPBACK : 0..1;
    WAKE : 0..1;
    UEN : 0..3;
    RESERVED0 : 0..1;
    RTSMD : 0..1;
    IREN : 0..1;
    SIDL : 0..1;
    RESERVED1 : 0..1;
    ON : 0..1;
  );
  1 : (
    RESERVED2 : 0..1;
    PDSEL0 : 0..1;
    PDSEL1 : 0..1;
    RESERVED3 : 0..31;
    UEN0 : 0..1;
    UEN1 : 0..1;
  );
  2 : (
    RESERVED4 : 0..8191;
    USIDL : 0..1;
    RESERVED5 : 0..1;
    UARTEN : 0..1;
  );
  3 : (
    w : 0..4294967295;
  );
  end;
var
  U3AMODEbits: TU3AMODEbits absolute ($BF806800);
  U2MODECLR : longWord absolute ($BF806804);
  U3AMODECLR : longWord absolute ($BF806804);
  U2MODESET : longWord absolute ($BF806808);
  U3AMODESET : longWord absolute ($BF806808);
  U2MODEINV : longWord absolute ($BF80680C);
  U3AMODEINV : longWord absolute ($BF80680C);
  U2STA : longWord absolute ($BF806810);
type
  TU2STAbits = bitpacked record
  case integer of
  0 : (
    URXDA : 0..1;
    OERR : 0..1;
    FERR : 0..1;
    PERR : 0..1;
    RIDLE : 0..1;
    ADDEN : 0..1;
    URXISEL : 0..3;
    TRMT : 0..1;
    UTXBF : 0..1;
    UTXEN : 0..1;
    UTXBRK : 0..1;
    URXEN : 0..1;
    UTXINV : 0..1;
    UTXISEL : 0..3;
    ADDR : 0..255;
    ADM_EN : 0..1;
  );
  1 : (
    RESERVED0 : 0..63;
    URXISEL0 : 0..1;
    URXISEL1 : 0..1;
    RESERVED1 : 0..63;
    UTXISEL0 : 0..1;
    UTXISEL1 : 0..1;
  );
  2 : (
    RESERVED2 : 0..16383;
    UTXSEL : 0..3;
  );
  3 : (
    w : 0..4294967295;
  );
  end;
var
  U2STAbits: TU2STAbits absolute ($BF806810);
  U3ASTA : longWord absolute ($BF806810);
type
  TU3ASTAbits = bitpacked record
  case integer of
  0 : (
    URXDA : 0..1;
    OERR : 0..1;
    FERR : 0..1;
    PERR : 0..1;
    RIDLE : 0..1;
    ADDEN : 0..1;
    URXISEL : 0..3;
    TRMT : 0..1;
    UTXBF : 0..1;
    UTXEN : 0..1;
    UTXBRK : 0..1;
    URXEN : 0..1;
    UTXINV : 0..1;
    UTXISEL : 0..3;
    ADDR : 0..255;
    ADM_EN : 0..1;
  );
  1 : (
    RESERVED0 : 0..63;
    URXISEL0 : 0..1;
    URXISEL1 : 0..1;
    RESERVED1 : 0..63;
    UTXISEL0 : 0..1;
    UTXISEL1 : 0..1;
  );
  2 : (
    RESERVED2 : 0..16383;
    UTXSEL : 0..3;
  );
  3 : (
    w : 0..4294967295;
  );
  end;
var
  U3ASTAbits: TU3ASTAbits absolute ($BF806810);
  U2STACLR : longWord absolute ($BF806814);
  U3ASTACLR : longWord absolute ($BF806814);
  U2STASET : longWord absolute ($BF806818);
  U3ASTASET : longWord absolute ($BF806818);
  U2STAINV : longWord absolute ($BF80681C);
  U3ASTAINV : longWord absolute ($BF80681C);
  U2TXREG : longWord absolute ($BF806820);
  U3ATXREG : longWord absolute ($BF806820);
  U2RXREG : longWord absolute ($BF806830);
  U3ARXREG : longWord absolute ($BF806830);
  U2BRG : longWord absolute ($BF806840);
  U3ABRG : longWord absolute ($BF806840);
  U2BRGCLR : longWord absolute ($BF806844);
  U3ABRGCLR : longWord absolute ($BF806844);
  U2BRGSET : longWord absolute ($BF806848);
  U3ABRGSET : longWord absolute ($BF806848);
  U2BRGINV : longWord absolute ($BF80684C);
  U3ABRGINV : longWord absolute ($BF80684C);
  U3BMODE : longWord absolute ($BF806A00);
type
  TU3BMODEbits = bitpacked record
  case integer of
  0 : (
    STSEL : 0..1;
    PDSEL : 0..3;
    BRGH : 0..1;
    RXINV : 0..1;
    ABAUD : 0..1;
    LPBACK : 0..1;
    WAKE : 0..1;
    RESERVED0 : 0..15;
    IREN : 0..1;
    SIDL : 0..1;
    RESERVED1 : 0..1;
    ON : 0..1;
  );
  1 : (
    RESERVED2 : 0..1;
    PDSEL0 : 0..1;
    PDSEL1 : 0..1;
  );
  2 : (
    RESERVED3 : 0..8191;
    USIDL : 0..1;
    RESERVED4 : 0..1;
    UARTEN : 0..1;
  );
  3 : (
    w : 0..4294967295;
  );
  end;
var
  U3BMODEbits: TU3BMODEbits absolute ($BF806A00);
  U5MODE : longWord absolute ($BF806A00);
type
  TU5MODEbits = bitpacked record
  case integer of
  0 : (
    STSEL : 0..1;
    PDSEL : 0..3;
    BRGH : 0..1;
    RXINV : 0..1;
    ABAUD : 0..1;
    LPBACK : 0..1;
    WAKE : 0..1;
    RESERVED0 : 0..15;
    IREN : 0..1;
    SIDL : 0..1;
    RESERVED1 : 0..1;
    ON : 0..1;
  );
  1 : (
    RESERVED2 : 0..1;
    PDSEL0 : 0..1;
    PDSEL1 : 0..1;
  );
  2 : (
    RESERVED3 : 0..8191;
    USIDL : 0..1;
    RESERVED4 : 0..1;
    UARTEN : 0..1;
  );
  3 : (
    w : 0..4294967295;
  );
  end;
var
  U5MODEbits: TU5MODEbits absolute ($BF806A00);
  U3BMODECLR : longWord absolute ($BF806A04);
  U5MODECLR : longWord absolute ($BF806A04);
  U3BMODESET : longWord absolute ($BF806A08);
  U5MODESET : longWord absolute ($BF806A08);
  U3BMODEINV : longWord absolute ($BF806A0C);
  U5MODEINV : longWord absolute ($BF806A0C);
  U3BSTA : longWord absolute ($BF806A10);
type
  TU3BSTAbits = bitpacked record
  case integer of
  0 : (
    URXDA : 0..1;
    OERR : 0..1;
    FERR : 0..1;
    PERR : 0..1;
    RIDLE : 0..1;
    ADDEN : 0..1;
    URXISEL : 0..3;
    TRMT : 0..1;
    UTXBF : 0..1;
    UTXEN : 0..1;
    UTXBRK : 0..1;
    URXEN : 0..1;
    UTXINV : 0..1;
    UTXISEL : 0..3;
    ADDR : 0..255;
    ADM_EN : 0..1;
  );
  1 : (
    RESERVED0 : 0..63;
    URXISEL0 : 0..1;
    URXISEL1 : 0..1;
    RESERVED1 : 0..63;
    UTXISEL0 : 0..1;
    UTXISEL1 : 0..1;
  );
  2 : (
    RESERVED2 : 0..16383;
    UTXSEL : 0..3;
  );
  3 : (
    w : 0..4294967295;
  );
  end;
var
  U3BSTAbits: TU3BSTAbits absolute ($BF806A10);
  U5STA : longWord absolute ($BF806A10);
type
  TU5STAbits = bitpacked record
  case integer of
  0 : (
    URXDA : 0..1;
    OERR : 0..1;
    FERR : 0..1;
    PERR : 0..1;
    RIDLE : 0..1;
    ADDEN : 0..1;
    URXISEL : 0..3;
    TRMT : 0..1;
    UTXBF : 0..1;
    UTXEN : 0..1;
    UTXBRK : 0..1;
    URXEN : 0..1;
    UTXINV : 0..1;
    UTXISEL : 0..3;
    ADDR : 0..255;
    ADM_EN : 0..1;
  );
  1 : (
    RESERVED0 : 0..63;
    URXISEL0 : 0..1;
    URXISEL1 : 0..1;
    RESERVED1 : 0..63;
    UTXISEL0 : 0..1;
    UTXISEL1 : 0..1;
  );
  2 : (
    RESERVED2 : 0..16383;
    UTXSEL : 0..3;
  );
  3 : (
    w : 0..4294967295;
  );
  end;
var
  U5STAbits: TU5STAbits absolute ($BF806A10);
  U3BSTACLR : longWord absolute ($BF806A14);
  U5STACLR : longWord absolute ($BF806A14);
  U3BSTASET : longWord absolute ($BF806A18);
  U5STASET : longWord absolute ($BF806A18);
  U3BSTAINV : longWord absolute ($BF806A1C);
  U5STAINV : longWord absolute ($BF806A1C);
  U3BTXREG : longWord absolute ($BF806A20);
  U5TXREG : longWord absolute ($BF806A20);
  U3BRXREG : longWord absolute ($BF806A30);
  U5RXREG : longWord absolute ($BF806A30);
  U3BBRG : longWord absolute ($BF806A40);
  U5BRG : longWord absolute ($BF806A40);
  U3BBRGCLR : longWord absolute ($BF806A44);
  U5BRGCLR : longWord absolute ($BF806A44);
  U3BBRGSET : longWord absolute ($BF806A48);
  U5BRGSET : longWord absolute ($BF806A48);
  U3BBRGINV : longWord absolute ($BF806A4C);
  U5BRGINV : longWord absolute ($BF806A4C);
  PMCON : longWord absolute ($BF807000);
type
  TPMCONbits = bitpacked record
  case integer of
  0 : (
    RDSP : 0..1;
    WRSP : 0..1;
    RESERVED0 : 0..1;
    CS1P : 0..1;
    CS2P : 0..1;
    ALP : 0..1;
    CSF : 0..3;
    PTRDEN : 0..1;
    PTWREN : 0..1;
    PMPTTL : 0..1;
    ADRMUX : 0..3;
    SIDL : 0..1;
    RESERVED1 : 0..1;
    ON : 0..1;
  );
  1 : (
    RESERVED2 : 0..63;
    CSF0 : 0..1;
    CSF1 : 0..1;
    RESERVED3 : 0..7;
    ADRMUX0 : 0..1;
    ADRMUX1 : 0..1;
  );
  2 : (
    RESERVED4 : 0..8191;
    PSIDL : 0..1;
    RESERVED5 : 0..1;
    PMPEN : 0..1;
  );
  3 : (
    w : 0..4294967295;
  );
  end;
var
  PMCONbits: TPMCONbits absolute ($BF807000);
  PMCONCLR : longWord absolute ($BF807004);
  PMCONSET : longWord absolute ($BF807008);
  PMCONINV : longWord absolute ($BF80700C);
  PMMODE : longWord absolute ($BF807010);
type
  TPMMODEbits = bitpacked record
  case integer of
  0 : (
    WAITE : 0..3;
    WAITM : 0..15;
    WAITB : 0..3;
    MODE : 0..3;
    MODE16 : 0..1;
    INCM : 0..3;
    IRQM : 0..3;
    BUSY : 0..1;
  );
  1 : (
    WAITE0 : 0..1;
    WAITE1 : 0..1;
    WAITM0 : 0..1;
    WAITM1 : 0..1;
    WAITM2 : 0..1;
    WAITM3 : 0..1;
    WAITB0 : 0..1;
    WAITB1 : 0..1;
    MODE0 : 0..1;
    MODE1 : 0..1;
    RESERVED0 : 0..1;
    INCM0 : 0..1;
    INCM1 : 0..1;
    IRQM0 : 0..1;
    IRQM1 : 0..1;
  );
  2 : (
    w : 0..4294967295;
  );
  end;
var
  PMMODEbits: TPMMODEbits absolute ($BF807010);
  PMMODECLR : longWord absolute ($BF807014);
  PMMODESET : longWord absolute ($BF807018);
  PMMODEINV : longWord absolute ($BF80701C);
  PMADDR : longWord absolute ($BF807020);
type
  TPMADDRbits = bitpacked record
  case integer of
  0 : (
    ADDR : 0..16383;
    CS : 0..3;
  );
  1 : (
    PADDR : 0..16383;
  );
  2 : (
    RESERVED0 : 0..16383;
    CS1 : 0..1;
    CS2 : 0..1;
  );
  3 : (
    w : 0..4294967295;
  );
  end;
var
  PMADDRbits: TPMADDRbits absolute ($BF807020);
  PMADDRCLR : longWord absolute ($BF807024);
  PMADDRSET : longWord absolute ($BF807028);
  PMADDRINV : longWord absolute ($BF80702C);
  PMDOUT : longWord absolute ($BF807030);
type
  TPMDOUTbits = bitpacked record
  case integer of
  0 : (
    DATAOUT : 0..4294967295;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  PMDOUTbits: TPMDOUTbits absolute ($BF807030);
  PMDOUTCLR : longWord absolute ($BF807034);
  PMDOUTSET : longWord absolute ($BF807038);
  PMDOUTINV : longWord absolute ($BF80703C);
  PMDIN : longWord absolute ($BF807040);
type
  TPMDINbits = bitpacked record
  case integer of
  0 : (
    DATAIN : 0..4294967295;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  PMDINbits: TPMDINbits absolute ($BF807040);
  PMDINCLR : longWord absolute ($BF807044);
  PMDINSET : longWord absolute ($BF807048);
  PMDININV : longWord absolute ($BF80704C);
  PMAEN : longWord absolute ($BF807050);
type
  TPMAENbits = bitpacked record
  case integer of
  0 : (
    PTEN : 0..65535;
  );
  1 : (
    PTEN0 : 0..1;
    PTEN1 : 0..1;
    PTEN2 : 0..1;
    PTEN3 : 0..1;
    PTEN4 : 0..1;
    PTEN5 : 0..1;
    PTEN6 : 0..1;
    PTEN7 : 0..1;
    PTEN8 : 0..1;
    PTEN9 : 0..1;
    PTEN10 : 0..1;
    PTEN11 : 0..1;
    PTEN12 : 0..1;
    PTEN13 : 0..1;
    PTEN14 : 0..1;
    PTEN15 : 0..1;
  );
  2 : (
    w : 0..4294967295;
  );
  end;
var
  PMAENbits: TPMAENbits absolute ($BF807050);
  PMAENCLR : longWord absolute ($BF807054);
  PMAENSET : longWord absolute ($BF807058);
  PMAENINV : longWord absolute ($BF80705C);
  PMSTAT : longWord absolute ($BF807060);
type
  TPMSTATbits = bitpacked record
  case integer of
  0 : (
    OB0E : 0..1;
    OB1E : 0..1;
    OB2E : 0..1;
    OB3E : 0..1;
    RESERVED0 : 0..3;
    OBUF : 0..1;
    OBE : 0..1;
    IB0F : 0..1;
    IB1F : 0..1;
    IB2F : 0..1;
    IB3F : 0..1;
    RESERVED1 : 0..3;
    IBOV : 0..1;
    IBF : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  PMSTATbits: TPMSTATbits absolute ($BF807060);
  PMSTATCLR : longWord absolute ($BF807064);
  PMSTATSET : longWord absolute ($BF807068);
  PMSTATINV : longWord absolute ($BF80706C);
  AD1CON1 : longWord absolute ($BF809000);
type
  TAD1CON1bits = bitpacked record
  case integer of
  0 : (
    DONE : 0..1;
    SAMP : 0..1;
    ASAM : 0..1;
    RESERVED0 : 0..1;
    CLRASAM : 0..1;
    SSRC : 0..7;
    FORM : 0..7;
    RESERVED1 : 0..3;
    SIDL : 0..1;
    RESERVED2 : 0..1;
    ON : 0..1;
  );
  1 : (
    RESERVED3 : 0..31;
    SSRC0 : 0..1;
    SSRC1 : 0..1;
    SSRC2 : 0..1;
    FORM0 : 0..1;
    FORM1 : 0..1;
    FORM2 : 0..1;
  );
  2 : (
    RESERVED4 : 0..8191;
    ADSIDL : 0..1;
    RESERVED5 : 0..1;
    ADON : 0..1;
  );
  3 : (
    w : 0..4294967295;
  );
  end;
var
  AD1CON1bits: TAD1CON1bits absolute ($BF809000);
  AD1CON1CLR : longWord absolute ($BF809004);
  AD1CON1SET : longWord absolute ($BF809008);
  AD1CON1INV : longWord absolute ($BF80900C);
  AD1CON2 : longWord absolute ($BF809010);
type
  TAD1CON2bits = bitpacked record
  case integer of
  0 : (
    ALTS : 0..1;
    BUFM : 0..1;
    SMPI : 0..15;
    RESERVED0 : 0..1;
    BUFS : 0..1;
    RESERVED1 : 0..3;
    CSCNA : 0..1;
    RESERVED2 : 0..1;
    OFFCAL : 0..1;
    VCFG : 0..7;
  );
  1 : (
    RESERVED3 : 0..3;
    SMPI0 : 0..1;
    SMPI1 : 0..1;
    SMPI2 : 0..1;
    SMPI3 : 0..1;
    RESERVED4 : 0..127;
    VCFG0 : 0..1;
    VCFG1 : 0..1;
    VCFG2 : 0..1;
  );
  2 : (
    w : 0..4294967295;
  );
  end;
var
  AD1CON2bits: TAD1CON2bits absolute ($BF809010);
  AD1CON2CLR : longWord absolute ($BF809014);
  AD1CON2SET : longWord absolute ($BF809018);
  AD1CON2INV : longWord absolute ($BF80901C);
  AD1CON3 : longWord absolute ($BF809020);
type
  TAD1CON3bits = bitpacked record
  case integer of
  0 : (
    ADCS : 0..255;
    SAMC : 0..31;
    RESERVED0 : 0..3;
    ADRC : 0..1;
  );
  1 : (
    ADCS0 : 0..1;
    ADCS1 : 0..1;
    ADCS2 : 0..1;
    ADCS3 : 0..1;
    ADCS4 : 0..1;
    ADCS5 : 0..1;
    ADCS6 : 0..1;
    ADCS7 : 0..1;
    SAMC0 : 0..1;
    SAMC1 : 0..1;
    SAMC2 : 0..1;
    SAMC3 : 0..1;
    SAMC4 : 0..1;
  );
  2 : (
    w : 0..4294967295;
  );
  end;
var
  AD1CON3bits: TAD1CON3bits absolute ($BF809020);
  AD1CON3CLR : longWord absolute ($BF809024);
  AD1CON3SET : longWord absolute ($BF809028);
  AD1CON3INV : longWord absolute ($BF80902C);
  AD1CHS : longWord absolute ($BF809040);
type
  TAD1CHSbits = bitpacked record
  case integer of
  0 : (
    RESERVED0 : 0..65535;
    CH0SA : 0..15;
    RESERVED1 : 0..7;
    CH0NA : 0..1;
    CH0SB : 0..15;
    RESERVED2 : 0..7;
    CH0NB : 0..1;
  );
  1 : (
    RESERVED3 : 0..65535;
    CH0SA0 : 0..1;
    CH0SA1 : 0..1;
    CH0SA2 : 0..1;
    CH0SA3 : 0..1;
    RESERVED4 : 0..15;
    CH0SB0 : 0..1;
    CH0SB1 : 0..1;
    CH0SB2 : 0..1;
    CH0SB3 : 0..1;
  );
  2 : (
    w : 0..4294967295;
  );
  end;
var
  AD1CHSbits: TAD1CHSbits absolute ($BF809040);
  AD1CHSCLR : longWord absolute ($BF809044);
  AD1CHSSET : longWord absolute ($BF809048);
  AD1CHSINV : longWord absolute ($BF80904C);
  AD1CSSL : longWord absolute ($BF809050);
type
  TAD1CSSLbits = bitpacked record
  case integer of
  0 : (
    CSSL : 0..65535;
  );
  1 : (
    CSSL0 : 0..1;
    CSSL1 : 0..1;
    CSSL2 : 0..1;
    CSSL3 : 0..1;
    CSSL4 : 0..1;
    CSSL5 : 0..1;
    CSSL6 : 0..1;
    CSSL7 : 0..1;
    CSSL8 : 0..1;
    CSSL9 : 0..1;
    CSSL10 : 0..1;
    CSSL11 : 0..1;
    CSSL12 : 0..1;
    CSSL13 : 0..1;
    CSSL14 : 0..1;
    CSSL15 : 0..1;
  );
  2 : (
    w : 0..4294967295;
  );
  end;
var
  AD1CSSLbits: TAD1CSSLbits absolute ($BF809050);
  AD1CSSLCLR : longWord absolute ($BF809054);
  AD1CSSLSET : longWord absolute ($BF809058);
  AD1CSSLINV : longWord absolute ($BF80905C);
  AD1PCFG : longWord absolute ($BF809060);
type
  TAD1PCFGbits = bitpacked record
  case integer of
  0 : (
    PCFG : 0..65535;
  );
  1 : (
    PCFG0 : 0..1;
    PCFG1 : 0..1;
    PCFG2 : 0..1;
    PCFG3 : 0..1;
    PCFG4 : 0..1;
    PCFG5 : 0..1;
    PCFG6 : 0..1;
    PCFG7 : 0..1;
    PCFG8 : 0..1;
    PCFG9 : 0..1;
    PCFG10 : 0..1;
    PCFG11 : 0..1;
    PCFG12 : 0..1;
    PCFG13 : 0..1;
    PCFG14 : 0..1;
    PCFG15 : 0..1;
  );
  2 : (
    w : 0..4294967295;
  );
  end;
var
  AD1PCFGbits: TAD1PCFGbits absolute ($BF809060);
  AD1PCFGCLR : longWord absolute ($BF809064);
  AD1PCFGSET : longWord absolute ($BF809068);
  AD1PCFGINV : longWord absolute ($BF80906C);
  ADC1BUF0 : longWord absolute ($BF809070);
  ADC1BUF1 : longWord absolute ($BF809080);
  ADC1BUF2 : longWord absolute ($BF809090);
  ADC1BUF3 : longWord absolute ($BF8090A0);
  ADC1BUF4 : longWord absolute ($BF8090B0);
  ADC1BUF5 : longWord absolute ($BF8090C0);
  ADC1BUF6 : longWord absolute ($BF8090D0);
  ADC1BUF7 : longWord absolute ($BF8090E0);
  ADC1BUF8 : longWord absolute ($BF8090F0);
  ADC1BUF9 : longWord absolute ($BF809100);
  ADC1BUFA : longWord absolute ($BF809110);
  ADC1BUFB : longWord absolute ($BF809120);
  ADC1BUFC : longWord absolute ($BF809130);
  ADC1BUFD : longWord absolute ($BF809140);
  ADC1BUFE : longWord absolute ($BF809150);
  ADC1BUFF : longWord absolute ($BF809160);
  CVRCON : longWord absolute ($BF809800);
type
  TCVRCONbits = bitpacked record
  case integer of
  0 : (
    CVR : 0..15;
    CVRSS : 0..1;
    CVRR : 0..1;
    CVROE : 0..1;
    RESERVED0 : 0..255;
    ON : 0..1;
  );
  1 : (
    CVR0 : 0..1;
    CVR1 : 0..1;
    CVR2 : 0..1;
    CVR3 : 0..1;
  );
  2 : (
    w : 0..4294967295;
  );
  end;
var
  CVRCONbits: TCVRCONbits absolute ($BF809800);
  CVRCONCLR : longWord absolute ($BF809804);
  CVRCONSET : longWord absolute ($BF809808);
  CVRCONINV : longWord absolute ($BF80980C);
  CM1CON : longWord absolute ($BF80A000);
type
  TCM1CONbits = bitpacked record
  case integer of
  0 : (
    CCH : 0..3;
    RESERVED0 : 0..3;
    CREF : 0..1;
    RESERVED1 : 0..1;
    EVPOL : 0..3;
    COUT : 0..1;
    RESERVED2 : 0..15;
    CPOL : 0..1;
    COE : 0..1;
    ON : 0..1;
  );
  1 : (
    CCH0 : 0..1;
    CCH1 : 0..1;
    RESERVED3 : 0..15;
    EVPOL0 : 0..1;
    EVPOL1 : 0..1;
  );
  2 : (
    w : 0..4294967295;
  );
  end;
var
  CM1CONbits: TCM1CONbits absolute ($BF80A000);
  CM1CONCLR : longWord absolute ($BF80A004);
  CM1CONSET : longWord absolute ($BF80A008);
  CM1CONINV : longWord absolute ($BF80A00C);
  CM2CON : longWord absolute ($BF80A010);
type
  TCM2CONbits = bitpacked record
  case integer of
  0 : (
    CCH : 0..3;
    RESERVED0 : 0..3;
    CREF : 0..1;
    RESERVED1 : 0..1;
    EVPOL : 0..3;
    COUT : 0..1;
    RESERVED2 : 0..15;
    CPOL : 0..1;
    COE : 0..1;
    ON : 0..1;
  );
  1 : (
    CCH0 : 0..1;
    CCH1 : 0..1;
    RESERVED3 : 0..15;
    EVPOL0 : 0..1;
    EVPOL1 : 0..1;
  );
  2 : (
    w : 0..4294967295;
  );
  end;
var
  CM2CONbits: TCM2CONbits absolute ($BF80A010);
  CM2CONCLR : longWord absolute ($BF80A014);
  CM2CONSET : longWord absolute ($BF80A018);
  CM2CONINV : longWord absolute ($BF80A01C);
  CMSTAT : longWord absolute ($BF80A060);
type
  TCMSTATbits = bitpacked record
  case integer of
  0 : (
    C1OUT : 0..1;
    C2OUT : 0..1;
    RESERVED0 : 0..2047;
    SIDL : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  CMSTATbits: TCMSTATbits absolute ($BF80A060);
  CMSTATCLR : longWord absolute ($BF80A064);
  CMSTATSET : longWord absolute ($BF80A068);
  CMSTATINV : longWord absolute ($BF80A06C);
  OSCCON : longWord absolute ($BF80F000);
type
  TOSCCONbits = bitpacked record
  case integer of
  0 : (
    OSWEN : 0..1;
    SOSCEN : 0..1;
    UFRCEN : 0..1;
    CF : 0..1;
    SLPEN : 0..1;
    SLOCK : 0..1;
    ULOCK : 0..1;
    CLKLOCK : 0..1;
    NOSC : 0..7;
    RESERVED0 : 0..1;
    COSC : 0..7;
    RESERVED1 : 0..1;
    PLLMULT : 0..7;
    PBDIV : 0..3;
    RESERVED2 : 0..1;
    SOSCRDY : 0..1;
    RESERVED3 : 0..1;
    FRCDIV : 0..7;
    PLLODIV : 0..7;
  );
  1 : (
    RESERVED4 : 0..255;
    NOSC0 : 0..1;
    NOSC1 : 0..1;
    NOSC2 : 0..1;
    RESERVED5 : 0..1;
    COSC0 : 0..1;
    COSC1 : 0..1;
    COSC2 : 0..1;
    RESERVED6 : 0..1;
    PLLMULT0 : 0..1;
    PLLMULT1 : 0..1;
    PLLMULT2 : 0..1;
    PBDIV0 : 0..1;
    PBDIV1 : 0..1;
    RESERVED7 : 0..7;
    FRCDIV0 : 0..1;
    FRCDIV1 : 0..1;
    FRCDIV2 : 0..1;
    PLLODIV0 : 0..1;
    PLLODIV1 : 0..1;
    PLLODIV2 : 0..1;
  );
  2 : (
    w : 0..4294967295;
  );
  end;
var
  OSCCONbits: TOSCCONbits absolute ($BF80F000);
  OSCCONCLR : longWord absolute ($BF80F004);
  OSCCONSET : longWord absolute ($BF80F008);
  OSCCONINV : longWord absolute ($BF80F00C);
  OSCTUN : longWord absolute ($BF80F010);
type
  TOSCTUNbits = bitpacked record
  case integer of
  0 : (
    TUN : 0..63;
  );
  1 : (
    TUN0 : 0..1;
    TUN1 : 0..1;
    TUN2 : 0..1;
    TUN3 : 0..1;
    TUN4 : 0..1;
    TUN5 : 0..1;
  );
  2 : (
    w : 0..4294967295;
  );
  end;
var
  OSCTUNbits: TOSCTUNbits absolute ($BF80F010);
  OSCTUNCLR : longWord absolute ($BF80F014);
  OSCTUNSET : longWord absolute ($BF80F018);
  OSCTUNINV : longWord absolute ($BF80F01C);
  DDPCON : longWord absolute ($BF80F200);
type
  TDDPCONbits = bitpacked record
    TDOEN : 0..1;
    RESERVED0 : 0..1;
    TROEN : 0..1;
    JTAGEN : 0..1;
  end;
var
  DDPCONbits: TDDPCONbits absolute ($BF80F200);
  DEVID : longWord absolute ($BF80F220);
type
  TDEVIDbits = bitpacked record
    DEVID : 0..268435455;
    VER : 0..15;
  end;
var
  DEVIDbits: TDEVIDbits absolute ($BF80F220);
  SYSKEY : longWord absolute ($BF80F230);
  SYSKEYCLR : longWord absolute ($BF80F234);
  SYSKEYSET : longWord absolute ($BF80F238);
  SYSKEYINV : longWord absolute ($BF80F23C);
  NVMCON : longWord absolute ($BF80F400);
type
  TNVMCONbits = bitpacked record
  case integer of
  0 : (
    NVMOP : 0..15;
    RESERVED0 : 0..127;
    LVDSTAT : 0..1;
    LVDERR : 0..1;
    WRERR : 0..1;
    WREN : 0..1;
    WR : 0..1;
  );
  1 : (
    NVMOP0 : 0..1;
    NVMOP1 : 0..1;
    NVMOP2 : 0..1;
    NVMOP3 : 0..1;
  );
  2 : (
    PROGOP : 0..15;
  );
  3 : (
    PROGOP0 : 0..1;
    PROGOP1 : 0..1;
    PROGOP2 : 0..1;
    PROGOP3 : 0..1;
  );
  4 : (
    w : 0..4294967295;
  );
  end;
var
  NVMCONbits: TNVMCONbits absolute ($BF80F400);
  NVMCONCLR : longWord absolute ($BF80F404);
  NVMCONSET : longWord absolute ($BF80F408);
  NVMCONINV : longWord absolute ($BF80F40C);
  NVMKEY : longWord absolute ($BF80F410);
  NVMADDR : longWord absolute ($BF80F420);
  NVMADDRCLR : longWord absolute ($BF80F424);
  NVMADDRSET : longWord absolute ($BF80F428);
  NVMADDRINV : longWord absolute ($BF80F42C);
  NVMDATA : longWord absolute ($BF80F430);
  NVMSRCADDR : longWord absolute ($BF80F440);
  RCON : longWord absolute ($BF80F600);
type
  TRCONbits = bitpacked record
  case integer of
  0 : (
    POR : 0..1;
    BOR : 0..1;
    IDLE : 0..1;
    SLEEP : 0..1;
    WDTO : 0..1;
    RESERVED0 : 0..1;
    SWR : 0..1;
    EXTR : 0..1;
    VREGS : 0..1;
    CMR : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  RCONbits: TRCONbits absolute ($BF80F600);
  RCONCLR : longWord absolute ($BF80F604);
  RCONSET : longWord absolute ($BF80F608);
  RCONINV : longWord absolute ($BF80F60C);
  RSWRST : longWord absolute ($BF80F610);
type
  TRSWRSTbits = bitpacked record
  case integer of
  0 : (
    SWRST : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  RSWRSTbits: TRSWRSTbits absolute ($BF80F610);
  RSWRSTCLR : longWord absolute ($BF80F614);
  RSWRSTSET : longWord absolute ($BF80F618);
  RSWRSTINV : longWord absolute ($BF80F61C);
  _DDPSTAT : longWord absolute ($BF880140);
type
  T_DDPSTATbits = bitpacked record
    RESERVED0 : 0..1;
    APIFUL : 0..1;
    APOFUL : 0..1;
    STRFUL : 0..1;
    RESERVED1 : 0..31;
    APIOV : 0..1;
    APOOV : 0..1;
    RESERVED2 : 0..31;
    STOV : 0..65535;
  end;
var
  _DDPSTATbits: T_DDPSTATbits absolute ($BF880140);
  _STRO : longWord absolute ($BF880170);
  _STROCLR : longWord absolute ($BF880174);
  _STROSET : longWord absolute ($BF880178);
  _STROINV : longWord absolute ($BF88017C);
  _APPO : longWord absolute ($BF880180);
  _APPOCLR : longWord absolute ($BF880184);
  _APPOSET : longWord absolute ($BF880188);
  _APPOINV : longWord absolute ($BF88018C);
  _APPI : longWord absolute ($BF880190);
  INTCON : longWord absolute ($BF881000);
type
  TINTCONbits = bitpacked record
  case integer of
  0 : (
    INT0EP : 0..1;
    INT1EP : 0..1;
    INT2EP : 0..1;
    INT3EP : 0..1;
    INT4EP : 0..1;
    RESERVED0 : 0..7;
    TPC : 0..7;
    RESERVED1 : 0..1;
    MVEC : 0..1;
    RESERVED2 : 0..1;
    FRZ : 0..1;
    RESERVED3 : 0..1;
    SS0 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  INTCONbits: TINTCONbits absolute ($BF881000);
  INTCONCLR : longWord absolute ($BF881004);
  INTCONSET : longWord absolute ($BF881008);
  INTCONINV : longWord absolute ($BF88100C);
  INTSTAT : longWord absolute ($BF881010);
type
  TINTSTATbits = bitpacked record
    VEC : 0..63;
    RESERVED0 : 0..3;
    SRIPL : 0..7;
  end;
var
  INTSTATbits: TINTSTATbits absolute ($BF881010);
  IPTMR : longWord absolute ($BF881020);
  IPTMRCLR : longWord absolute ($BF881024);
  IPTMRSET : longWord absolute ($BF881028);
  IPTMRINV : longWord absolute ($BF88102C);
  IFS0 : longWord absolute ($BF881030);
type
  TIFS0bits = bitpacked record
  case integer of
  0 : (
    CTIF : 0..1;
    CS0IF : 0..1;
    CS1IF : 0..1;
    INT0IF : 0..1;
    T1IF : 0..1;
    IC1IF : 0..1;
    OC1IF : 0..1;
    INT1IF : 0..1;
    T2IF : 0..1;
    IC2IF : 0..1;
    OC2IF : 0..1;
    INT2IF : 0..1;
    T3IF : 0..1;
    IC3IF : 0..1;
    OC3IF : 0..1;
    INT3IF : 0..1;
    T4IF : 0..1;
    IC4IF : 0..1;
    OC4IF : 0..1;
    INT4IF : 0..1;
    T5IF : 0..1;
    IC5IF : 0..1;
    OC5IF : 0..1;
    SPI1EIF : 0..1;
    SPI1RXIF : 0..1;
    SPI1TXIF : 0..1;
    U1EIF : 0..1;
    U1RXIF : 0..1;
    U1TXIF : 0..1;
    I2C1BIF : 0..1;
    I2C1SIF : 0..1;
    I2C1MIF : 0..1;
  );
  1 : (
    RESERVED0 : 0..67108863;
    U1AEIF : 0..1;
    U1ARXIF : 0..1;
    U1ATXIF : 0..1;
  );
  2 : (
    RESERVED1 : 0..67108863;
    SPI3EIF : 0..1;
    SPI3RXIF : 0..1;
    SPI3TXIF : 0..1;
  );
  3 : (
    RESERVED2 : 0..67108863;
    SPI1AEIF : 0..1;
    SPI1ARXIF : 0..1;
    SPI1ATXIF : 0..1;
  );
  4 : (
    RESERVED3 : 0..67108863;
    I2C3BIF : 0..1;
    I2C3SIF : 0..1;
    I2C3MIF : 0..1;
  );
  5 : (
    RESERVED4 : 0..67108863;
    I2C1ABIF : 0..1;
    I2C1ASIF : 0..1;
    I2C1AMIF : 0..1;
  );
  6 : (
    w : 0..4294967295;
  );
  end;
var
  IFS0bits: TIFS0bits absolute ($BF881030);
  IFS0CLR : longWord absolute ($BF881034);
  IFS0SET : longWord absolute ($BF881038);
  IFS0INV : longWord absolute ($BF88103C);
  IFS1 : longWord absolute ($BF881040);
type
  TIFS1bits = bitpacked record
  case integer of
  0 : (
    CNIF : 0..1;
    AD1IF : 0..1;
    PMPIF : 0..1;
    CMP1IF : 0..1;
    CMP2IF : 0..1;
    U3EIF : 0..1;
    U3RXIF : 0..1;
    U3TXIF : 0..1;
    U2EIF : 0..1;
    U2RXIF : 0..1;
    U2TXIF : 0..1;
    I2C2BIF : 0..1;
    I2C2SIF : 0..1;
    I2C2MIF : 0..1;
    FSCMIF : 0..1;
    RTCCIF : 0..1;
    DMA0IF : 0..1;
    DMA1IF : 0..1;
    DMA2IF : 0..1;
    DMA3IF : 0..1;
    DMA4IF : 0..1;
    DMA5IF : 0..1;
    DMA6IF : 0..1;
    DMA7IF : 0..1;
    FCEIF : 0..1;
    USBIF : 0..1;
    CAN1IF : 0..1;
    CAN2IF : 0..1;
    ETHIF : 0..1;
    IC1EIF : 0..1;
    IC2EIF : 0..1;
    IC3EIF : 0..1;
  );
  1 : (
    RESERVED0 : 0..31;
    U2AEIF : 0..1;
    U2ARXIF : 0..1;
    U2ATXIF : 0..1;
    U3AEIF : 0..1;
    U3ARXIF : 0..1;
    U3ATXIF : 0..1;
  );
  2 : (
    RESERVED1 : 0..31;
    SPI2EIF : 0..1;
    SPI2RXIF : 0..1;
    SPI2TXIF : 0..1;
    SPI4EIF : 0..1;
    SPI4RXIF : 0..1;
    SPI4TXIF : 0..1;
  );
  3 : (
    RESERVED2 : 0..31;
    SPI2AEIF : 0..1;
    SPI2ARXIF : 0..1;
    SPI2ATXIF : 0..1;
    SPI3AEIF : 0..1;
    SPI3ARXIF : 0..1;
    SPI3ATXIF : 0..1;
  );
  4 : (
    RESERVED3 : 0..31;
    I2C4BIF : 0..1;
    I2C4SIF : 0..1;
    I2C4MIF : 0..1;
    I2C5BIF : 0..1;
    I2C5SIF : 0..1;
    I2C5MIF : 0..1;
  );
  5 : (
    RESERVED4 : 0..31;
    I2C2ABIF : 0..1;
    I2C2ASIF : 0..1;
    I2C2AMIF : 0..1;
    I2C3ABIF : 0..1;
    I2C3ASIF : 0..1;
    I2C3AMIF : 0..1;
  );
  6 : (
    w : 0..4294967295;
  );
  end;
var
  IFS1bits: TIFS1bits absolute ($BF881040);
  IFS1CLR : longWord absolute ($BF881044);
  IFS1SET : longWord absolute ($BF881048);
  IFS1INV : longWord absolute ($BF88104C);
  IFS2 : longWord absolute ($BF881050);
type
  TIFS2bits = bitpacked record
  case integer of
  0 : (
    IC4EIF : 0..1;
    IC5EIF : 0..1;
    PMPEIF : 0..1;
    U1BEIF : 0..1;
    U1BRXIF : 0..1;
    U1BTXIF : 0..1;
    U2BEIF : 0..1;
    U2BRXIF : 0..1;
    U2BTXIF : 0..1;
    U3BEIF : 0..1;
    U3BRXIF : 0..1;
    U3BTXIF : 0..1;
  );
  1 : (
    RESERVED0 : 0..7;
    U4EIF : 0..1;
    U4RXIF : 0..1;
    U4TXIF : 0..1;
    U6EIF : 0..1;
    U6RXIF : 0..1;
    U6TXIF : 0..1;
    U5EIF : 0..1;
    U5RXIF : 0..1;
    U5TXIF : 0..1;
  );
  2 : (
    w : 0..4294967295;
  );
  end;
var
  IFS2bits: TIFS2bits absolute ($BF881050);
  IFS2CLR : longWord absolute ($BF881054);
  IFS2SET : longWord absolute ($BF881058);
  IFS2INV : longWord absolute ($BF88105C);
  IEC0 : longWord absolute ($BF881060);
type
  TIEC0bits = bitpacked record
  case integer of
  0 : (
    CTIE : 0..1;
    CS0IE : 0..1;
    CS1IE : 0..1;
    INT0IE : 0..1;
    T1IE : 0..1;
    IC1IE : 0..1;
    OC1IE : 0..1;
    INT1IE : 0..1;
    T2IE : 0..1;
    IC2IE : 0..1;
    OC2IE : 0..1;
    INT2IE : 0..1;
    T3IE : 0..1;
    IC3IE : 0..1;
    OC3IE : 0..1;
    INT3IE : 0..1;
    T4IE : 0..1;
    IC4IE : 0..1;
    OC4IE : 0..1;
    INT4IE : 0..1;
    T5IE : 0..1;
    IC5IE : 0..1;
    OC5IE : 0..1;
    SPI1EIE : 0..1;
    SPI1RXIE : 0..1;
    SPI1TXIE : 0..1;
    U1EIE : 0..1;
    U1RXIE : 0..1;
    U1TXIE : 0..1;
    I2C1BIE : 0..1;
    I2C1SIE : 0..1;
    I2C1MIE : 0..1;
  );
  1 : (
    RESERVED0 : 0..67108863;
    U1AEIE : 0..1;
    U1ARXIE : 0..1;
    U1ATXIE : 0..1;
  );
  2 : (
    RESERVED1 : 0..67108863;
    SPI3EIE : 0..1;
    SPI3RXIE : 0..1;
    SPI3TXIE : 0..1;
  );
  3 : (
    RESERVED2 : 0..67108863;
    SPI1AEIE : 0..1;
    SPI1ARXIE : 0..1;
    SPI1ATXIE : 0..1;
  );
  4 : (
    RESERVED3 : 0..67108863;
    I2C3BIE : 0..1;
    I2C3SIE : 0..1;
    I2C3MIE : 0..1;
  );
  5 : (
    RESERVED4 : 0..67108863;
    I2C1ABIE : 0..1;
    I2C1ASIE : 0..1;
    I2C1AMIE : 0..1;
  );
  6 : (
    w : 0..4294967295;
  );
  end;
var
  IEC0bits: TIEC0bits absolute ($BF881060);
  IEC0CLR : longWord absolute ($BF881064);
  IEC0SET : longWord absolute ($BF881068);
  IEC0INV : longWord absolute ($BF88106C);
  IEC1 : longWord absolute ($BF881070);
type
  TIEC1bits = bitpacked record
  case integer of
  0 : (
    CNIE : 0..1;
    AD1IE : 0..1;
    PMPIE : 0..1;
    CMP1IE : 0..1;
    CMP2IE : 0..1;
    U3EIE : 0..1;
    U3RXIE : 0..1;
    U3TXIE : 0..1;
    U2EIE : 0..1;
    U2RXIE : 0..1;
    U2TXIE : 0..1;
    I2C2BIE : 0..1;
    I2C2SIE : 0..1;
    I2C2MIE : 0..1;
    FSCMIE : 0..1;
    RTCCIE : 0..1;
    DMA0IE : 0..1;
    DMA1IE : 0..1;
    DMA2IE : 0..1;
    DMA3IE : 0..1;
    DMA4IE : 0..1;
    DMA5IE : 0..1;
    DMA6IE : 0..1;
    DMA7IE : 0..1;
    FCEIE : 0..1;
    USBIE : 0..1;
    CAN1IE : 0..1;
    CAN2IE : 0..1;
    ETHIE : 0..1;
    IC1EIE : 0..1;
    IC2EIE : 0..1;
    IC3EIE : 0..1;
  );
  1 : (
    RESERVED0 : 0..31;
    U2AEIE : 0..1;
    U2ARXIE : 0..1;
    U2ATXIE : 0..1;
    U3AEIE : 0..1;
    U3ARXIE : 0..1;
    U3ATXIE : 0..1;
  );
  2 : (
    RESERVED1 : 0..31;
    SPI2EIE : 0..1;
    SPI2RXIE : 0..1;
    SPI2TXIE : 0..1;
    SPI4EIE : 0..1;
    SPI4RXIE : 0..1;
    SPI4TXIE : 0..1;
  );
  3 : (
    RESERVED2 : 0..31;
    SPI2AEIE : 0..1;
    SPI2ARXIE : 0..1;
    SPI2ATXIE : 0..1;
    SPI3AEIE : 0..1;
    SPI3ARXIE : 0..1;
    SPI3ATXIE : 0..1;
  );
  4 : (
    RESERVED3 : 0..31;
    I2C4BIE : 0..1;
    I2C4SIE : 0..1;
    I2C4MIE : 0..1;
    I2C5BIE : 0..1;
    I2C5SIE : 0..1;
    I2C5MIE : 0..1;
  );
  5 : (
    RESERVED4 : 0..31;
    I2C2ABIE : 0..1;
    I2C2ASIE : 0..1;
    I2C2AMIE : 0..1;
    I2C3ABIE : 0..1;
    I2C3ASIE : 0..1;
    I2C3AMIE : 0..1;
  );
  6 : (
    w : 0..4294967295;
  );
  end;
var
  IEC1bits: TIEC1bits absolute ($BF881070);
  IEC1CLR : longWord absolute ($BF881074);
  IEC1SET : longWord absolute ($BF881078);
  IEC1INV : longWord absolute ($BF88107C);
  IEC2 : longWord absolute ($BF881080);
type
  TIEC2bits = bitpacked record
  case integer of
  0 : (
    IC4EIE : 0..1;
    IC5EIE : 0..1;
    PMPEIE : 0..1;
    U1BEIE : 0..1;
    U1BRXIE : 0..1;
    U1BTXIE : 0..1;
    U2BEIE : 0..1;
    U2BRXIE : 0..1;
    U2BTXIE : 0..1;
    U3BEIE : 0..1;
    U3BRXIE : 0..1;
    U3BTXIE : 0..1;
  );
  1 : (
    RESERVED0 : 0..7;
    U4EIE : 0..1;
    U4RXIE : 0..1;
    U4TXIE : 0..1;
    U6EIE : 0..1;
    U6RXIE : 0..1;
    U6TXIE : 0..1;
    U5EIE : 0..1;
    U5RXIE : 0..1;
    U5TXIE : 0..1;
  );
  2 : (
    w : 0..4294967295;
  );
  end;
var
  IEC2bits: TIEC2bits absolute ($BF881080);
  IEC2CLR : longWord absolute ($BF881084);
  IEC2SET : longWord absolute ($BF881088);
  IEC2INV : longWord absolute ($BF88108C);
  IPC0 : longWord absolute ($BF881090);
type
  TIPC0bits = bitpacked record
  case integer of
  0 : (
    CTIS : 0..3;
    CTIP : 0..7;
    RESERVED0 : 0..7;
    CS0IS : 0..3;
    CS0IP : 0..7;
    RESERVED1 : 0..7;
    CS1IS : 0..3;
    CS1IP : 0..7;
    RESERVED2 : 0..7;
    INT0IS : 0..3;
    INT0IP : 0..7;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  IPC0bits: TIPC0bits absolute ($BF881090);
  IPC0CLR : longWord absolute ($BF881094);
  IPC0SET : longWord absolute ($BF881098);
  IPC0INV : longWord absolute ($BF88109C);
  IPC1 : longWord absolute ($BF8810A0);
type
  TIPC1bits = bitpacked record
  case integer of
  0 : (
    T1IS : 0..3;
    T1IP : 0..7;
    RESERVED0 : 0..7;
    IC1IS : 0..3;
    IC1IP : 0..7;
    RESERVED1 : 0..7;
    OC1IS : 0..3;
    OC1IP : 0..7;
    RESERVED2 : 0..7;
    INT1IS : 0..3;
    INT1IP : 0..7;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  IPC1bits: TIPC1bits absolute ($BF8810A0);
  IPC1CLR : longWord absolute ($BF8810A4);
  IPC1SET : longWord absolute ($BF8810A8);
  IPC1INV : longWord absolute ($BF8810AC);
  IPC2 : longWord absolute ($BF8810B0);
type
  TIPC2bits = bitpacked record
  case integer of
  0 : (
    T2IS : 0..3;
    T2IP : 0..7;
    RESERVED0 : 0..7;
    IC2IS : 0..3;
    IC2IP : 0..7;
    RESERVED1 : 0..7;
    OC2IS : 0..3;
    OC2IP : 0..7;
    RESERVED2 : 0..7;
    INT2IS : 0..3;
    INT2IP : 0..7;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  IPC2bits: TIPC2bits absolute ($BF8810B0);
  IPC2CLR : longWord absolute ($BF8810B4);
  IPC2SET : longWord absolute ($BF8810B8);
  IPC2INV : longWord absolute ($BF8810BC);
  IPC3 : longWord absolute ($BF8810C0);
type
  TIPC3bits = bitpacked record
  case integer of
  0 : (
    T3IS : 0..3;
    T3IP : 0..7;
    RESERVED0 : 0..7;
    IC3IS : 0..3;
    IC3IP : 0..7;
    RESERVED1 : 0..7;
    OC3IS : 0..3;
    OC3IP : 0..7;
    RESERVED2 : 0..7;
    INT3IS : 0..3;
    INT3IP : 0..7;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  IPC3bits: TIPC3bits absolute ($BF8810C0);
  IPC3CLR : longWord absolute ($BF8810C4);
  IPC3SET : longWord absolute ($BF8810C8);
  IPC3INV : longWord absolute ($BF8810CC);
  IPC4 : longWord absolute ($BF8810D0);
type
  TIPC4bits = bitpacked record
  case integer of
  0 : (
    T4IS : 0..3;
    T4IP : 0..7;
    RESERVED0 : 0..7;
    IC4IS : 0..3;
    IC4IP : 0..7;
    RESERVED1 : 0..7;
    OC4IS : 0..3;
    OC4IP : 0..7;
    RESERVED2 : 0..7;
    INT4IS : 0..3;
    INT4IP : 0..7;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  IPC4bits: TIPC4bits absolute ($BF8810D0);
  IPC4CLR : longWord absolute ($BF8810D4);
  IPC4SET : longWord absolute ($BF8810D8);
  IPC4INV : longWord absolute ($BF8810DC);
  IPC5 : longWord absolute ($BF8810E0);
type
  TIPC5bits = bitpacked record
  case integer of
  0 : (
    T5IS : 0..3;
    T5IP : 0..7;
    RESERVED0 : 0..7;
    IC5IS : 0..3;
    IC5IP : 0..7;
    RESERVED1 : 0..7;
    OC5IS : 0..3;
    OC5IP : 0..7;
    RESERVED2 : 0..7;
    SPI1IS : 0..3;
    SPI1IP : 0..7;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  IPC5bits: TIPC5bits absolute ($BF8810E0);
  IPC5CLR : longWord absolute ($BF8810E4);
  IPC5SET : longWord absolute ($BF8810E8);
  IPC5INV : longWord absolute ($BF8810EC);
  IPC6 : longWord absolute ($BF8810F0);
type
  TIPC6bits = bitpacked record
  case integer of
  0 : (
    U1IS : 0..3;
    U1IP : 0..7;
    RESERVED0 : 0..7;
    I2C1IS : 0..3;
    I2C1IP : 0..7;
    RESERVED1 : 0..7;
    CNIS : 0..3;
    CNIP : 0..7;
    RESERVED2 : 0..7;
    AD1IS : 0..3;
    AD1IP : 0..7;
  );
  1 : (
    U1AIS : 0..3;
    U1AIP : 0..7;
  );
  2 : (
    SPI3IS : 0..3;
    SPI3IP : 0..7;
  );
  3 : (
    SPI1AIS : 0..3;
    SPI1AIP : 0..7;
  );
  4 : (
    I2C3IS : 0..3;
    I2C3IP : 0..7;
  );
  5 : (
    I2C1AIS : 0..3;
    I2C1AIP : 0..7;
  );
  6 : (
    w : 0..4294967295;
  );
  end;
var
  IPC6bits: TIPC6bits absolute ($BF8810F0);
  IPC6CLR : longWord absolute ($BF8810F4);
  IPC6SET : longWord absolute ($BF8810F8);
  IPC6INV : longWord absolute ($BF8810FC);
  IPC7 : longWord absolute ($BF881100);
type
  TIPC7bits = bitpacked record
  case integer of
  0 : (
    PMPIS : 0..3;
    PMPIP : 0..7;
    RESERVED0 : 0..7;
    CMP1IS : 0..3;
    CMP1IP : 0..7;
    RESERVED1 : 0..7;
    CMP2IS : 0..3;
    CMP2IP : 0..7;
    RESERVED2 : 0..7;
    U3IS : 0..3;
    U3IP : 0..7;
  );
  1 : (
    RESERVED3 : 0..16777215;
    U2AIS : 0..3;
    U2AIP : 0..7;
  );
  2 : (
    RESERVED4 : 0..16777215;
    SPI2IS : 0..3;
    SPI2IP : 0..7;
  );
  3 : (
    RESERVED5 : 0..16777215;
    SPI2AIS : 0..3;
    SPI2AIP : 0..7;
  );
  4 : (
    RESERVED6 : 0..16777215;
    I2C4IS : 0..3;
    I2C4IP : 0..7;
  );
  5 : (
    RESERVED7 : 0..16777215;
    I2C2AIS : 0..3;
    I2C2AIP : 0..7;
  );
  6 : (
    w : 0..4294967295;
  );
  end;
var
  IPC7bits: TIPC7bits absolute ($BF881100);
  IPC7CLR : longWord absolute ($BF881104);
  IPC7SET : longWord absolute ($BF881108);
  IPC7INV : longWord absolute ($BF88110C);
  IPC8 : longWord absolute ($BF881110);
type
  TIPC8bits = bitpacked record
  case integer of
  0 : (
    U2IS : 0..3;
    U2IP : 0..7;
    RESERVED0 : 0..7;
    I2C2IS : 0..3;
    I2C2IP : 0..7;
    RESERVED1 : 0..7;
    FSCMIS : 0..3;
    FSCMIP : 0..7;
    RESERVED2 : 0..7;
    RTCCIS : 0..3;
    RTCCIP : 0..7;
  );
  1 : (
    U3AIS : 0..3;
    U3AIP : 0..7;
  );
  2 : (
    SPI4IS : 0..3;
    SPI4IP : 0..7;
  );
  3 : (
    SPI3AIS : 0..3;
    SPI3AIP : 0..7;
  );
  4 : (
    I2C5IS : 0..3;
    I2C5IP : 0..7;
  );
  5 : (
    I2C3AIS : 0..3;
    I2C3AIP : 0..7;
  );
  6 : (
    w : 0..4294967295;
  );
  end;
var
  IPC8bits: TIPC8bits absolute ($BF881110);
  IPC8CLR : longWord absolute ($BF881114);
  IPC8SET : longWord absolute ($BF881118);
  IPC8INV : longWord absolute ($BF88111C);
  IPC9 : longWord absolute ($BF881120);
type
  TIPC9bits = bitpacked record
  case integer of
  0 : (
    DMA0IS : 0..3;
    DMA0IP : 0..7;
    RESERVED0 : 0..7;
    DMA1IS : 0..3;
    DMA1IP : 0..7;
    RESERVED1 : 0..7;
    DMA2IS : 0..3;
    DMA2IP : 0..7;
    RESERVED2 : 0..7;
    DMA3IS : 0..3;
    DMA3IP : 0..7;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  IPC9bits: TIPC9bits absolute ($BF881120);
  IPC9CLR : longWord absolute ($BF881124);
  IPC9SET : longWord absolute ($BF881128);
  IPC9INV : longWord absolute ($BF88112C);
  IPC10 : longWord absolute ($BF881130);
type
  TIPC10bits = bitpacked record
  case integer of
  0 : (
    DMA4IS : 0..3;
    DMA4IP : 0..7;
    RESERVED0 : 0..7;
    DMA5IS : 0..3;
    DMA5IP : 0..7;
    RESERVED1 : 0..7;
    DMA6IS : 0..3;
    DMA6IP : 0..7;
    RESERVED2 : 0..7;
    DMA7IS : 0..3;
    DMA7IP : 0..7;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  IPC10bits: TIPC10bits absolute ($BF881130);
  IPC10CLR : longWord absolute ($BF881134);
  IPC10SET : longWord absolute ($BF881138);
  IPC10INV : longWord absolute ($BF88113C);
  IPC11 : longWord absolute ($BF881140);
type
  TIPC11bits = bitpacked record
  case integer of
  0 : (
    FCEIS : 0..3;
    FCEIP : 0..7;
    RESERVED0 : 0..7;
    USBIS : 0..3;
    USBIP : 0..7;
    RESERVED1 : 0..7;
    CAN1IS : 0..3;
    CAN1IP : 0..7;
    RESERVED2 : 0..7;
    CAN2IS : 0..3;
    CAN2IP : 0..7;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  IPC11bits: TIPC11bits absolute ($BF881140);
  IPC11CLR : longWord absolute ($BF881144);
  IPC11SET : longWord absolute ($BF881148);
  IPC11INV : longWord absolute ($BF88114C);
  IPC12 : longWord absolute ($BF881150);
type
  TIPC12bits = bitpacked record
  case integer of
  0 : (
    ETHIS : 0..3;
    ETHIP : 0..7;
    RESERVED0 : 0..7;
    U4IS : 0..3;
    U4IP : 0..7;
    RESERVED1 : 0..7;
    U6IS : 0..3;
    U6IP : 0..7;
    RESERVED2 : 0..7;
    U5IS : 0..3;
    U5IP : 0..7;
  );
  1 : (
    RESERVED3 : 0..255;
    U1BIS : 0..3;
    U1BIP : 0..7;
    RESERVED4 : 0..7;
    U2BIS : 0..3;
    U2BIP : 0..7;
    RESERVED5 : 0..7;
    U3BIS : 0..3;
    U3BIP : 0..7;
  );
  2 : (
    w : 0..4294967295;
  );
  end;
var
  IPC12bits: TIPC12bits absolute ($BF881150);
  IPC12CLR : longWord absolute ($BF881154);
  IPC12SET : longWord absolute ($BF881158);
  IPC12INV : longWord absolute ($BF88115C);
  BMXCON : longWord absolute ($BF882000);
type
  TBMXCONbits = bitpacked record
  case integer of
  0 : (
    BMXARB : 0..7;
    RESERVED0 : 0..7;
    BMXWSDRM : 0..1;
    RESERVED1 : 0..511;
    BMXERRIS : 0..1;
    BMXERRDS : 0..1;
    BMXERRDMA : 0..1;
    BMXERRICD : 0..1;
    BMXERRIXI : 0..1;
    RESERVED2 : 0..31;
    BMXCHEDMA : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  BMXCONbits: TBMXCONbits absolute ($BF882000);
  BMXCONCLR : longWord absolute ($BF882004);
  BMXCONSET : longWord absolute ($BF882008);
  BMXCONINV : longWord absolute ($BF88200C);
  BMXDKPBA : longWord absolute ($BF882010);
  BMXDKPBACLR : longWord absolute ($BF882014);
  BMXDKPBASET : longWord absolute ($BF882018);
  BMXDKPBAINV : longWord absolute ($BF88201C);
  BMXDUDBA : longWord absolute ($BF882020);
  BMXDUDBACLR : longWord absolute ($BF882024);
  BMXDUDBASET : longWord absolute ($BF882028);
  BMXDUDBAINV : longWord absolute ($BF88202C);
  BMXDUPBA : longWord absolute ($BF882030);
  BMXDUPBACLR : longWord absolute ($BF882034);
  BMXDUPBASET : longWord absolute ($BF882038);
  BMXDUPBAINV : longWord absolute ($BF88203C);
  BMXDRMSZ : longWord absolute ($BF882040);
  BMXPUPBA : longWord absolute ($BF882050);
  BMXPUPBACLR : longWord absolute ($BF882054);
  BMXPUPBASET : longWord absolute ($BF882058);
  BMXPUPBAINV : longWord absolute ($BF88205C);
  BMXPFMSZ : longWord absolute ($BF882060);
  BMXBOOTSZ : longWord absolute ($BF882070);
  DMACON : longWord absolute ($BF883000);
type
  TDMACONbits = bitpacked record
  case integer of
  0 : (
    RESERVED0 : 0..2047;
    DMABUSY : 0..1;
    SUSPEND : 0..1;
    RESERVED1 : 0..3;
    ON : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  DMACONbits: TDMACONbits absolute ($BF883000);
  DMACONCLR : longWord absolute ($BF883004);
  DMACONSET : longWord absolute ($BF883008);
  DMACONINV : longWord absolute ($BF88300C);
  DMASTAT : longWord absolute ($BF883010);
type
  TDMASTATbits = bitpacked record
  case integer of
  0 : (
    DMACH : 0..7;
    RDWR : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  DMASTATbits: TDMASTATbits absolute ($BF883010);
  DMASTATCLR : longWord absolute ($BF883014);
  DMASTATSET : longWord absolute ($BF883018);
  DMASTATINV : longWord absolute ($BF88301C);
  DMAADDR : longWord absolute ($BF883020);
  DMAADDRCLR : longWord absolute ($BF883024);
  DMAADDRSET : longWord absolute ($BF883028);
  DMAADDRINV : longWord absolute ($BF88302C);
  DCRCCON : longWord absolute ($BF883030);
type
  TDCRCCONbits = bitpacked record
  case integer of
  0 : (
    CRCCH : 0..7;
    RESERVED0 : 0..3;
    CRCTYP : 0..1;
    CRCAPP : 0..1;
    CRCEN : 0..1;
    PLEN : 0..31;
    RESERVED1 : 0..2047;
    BITO : 0..1;
    RESERVED2 : 0..3;
    WBO : 0..1;
    BYTO : 0..3;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  DCRCCONbits: TDCRCCONbits absolute ($BF883030);
  DCRCCONCLR : longWord absolute ($BF883034);
  DCRCCONSET : longWord absolute ($BF883038);
  DCRCCONINV : longWord absolute ($BF88303C);
  DCRCDATA : longWord absolute ($BF883040);
  DCRCDATACLR : longWord absolute ($BF883044);
  DCRCDATASET : longWord absolute ($BF883048);
  DCRCDATAINV : longWord absolute ($BF88304C);
  DCRCXOR : longWord absolute ($BF883050);
  DCRCXORCLR : longWord absolute ($BF883054);
  DCRCXORSET : longWord absolute ($BF883058);
  DCRCXORINV : longWord absolute ($BF88305C);
  DCH0CON : longWord absolute ($BF883060);
type
  TDCH0CONbits = bitpacked record
  case integer of
  0 : (
    CHPRI : 0..3;
    CHEDET : 0..1;
    RESERVED0 : 0..1;
    CHAEN : 0..1;
    CHCHN : 0..1;
    CHAED : 0..1;
    CHEN : 0..1;
    CHCHNS : 0..1;
    RESERVED1 : 0..63;
    CHBUSY : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  DCH0CONbits: TDCH0CONbits absolute ($BF883060);
  DCH0CONCLR : longWord absolute ($BF883064);
  DCH0CONSET : longWord absolute ($BF883068);
  DCH0CONINV : longWord absolute ($BF88306C);
  DCH0ECON : longWord absolute ($BF883070);
type
  TDCH0ECONbits = bitpacked record
  case integer of
  0 : (
    RESERVED0 : 0..7;
    AIRQEN : 0..1;
    SIRQEN : 0..1;
    PATEN : 0..1;
    CABORT : 0..1;
    CFORCE : 0..1;
    CHSIRQ : 0..255;
    CHAIRQ : 0..255;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  DCH0ECONbits: TDCH0ECONbits absolute ($BF883070);
  DCH0ECONCLR : longWord absolute ($BF883074);
  DCH0ECONSET : longWord absolute ($BF883078);
  DCH0ECONINV : longWord absolute ($BF88307C);
  DCH0INT : longWord absolute ($BF883080);
type
  TDCH0INTbits = bitpacked record
  case integer of
  0 : (
    CHERIF : 0..1;
    CHTAIF : 0..1;
    CHCCIF : 0..1;
    CHBCIF : 0..1;
    CHDHIF : 0..1;
    CHDDIF : 0..1;
    CHSHIF : 0..1;
    CHSDIF : 0..1;
    RESERVED0 : 0..255;
    CHERIE : 0..1;
    CHTAIE : 0..1;
    CHCCIE : 0..1;
    CHBCIE : 0..1;
    CHDHIE : 0..1;
    CHDDIE : 0..1;
    CHSHIE : 0..1;
    CHSDIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  DCH0INTbits: TDCH0INTbits absolute ($BF883080);
  DCH0INTCLR : longWord absolute ($BF883084);
  DCH0INTSET : longWord absolute ($BF883088);
  DCH0INTINV : longWord absolute ($BF88308C);
  DCH0SSA : longWord absolute ($BF883090);
  DCH0SSACLR : longWord absolute ($BF883094);
  DCH0SSASET : longWord absolute ($BF883098);
  DCH0SSAINV : longWord absolute ($BF88309C);
  DCH0DSA : longWord absolute ($BF8830A0);
  DCH0DSACLR : longWord absolute ($BF8830A4);
  DCH0DSASET : longWord absolute ($BF8830A8);
  DCH0DSAINV : longWord absolute ($BF8830AC);
  DCH0SSIZ : longWord absolute ($BF8830B0);
  DCH0SSIZCLR : longWord absolute ($BF8830B4);
  DCH0SSIZSET : longWord absolute ($BF8830B8);
  DCH0SSIZINV : longWord absolute ($BF8830BC);
  DCH0DSIZ : longWord absolute ($BF8830C0);
  DCH0DSIZCLR : longWord absolute ($BF8830C4);
  DCH0DSIZSET : longWord absolute ($BF8830C8);
  DCH0DSIZINV : longWord absolute ($BF8830CC);
  DCH0SPTR : longWord absolute ($BF8830D0);
  DCH0SPTRCLR : longWord absolute ($BF8830D4);
  DCH0SPTRSET : longWord absolute ($BF8830D8);
  DCH0SPTRINV : longWord absolute ($BF8830DC);
  DCH0DPTR : longWord absolute ($BF8830E0);
  DCH0DPTRCLR : longWord absolute ($BF8830E4);
  DCH0DPTRSET : longWord absolute ($BF8830E8);
  DCH0DPTRINV : longWord absolute ($BF8830EC);
  DCH0CSIZ : longWord absolute ($BF8830F0);
  DCH0CSIZCLR : longWord absolute ($BF8830F4);
  DCH0CSIZSET : longWord absolute ($BF8830F8);
  DCH0CSIZINV : longWord absolute ($BF8830FC);
  DCH0CPTR : longWord absolute ($BF883100);
  DCH0CPTRCLR : longWord absolute ($BF883104);
  DCH0CPTRSET : longWord absolute ($BF883108);
  DCH0CPTRINV : longWord absolute ($BF88310C);
  DCH0DAT : longWord absolute ($BF883110);
  DCH0DATCLR : longWord absolute ($BF883114);
  DCH0DATSET : longWord absolute ($BF883118);
  DCH0DATINV : longWord absolute ($BF88311C);
  DCH1CON : longWord absolute ($BF883120);
type
  TDCH1CONbits = bitpacked record
  case integer of
  0 : (
    CHPRI : 0..3;
    CHEDET : 0..1;
    RESERVED0 : 0..1;
    CHAEN : 0..1;
    CHCHN : 0..1;
    CHAED : 0..1;
    CHEN : 0..1;
    CHCHNS : 0..1;
    RESERVED1 : 0..63;
    CHBUSY : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  DCH1CONbits: TDCH1CONbits absolute ($BF883120);
  DCH1CONCLR : longWord absolute ($BF883124);
  DCH1CONSET : longWord absolute ($BF883128);
  DCH1CONINV : longWord absolute ($BF88312C);
  DCH1ECON : longWord absolute ($BF883130);
type
  TDCH1ECONbits = bitpacked record
  case integer of
  0 : (
    RESERVED0 : 0..7;
    AIRQEN : 0..1;
    SIRQEN : 0..1;
    PATEN : 0..1;
    CABORT : 0..1;
    CFORCE : 0..1;
    CHSIRQ : 0..255;
    CHAIRQ : 0..255;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  DCH1ECONbits: TDCH1ECONbits absolute ($BF883130);
  DCH1ECONCLR : longWord absolute ($BF883134);
  DCH1ECONSET : longWord absolute ($BF883138);
  DCH1ECONINV : longWord absolute ($BF88313C);
  DCH1INT : longWord absolute ($BF883140);
type
  TDCH1INTbits = bitpacked record
  case integer of
  0 : (
    CHERIF : 0..1;
    CHTAIF : 0..1;
    CHCCIF : 0..1;
    CHBCIF : 0..1;
    CHDHIF : 0..1;
    CHDDIF : 0..1;
    CHSHIF : 0..1;
    CHSDIF : 0..1;
    RESERVED0 : 0..255;
    CHERIE : 0..1;
    CHTAIE : 0..1;
    CHCCIE : 0..1;
    CHBCIE : 0..1;
    CHDHIE : 0..1;
    CHDDIE : 0..1;
    CHSHIE : 0..1;
    CHSDIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  DCH1INTbits: TDCH1INTbits absolute ($BF883140);
  DCH1INTCLR : longWord absolute ($BF883144);
  DCH1INTSET : longWord absolute ($BF883148);
  DCH1INTINV : longWord absolute ($BF88314C);
  DCH1SSA : longWord absolute ($BF883150);
  DCH1SSACLR : longWord absolute ($BF883154);
  DCH1SSASET : longWord absolute ($BF883158);
  DCH1SSAINV : longWord absolute ($BF88315C);
  DCH1DSA : longWord absolute ($BF883160);
  DCH1DSACLR : longWord absolute ($BF883164);
  DCH1DSASET : longWord absolute ($BF883168);
  DCH1DSAINV : longWord absolute ($BF88316C);
  DCH1SSIZ : longWord absolute ($BF883170);
  DCH1SSIZCLR : longWord absolute ($BF883174);
  DCH1SSIZSET : longWord absolute ($BF883178);
  DCH1SSIZINV : longWord absolute ($BF88317C);
  DCH1DSIZ : longWord absolute ($BF883180);
  DCH1DSIZCLR : longWord absolute ($BF883184);
  DCH1DSIZSET : longWord absolute ($BF883188);
  DCH1DSIZINV : longWord absolute ($BF88318C);
  DCH1SPTR : longWord absolute ($BF883190);
  DCH1SPTRCLR : longWord absolute ($BF883194);
  DCH1SPTRSET : longWord absolute ($BF883198);
  DCH1SPTRINV : longWord absolute ($BF88319C);
  DCH1DPTR : longWord absolute ($BF8831A0);
  DCH1DPTRCLR : longWord absolute ($BF8831A4);
  DCH1DPTRSET : longWord absolute ($BF8831A8);
  DCH1DPTRINV : longWord absolute ($BF8831AC);
  DCH1CSIZ : longWord absolute ($BF8831B0);
  DCH1CSIZCLR : longWord absolute ($BF8831B4);
  DCH1CSIZSET : longWord absolute ($BF8831B8);
  DCH1CSIZINV : longWord absolute ($BF8831BC);
  DCH1CPTR : longWord absolute ($BF8831C0);
  DCH1CPTRCLR : longWord absolute ($BF8831C4);
  DCH1CPTRSET : longWord absolute ($BF8831C8);
  DCH1CPTRINV : longWord absolute ($BF8831CC);
  DCH1DAT : longWord absolute ($BF8831D0);
  DCH1DATCLR : longWord absolute ($BF8831D4);
  DCH1DATSET : longWord absolute ($BF8831D8);
  DCH1DATINV : longWord absolute ($BF8831DC);
  DCH2CON : longWord absolute ($BF8831E0);
type
  TDCH2CONbits = bitpacked record
  case integer of
  0 : (
    CHPRI : 0..3;
    CHEDET : 0..1;
    RESERVED0 : 0..1;
    CHAEN : 0..1;
    CHCHN : 0..1;
    CHAED : 0..1;
    CHEN : 0..1;
    CHCHNS : 0..1;
    RESERVED1 : 0..63;
    CHBUSY : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  DCH2CONbits: TDCH2CONbits absolute ($BF8831E0);
  DCH2CONCLR : longWord absolute ($BF8831E4);
  DCH2CONSET : longWord absolute ($BF8831E8);
  DCH2CONINV : longWord absolute ($BF8831EC);
  DCH2ECON : longWord absolute ($BF8831F0);
type
  TDCH2ECONbits = bitpacked record
  case integer of
  0 : (
    RESERVED0 : 0..7;
    AIRQEN : 0..1;
    SIRQEN : 0..1;
    PATEN : 0..1;
    CABORT : 0..1;
    CFORCE : 0..1;
    CHSIRQ : 0..255;
    CHAIRQ : 0..255;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  DCH2ECONbits: TDCH2ECONbits absolute ($BF8831F0);
  DCH2ECONCLR : longWord absolute ($BF8831F4);
  DCH2ECONSET : longWord absolute ($BF8831F8);
  DCH2ECONINV : longWord absolute ($BF8831FC);
  DCH2INT : longWord absolute ($BF883200);
type
  TDCH2INTbits = bitpacked record
  case integer of
  0 : (
    CHERIF : 0..1;
    CHTAIF : 0..1;
    CHCCIF : 0..1;
    CHBCIF : 0..1;
    CHDHIF : 0..1;
    CHDDIF : 0..1;
    CHSHIF : 0..1;
    CHSDIF : 0..1;
    RESERVED0 : 0..255;
    CHERIE : 0..1;
    CHTAIE : 0..1;
    CHCCIE : 0..1;
    CHBCIE : 0..1;
    CHDHIE : 0..1;
    CHDDIE : 0..1;
    CHSHIE : 0..1;
    CHSDIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  DCH2INTbits: TDCH2INTbits absolute ($BF883200);
  DCH2INTCLR : longWord absolute ($BF883204);
  DCH2INTSET : longWord absolute ($BF883208);
  DCH2INTINV : longWord absolute ($BF88320C);
  DCH2SSA : longWord absolute ($BF883210);
  DCH2SSACLR : longWord absolute ($BF883214);
  DCH2SSASET : longWord absolute ($BF883218);
  DCH2SSAINV : longWord absolute ($BF88321C);
  DCH2DSA : longWord absolute ($BF883220);
  DCH2DSACLR : longWord absolute ($BF883224);
  DCH2DSASET : longWord absolute ($BF883228);
  DCH2DSAINV : longWord absolute ($BF88322C);
  DCH2SSIZ : longWord absolute ($BF883230);
  DCH2SSIZCLR : longWord absolute ($BF883234);
  DCH2SSIZSET : longWord absolute ($BF883238);
  DCH2SSIZINV : longWord absolute ($BF88323C);
  DCH2DSIZ : longWord absolute ($BF883240);
  DCH2DSIZCLR : longWord absolute ($BF883244);
  DCH2DSIZSET : longWord absolute ($BF883248);
  DCH2DSIZINV : longWord absolute ($BF88324C);
  DCH2SPTR : longWord absolute ($BF883250);
  DCH2SPTRCLR : longWord absolute ($BF883254);
  DCH2SPTRSET : longWord absolute ($BF883258);
  DCH2SPTRINV : longWord absolute ($BF88325C);
  DCH2DPTR : longWord absolute ($BF883260);
  DCH2DPTRCLR : longWord absolute ($BF883264);
  DCH2DPTRSET : longWord absolute ($BF883268);
  DCH2DPTRINV : longWord absolute ($BF88326C);
  DCH2CSIZ : longWord absolute ($BF883270);
  DCH2CSIZCLR : longWord absolute ($BF883274);
  DCH2CSIZSET : longWord absolute ($BF883278);
  DCH2CSIZINV : longWord absolute ($BF88327C);
  DCH2CPTR : longWord absolute ($BF883280);
  DCH2CPTRCLR : longWord absolute ($BF883284);
  DCH2CPTRSET : longWord absolute ($BF883288);
  DCH2CPTRINV : longWord absolute ($BF88328C);
  DCH2DAT : longWord absolute ($BF883290);
  DCH2DATCLR : longWord absolute ($BF883294);
  DCH2DATSET : longWord absolute ($BF883298);
  DCH2DATINV : longWord absolute ($BF88329C);
  DCH3CON : longWord absolute ($BF8832A0);
type
  TDCH3CONbits = bitpacked record
  case integer of
  0 : (
    CHPRI : 0..3;
    CHEDET : 0..1;
    RESERVED0 : 0..1;
    CHAEN : 0..1;
    CHCHN : 0..1;
    CHAED : 0..1;
    CHEN : 0..1;
    CHCHNS : 0..1;
    RESERVED1 : 0..63;
    CHBUSY : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  DCH3CONbits: TDCH3CONbits absolute ($BF8832A0);
  DCH3CONCLR : longWord absolute ($BF8832A4);
  DCH3CONSET : longWord absolute ($BF8832A8);
  DCH3CONINV : longWord absolute ($BF8832AC);
  DCH3ECON : longWord absolute ($BF8832B0);
type
  TDCH3ECONbits = bitpacked record
  case integer of
  0 : (
    RESERVED0 : 0..7;
    AIRQEN : 0..1;
    SIRQEN : 0..1;
    PATEN : 0..1;
    CABORT : 0..1;
    CFORCE : 0..1;
    CHSIRQ : 0..255;
    CHAIRQ : 0..255;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  DCH3ECONbits: TDCH3ECONbits absolute ($BF8832B0);
  DCH3ECONCLR : longWord absolute ($BF8832B4);
  DCH3ECONSET : longWord absolute ($BF8832B8);
  DCH3ECONINV : longWord absolute ($BF8832BC);
  DCH3INT : longWord absolute ($BF8832C0);
type
  TDCH3INTbits = bitpacked record
  case integer of
  0 : (
    CHERIF : 0..1;
    CHTAIF : 0..1;
    CHCCIF : 0..1;
    CHBCIF : 0..1;
    CHDHIF : 0..1;
    CHDDIF : 0..1;
    CHSHIF : 0..1;
    CHSDIF : 0..1;
    RESERVED0 : 0..255;
    CHERIE : 0..1;
    CHTAIE : 0..1;
    CHCCIE : 0..1;
    CHBCIE : 0..1;
    CHDHIE : 0..1;
    CHDDIE : 0..1;
    CHSHIE : 0..1;
    CHSDIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  DCH3INTbits: TDCH3INTbits absolute ($BF8832C0);
  DCH3INTCLR : longWord absolute ($BF8832C4);
  DCH3INTSET : longWord absolute ($BF8832C8);
  DCH3INTINV : longWord absolute ($BF8832CC);
  DCH3SSA : longWord absolute ($BF8832D0);
  DCH3SSACLR : longWord absolute ($BF8832D4);
  DCH3SSASET : longWord absolute ($BF8832D8);
  DCH3SSAINV : longWord absolute ($BF8832DC);
  DCH3DSA : longWord absolute ($BF8832E0);
  DCH3DSACLR : longWord absolute ($BF8832E4);
  DCH3DSASET : longWord absolute ($BF8832E8);
  DCH3DSAINV : longWord absolute ($BF8832EC);
  DCH3SSIZ : longWord absolute ($BF8832F0);
  DCH3SSIZCLR : longWord absolute ($BF8832F4);
  DCH3SSIZSET : longWord absolute ($BF8832F8);
  DCH3SSIZINV : longWord absolute ($BF8832FC);
  DCH3DSIZ : longWord absolute ($BF883300);
  DCH3DSIZCLR : longWord absolute ($BF883304);
  DCH3DSIZSET : longWord absolute ($BF883308);
  DCH3DSIZINV : longWord absolute ($BF88330C);
  DCH3SPTR : longWord absolute ($BF883310);
  DCH3SPTRCLR : longWord absolute ($BF883314);
  DCH3SPTRSET : longWord absolute ($BF883318);
  DCH3SPTRINV : longWord absolute ($BF88331C);
  DCH3DPTR : longWord absolute ($BF883320);
  DCH3DPTRCLR : longWord absolute ($BF883324);
  DCH3DPTRSET : longWord absolute ($BF883328);
  DCH3DPTRINV : longWord absolute ($BF88332C);
  DCH3CSIZ : longWord absolute ($BF883330);
  DCH3CSIZCLR : longWord absolute ($BF883334);
  DCH3CSIZSET : longWord absolute ($BF883338);
  DCH3CSIZINV : longWord absolute ($BF88333C);
  DCH3CPTR : longWord absolute ($BF883340);
  DCH3CPTRCLR : longWord absolute ($BF883344);
  DCH3CPTRSET : longWord absolute ($BF883348);
  DCH3CPTRINV : longWord absolute ($BF88334C);
  DCH3DAT : longWord absolute ($BF883350);
  DCH3DATCLR : longWord absolute ($BF883354);
  DCH3DATSET : longWord absolute ($BF883358);
  DCH3DATINV : longWord absolute ($BF88335C);
  DCH4CON : longWord absolute ($BF883360);
type
  TDCH4CONbits = bitpacked record
  case integer of
  0 : (
    CHPRI : 0..3;
    CHEDET : 0..1;
    RESERVED0 : 0..1;
    CHAEN : 0..1;
    CHCHN : 0..1;
    CHAED : 0..1;
    CHEN : 0..1;
    CHCHNS : 0..1;
    RESERVED1 : 0..63;
    CHBUSY : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  DCH4CONbits: TDCH4CONbits absolute ($BF883360);
  DCH4CONCLR : longWord absolute ($BF883364);
  DCH4CONSET : longWord absolute ($BF883368);
  DCH4CONINV : longWord absolute ($BF88336C);
  DCH4ECON : longWord absolute ($BF883370);
type
  TDCH4ECONbits = bitpacked record
  case integer of
  0 : (
    RESERVED0 : 0..7;
    AIRQEN : 0..1;
    SIRQEN : 0..1;
    PATEN : 0..1;
    CABORT : 0..1;
    CFORCE : 0..1;
    CHSIRQ : 0..255;
    CHAIRQ : 0..255;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  DCH4ECONbits: TDCH4ECONbits absolute ($BF883370);
  DCH4ECONCLR : longWord absolute ($BF883374);
  DCH4ECONSET : longWord absolute ($BF883378);
  DCH4ECONINV : longWord absolute ($BF88337C);
  DCH4INT : longWord absolute ($BF883380);
type
  TDCH4INTbits = bitpacked record
  case integer of
  0 : (
    CHERIF : 0..1;
    CHTAIF : 0..1;
    CHCCIF : 0..1;
    CHBCIF : 0..1;
    CHDHIF : 0..1;
    CHDDIF : 0..1;
    CHSHIF : 0..1;
    CHSDIF : 0..1;
    RESERVED0 : 0..255;
    CHERIE : 0..1;
    CHTAIE : 0..1;
    CHCCIE : 0..1;
    CHBCIE : 0..1;
    CHDHIE : 0..1;
    CHDDIE : 0..1;
    CHSHIE : 0..1;
    CHSDIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  DCH4INTbits: TDCH4INTbits absolute ($BF883380);
  DCH4INTCLR : longWord absolute ($BF883384);
  DCH4INTSET : longWord absolute ($BF883388);
  DCH4INTINV : longWord absolute ($BF88338C);
  DCH4SSA : longWord absolute ($BF883390);
  DCH4SSACLR : longWord absolute ($BF883394);
  DCH4SSASET : longWord absolute ($BF883398);
  DCH4SSAINV : longWord absolute ($BF88339C);
  DCH4DSA : longWord absolute ($BF8833A0);
  DCH4DSACLR : longWord absolute ($BF8833A4);
  DCH4DSASET : longWord absolute ($BF8833A8);
  DCH4DSAINV : longWord absolute ($BF8833AC);
  DCH4SSIZ : longWord absolute ($BF8833B0);
  DCH4SSIZCLR : longWord absolute ($BF8833B4);
  DCH4SSIZSET : longWord absolute ($BF8833B8);
  DCH4SSIZINV : longWord absolute ($BF8833BC);
  DCH4DSIZ : longWord absolute ($BF8833C0);
  DCH4DSIZCLR : longWord absolute ($BF8833C4);
  DCH4DSIZSET : longWord absolute ($BF8833C8);
  DCH4DSIZINV : longWord absolute ($BF8833CC);
  DCH4SPTR : longWord absolute ($BF8833D0);
  DCH4SPTRCLR : longWord absolute ($BF8833D4);
  DCH4SPTRSET : longWord absolute ($BF8833D8);
  DCH4SPTRINV : longWord absolute ($BF8833DC);
  DCH4DPTR : longWord absolute ($BF8833E0);
  DCH4DPTRCLR : longWord absolute ($BF8833E4);
  DCH4DPTRSET : longWord absolute ($BF8833E8);
  DCH4DPTRINV : longWord absolute ($BF8833EC);
  DCH4CSIZ : longWord absolute ($BF8833F0);
  DCH4CSIZCLR : longWord absolute ($BF8833F4);
  DCH4CSIZSET : longWord absolute ($BF8833F8);
  DCH4CSIZINV : longWord absolute ($BF8833FC);
  DCH4CPTR : longWord absolute ($BF883400);
  DCH4CPTRCLR : longWord absolute ($BF883404);
  DCH4CPTRSET : longWord absolute ($BF883408);
  DCH4CPTRINV : longWord absolute ($BF88340C);
  DCH4DAT : longWord absolute ($BF883410);
  DCH4DATCLR : longWord absolute ($BF883414);
  DCH4DATSET : longWord absolute ($BF883418);
  DCH4DATINV : longWord absolute ($BF88341C);
  DCH5CON : longWord absolute ($BF883420);
type
  TDCH5CONbits = bitpacked record
  case integer of
  0 : (
    CHPRI : 0..3;
    CHEDET : 0..1;
    RESERVED0 : 0..1;
    CHAEN : 0..1;
    CHCHN : 0..1;
    CHAED : 0..1;
    CHEN : 0..1;
    CHCHNS : 0..1;
    RESERVED1 : 0..63;
    CHBUSY : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  DCH5CONbits: TDCH5CONbits absolute ($BF883420);
  DCH5CONCLR : longWord absolute ($BF883424);
  DCH5CONSET : longWord absolute ($BF883428);
  DCH5CONINV : longWord absolute ($BF88342C);
  DCH5ECON : longWord absolute ($BF883430);
type
  TDCH5ECONbits = bitpacked record
  case integer of
  0 : (
    RESERVED0 : 0..7;
    AIRQEN : 0..1;
    SIRQEN : 0..1;
    PATEN : 0..1;
    CABORT : 0..1;
    CFORCE : 0..1;
    CHSIRQ : 0..255;
    CHAIRQ : 0..255;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  DCH5ECONbits: TDCH5ECONbits absolute ($BF883430);
  DCH5ECONCLR : longWord absolute ($BF883434);
  DCH5ECONSET : longWord absolute ($BF883438);
  DCH5ECONINV : longWord absolute ($BF88343C);
  DCH5INT : longWord absolute ($BF883440);
type
  TDCH5INTbits = bitpacked record
  case integer of
  0 : (
    CHERIF : 0..1;
    CHTAIF : 0..1;
    CHCCIF : 0..1;
    CHBCIF : 0..1;
    CHDHIF : 0..1;
    CHDDIF : 0..1;
    CHSHIF : 0..1;
    CHSDIF : 0..1;
    RESERVED0 : 0..255;
    CHERIE : 0..1;
    CHTAIE : 0..1;
    CHCCIE : 0..1;
    CHBCIE : 0..1;
    CHDHIE : 0..1;
    CHDDIE : 0..1;
    CHSHIE : 0..1;
    CHSDIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  DCH5INTbits: TDCH5INTbits absolute ($BF883440);
  DCH5INTCLR : longWord absolute ($BF883444);
  DCH5INTSET : longWord absolute ($BF883448);
  DCH5INTINV : longWord absolute ($BF88344C);
  DCH5SSA : longWord absolute ($BF883450);
  DCH5SSACLR : longWord absolute ($BF883454);
  DCH5SSASET : longWord absolute ($BF883458);
  DCH5SSAINV : longWord absolute ($BF88345C);
  DCH5DSA : longWord absolute ($BF883460);
  DCH5DSACLR : longWord absolute ($BF883464);
  DCH5DSASET : longWord absolute ($BF883468);
  DCH5DSAINV : longWord absolute ($BF88346C);
  DCH5SSIZ : longWord absolute ($BF883470);
  DCH5SSIZCLR : longWord absolute ($BF883474);
  DCH5SSIZSET : longWord absolute ($BF883478);
  DCH5SSIZINV : longWord absolute ($BF88347C);
  DCH5DSIZ : longWord absolute ($BF883480);
  DCH5DSIZCLR : longWord absolute ($BF883484);
  DCH5DSIZSET : longWord absolute ($BF883488);
  DCH5DSIZINV : longWord absolute ($BF88348C);
  DCH5SPTR : longWord absolute ($BF883490);
  DCH5SPTRCLR : longWord absolute ($BF883494);
  DCH5SPTRSET : longWord absolute ($BF883498);
  DCH5SPTRINV : longWord absolute ($BF88349C);
  DCH5DPTR : longWord absolute ($BF8834A0);
  DCH5DPTRCLR : longWord absolute ($BF8834A4);
  DCH5DPTRSET : longWord absolute ($BF8834A8);
  DCH5DPTRINV : longWord absolute ($BF8834AC);
  DCH5CSIZ : longWord absolute ($BF8834B0);
  DCH5CSIZCLR : longWord absolute ($BF8834B4);
  DCH5CSIZSET : longWord absolute ($BF8834B8);
  DCH5CSIZINV : longWord absolute ($BF8834BC);
  DCH5CPTR : longWord absolute ($BF8834C0);
  DCH5CPTRCLR : longWord absolute ($BF8834C4);
  DCH5CPTRSET : longWord absolute ($BF8834C8);
  DCH5CPTRINV : longWord absolute ($BF8834CC);
  DCH5DAT : longWord absolute ($BF8834D0);
  DCH5DATCLR : longWord absolute ($BF8834D4);
  DCH5DATSET : longWord absolute ($BF8834D8);
  DCH5DATINV : longWord absolute ($BF8834DC);
  DCH6CON : longWord absolute ($BF8834E0);
type
  TDCH6CONbits = bitpacked record
  case integer of
  0 : (
    CHPRI : 0..3;
    CHEDET : 0..1;
    RESERVED0 : 0..1;
    CHAEN : 0..1;
    CHCHN : 0..1;
    CHAED : 0..1;
    CHEN : 0..1;
    CHCHNS : 0..1;
    RESERVED1 : 0..63;
    CHBUSY : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  DCH6CONbits: TDCH6CONbits absolute ($BF8834E0);
  DCH6CONCLR : longWord absolute ($BF8834E4);
  DCH6CONSET : longWord absolute ($BF8834E8);
  DCH6CONINV : longWord absolute ($BF8834EC);
  DCH6ECON : longWord absolute ($BF8834F0);
type
  TDCH6ECONbits = bitpacked record
  case integer of
  0 : (
    RESERVED0 : 0..7;
    AIRQEN : 0..1;
    SIRQEN : 0..1;
    PATEN : 0..1;
    CABORT : 0..1;
    CFORCE : 0..1;
    CHSIRQ : 0..255;
    CHAIRQ : 0..255;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  DCH6ECONbits: TDCH6ECONbits absolute ($BF8834F0);
  DCH6ECONCLR : longWord absolute ($BF8834F4);
  DCH6ECONSET : longWord absolute ($BF8834F8);
  DCH6ECONINV : longWord absolute ($BF8834FC);
  DCH6INT : longWord absolute ($BF883500);
type
  TDCH6INTbits = bitpacked record
  case integer of
  0 : (
    CHERIF : 0..1;
    CHTAIF : 0..1;
    CHCCIF : 0..1;
    CHBCIF : 0..1;
    CHDHIF : 0..1;
    CHDDIF : 0..1;
    CHSHIF : 0..1;
    CHSDIF : 0..1;
    RESERVED0 : 0..255;
    CHERIE : 0..1;
    CHTAIE : 0..1;
    CHCCIE : 0..1;
    CHBCIE : 0..1;
    CHDHIE : 0..1;
    CHDDIE : 0..1;
    CHSHIE : 0..1;
    CHSDIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  DCH6INTbits: TDCH6INTbits absolute ($BF883500);
  DCH6INTCLR : longWord absolute ($BF883504);
  DCH6INTSET : longWord absolute ($BF883508);
  DCH6INTINV : longWord absolute ($BF88350C);
  DCH6SSA : longWord absolute ($BF883510);
  DCH6SSACLR : longWord absolute ($BF883514);
  DCH6SSASET : longWord absolute ($BF883518);
  DCH6SSAINV : longWord absolute ($BF88351C);
  DCH6DSA : longWord absolute ($BF883520);
  DCH6DSACLR : longWord absolute ($BF883524);
  DCH6DSASET : longWord absolute ($BF883528);
  DCH6DSAINV : longWord absolute ($BF88352C);
  DCH6SSIZ : longWord absolute ($BF883530);
  DCH6SSIZCLR : longWord absolute ($BF883534);
  DCH6SSIZSET : longWord absolute ($BF883538);
  DCH6SSIZINV : longWord absolute ($BF88353C);
  DCH6DSIZ : longWord absolute ($BF883540);
  DCH6DSIZCLR : longWord absolute ($BF883544);
  DCH6DSIZSET : longWord absolute ($BF883548);
  DCH6DSIZINV : longWord absolute ($BF88354C);
  DCH6SPTR : longWord absolute ($BF883550);
  DCH6SPTRCLR : longWord absolute ($BF883554);
  DCH6SPTRSET : longWord absolute ($BF883558);
  DCH6SPTRINV : longWord absolute ($BF88355C);
  DCH6DPTR : longWord absolute ($BF883560);
  DCH6DPTRCLR : longWord absolute ($BF883564);
  DCH6DPTRSET : longWord absolute ($BF883568);
  DCH6DPTRINV : longWord absolute ($BF88356C);
  DCH6CSIZ : longWord absolute ($BF883570);
  DCH6CSIZCLR : longWord absolute ($BF883574);
  DCH6CSIZSET : longWord absolute ($BF883578);
  DCH6CSIZINV : longWord absolute ($BF88357C);
  DCH6CPTR : longWord absolute ($BF883580);
  DCH6CPTRCLR : longWord absolute ($BF883584);
  DCH6CPTRSET : longWord absolute ($BF883588);
  DCH6CPTRINV : longWord absolute ($BF88358C);
  DCH6DAT : longWord absolute ($BF883590);
  DCH6DATCLR : longWord absolute ($BF883594);
  DCH6DATSET : longWord absolute ($BF883598);
  DCH6DATINV : longWord absolute ($BF88359C);
  DCH7CON : longWord absolute ($BF8835A0);
type
  TDCH7CONbits = bitpacked record
  case integer of
  0 : (
    CHPRI : 0..3;
    CHEDET : 0..1;
    RESERVED0 : 0..1;
    CHAEN : 0..1;
    CHCHN : 0..1;
    CHAED : 0..1;
    CHEN : 0..1;
    CHCHNS : 0..1;
    RESERVED1 : 0..63;
    CHBUSY : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  DCH7CONbits: TDCH7CONbits absolute ($BF8835A0);
  DCH7CONCLR : longWord absolute ($BF8835A4);
  DCH7CONSET : longWord absolute ($BF8835A8);
  DCH7CONINV : longWord absolute ($BF8835AC);
  DCH7ECON : longWord absolute ($BF8835B0);
type
  TDCH7ECONbits = bitpacked record
  case integer of
  0 : (
    RESERVED0 : 0..7;
    AIRQEN : 0..1;
    SIRQEN : 0..1;
    PATEN : 0..1;
    CABORT : 0..1;
    CFORCE : 0..1;
    CHSIRQ : 0..255;
    CHAIRQ : 0..255;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  DCH7ECONbits: TDCH7ECONbits absolute ($BF8835B0);
  DCH7ECONCLR : longWord absolute ($BF8835B4);
  DCH7ECONSET : longWord absolute ($BF8835B8);
  DCH7ECONINV : longWord absolute ($BF8835BC);
  DCH7INT : longWord absolute ($BF8835C0);
type
  TDCH7INTbits = bitpacked record
  case integer of
  0 : (
    CHERIF : 0..1;
    CHTAIF : 0..1;
    CHCCIF : 0..1;
    CHBCIF : 0..1;
    CHDHIF : 0..1;
    CHDDIF : 0..1;
    CHSHIF : 0..1;
    CHSDIF : 0..1;
    RESERVED0 : 0..255;
    CHERIE : 0..1;
    CHTAIE : 0..1;
    CHCCIE : 0..1;
    CHBCIE : 0..1;
    CHDHIE : 0..1;
    CHDDIE : 0..1;
    CHSHIE : 0..1;
    CHSDIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  DCH7INTbits: TDCH7INTbits absolute ($BF8835C0);
  DCH7INTCLR : longWord absolute ($BF8835C4);
  DCH7INTSET : longWord absolute ($BF8835C8);
  DCH7INTINV : longWord absolute ($BF8835CC);
  DCH7SSA : longWord absolute ($BF8835D0);
  DCH7SSACLR : longWord absolute ($BF8835D4);
  DCH7SSASET : longWord absolute ($BF8835D8);
  DCH7SSAINV : longWord absolute ($BF8835DC);
  DCH7DSA : longWord absolute ($BF8835E0);
  DCH7DSACLR : longWord absolute ($BF8835E4);
  DCH7DSASET : longWord absolute ($BF8835E8);
  DCH7DSAINV : longWord absolute ($BF8835EC);
  DCH7SSIZ : longWord absolute ($BF8835F0);
  DCH7SSIZCLR : longWord absolute ($BF8835F4);
  DCH7SSIZSET : longWord absolute ($BF8835F8);
  DCH7SSIZINV : longWord absolute ($BF8835FC);
  DCH7DSIZ : longWord absolute ($BF883600);
  DCH7DSIZCLR : longWord absolute ($BF883604);
  DCH7DSIZSET : longWord absolute ($BF883608);
  DCH7DSIZINV : longWord absolute ($BF88360C);
  DCH7SPTR : longWord absolute ($BF883610);
  DCH7SPTRCLR : longWord absolute ($BF883614);
  DCH7SPTRSET : longWord absolute ($BF883618);
  DCH7SPTRINV : longWord absolute ($BF88361C);
  DCH7DPTR : longWord absolute ($BF883620);
  DCH7DPTRCLR : longWord absolute ($BF883624);
  DCH7DPTRSET : longWord absolute ($BF883628);
  DCH7DPTRINV : longWord absolute ($BF88362C);
  DCH7CSIZ : longWord absolute ($BF883630);
  DCH7CSIZCLR : longWord absolute ($BF883634);
  DCH7CSIZSET : longWord absolute ($BF883638);
  DCH7CSIZINV : longWord absolute ($BF88363C);
  DCH7CPTR : longWord absolute ($BF883640);
  DCH7CPTRCLR : longWord absolute ($BF883644);
  DCH7CPTRSET : longWord absolute ($BF883648);
  DCH7CPTRINV : longWord absolute ($BF88364C);
  DCH7DAT : longWord absolute ($BF883650);
  DCH7DATCLR : longWord absolute ($BF883654);
  DCH7DATSET : longWord absolute ($BF883658);
  DCH7DATINV : longWord absolute ($BF88365C);
  CHECON : longWord absolute ($BF884000);
type
  TCHECONbits = bitpacked record
  case integer of
  0 : (
    PFMWS : 0..7;
    RESERVED0 : 0..1;
    PREFEN : 0..3;
    RESERVED1 : 0..3;
    DCSZ : 0..3;
    RESERVED2 : 0..63;
    CHECOH : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  CHECONbits: TCHECONbits absolute ($BF884000);
  CHECONCLR : longWord absolute ($BF884004);
  CHECONSET : longWord absolute ($BF884008);
  CHECONINV : longWord absolute ($BF88400C);
  CHEACC : longWord absolute ($BF884010);
type
  TCHEACCbits = bitpacked record
    CHEIDX : 0..15;
    RESERVED0 : 0..134217727;
    CHEWEN : 0..1;
  end;
var
  CHEACCbits: TCHEACCbits absolute ($BF884010);
  CHEACCCLR : longWord absolute ($BF884014);
  CHEACCSET : longWord absolute ($BF884018);
  CHEACCINV : longWord absolute ($BF88401C);
  CHETAG : longWord absolute ($BF884020);
type
  TCHETAGbits = bitpacked record
  case integer of
  0 : (
    RESERVED0 : 0..1;
    LTYPE : 0..1;
    LLOCK : 0..1;
    LVALID : 0..1;
    LTAG : 0..1048575;
    RESERVED1 : 0..127;
    LTAGBOOT : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  CHETAGbits: TCHETAGbits absolute ($BF884020);
  CHETAGCLR : longWord absolute ($BF884024);
  CHETAGSET : longWord absolute ($BF884028);
  CHETAGINV : longWord absolute ($BF88402C);
  CHEMSK : longWord absolute ($BF884030);
type
  TCHEMSKbits = bitpacked record
    RESERVED0 : 0..31;
    LMASK : 0..2047;
  end;
var
  CHEMSKbits: TCHEMSKbits absolute ($BF884030);
  CHEMSKCLR : longWord absolute ($BF884034);
  CHEMSKSET : longWord absolute ($BF884038);
  CHEMSKINV : longWord absolute ($BF88403C);
  CHEW0 : longWord absolute ($BF884040);
  CHEW1 : longWord absolute ($BF884050);
  CHEW2 : longWord absolute ($BF884060);
  CHEW3 : longWord absolute ($BF884070);
  CHELRU : longWord absolute ($BF884080);
type
  TCHELRUbits = bitpacked record
    CHELRU : 0..33554431;
  end;
var
  CHELRUbits: TCHELRUbits absolute ($BF884080);
  CHEHIT : longWord absolute ($BF884090);
  CHEMIS : longWord absolute ($BF8840A0);
  CHEPFABT : longWord absolute ($BF8840C0);
  U1OTGIR : longWord absolute ($BF885040);
type
  TU1OTGIRbits = bitpacked record
    VBUSVDIF : 0..1;
    RESERVED0 : 0..1;
    SESENDIF : 0..1;
    SESVDIF : 0..1;
    ACTVIF : 0..1;
    LSTATEIF : 0..1;
    T1MSECIF : 0..1;
    IDIF : 0..1;
  end;
var
  U1OTGIRbits: TU1OTGIRbits absolute ($BF885040);
  U1OTGIRCLR : longWord absolute ($BF885044);
  U1OTGIE : longWord absolute ($BF885050);
type
  TU1OTGIEbits = bitpacked record
    VBUSVDIE : 0..1;
    RESERVED0 : 0..1;
    SESENDIE : 0..1;
    SESVDIE : 0..1;
    ACTVIE : 0..1;
    LSTATEIE : 0..1;
    T1MSECIE : 0..1;
    IDIE : 0..1;
  end;
var
  U1OTGIEbits: TU1OTGIEbits absolute ($BF885050);
  U1OTGIECLR : longWord absolute ($BF885054);
  U1OTGIESET : longWord absolute ($BF885058);
  U1OTGIEINV : longWord absolute ($BF88505C);
  U1OTGSTAT : longWord absolute ($BF885060);
type
  TU1OTGSTATbits = bitpacked record
    VBUSVD : 0..1;
    RESERVED0 : 0..1;
    SESEND : 0..1;
    SESVD : 0..1;
    RESERVED1 : 0..1;
    LSTATE : 0..1;
    RESERVED2 : 0..1;
    ID : 0..1;
  end;
var
  U1OTGSTATbits: TU1OTGSTATbits absolute ($BF885060);
  U1OTGCON : longWord absolute ($BF885070);
type
  TU1OTGCONbits = bitpacked record
    VBUSDIS : 0..1;
    VBUSCHG : 0..1;
    OTGEN : 0..1;
    VBUSON : 0..1;
    DMPULDWN : 0..1;
    DPPULDWN : 0..1;
    DMPULUP : 0..1;
    DPPULUP : 0..1;
  end;
var
  U1OTGCONbits: TU1OTGCONbits absolute ($BF885070);
  U1OTGCONCLR : longWord absolute ($BF885074);
  U1OTGCONSET : longWord absolute ($BF885078);
  U1OTGCONINV : longWord absolute ($BF88507C);
  U1PWRC : longWord absolute ($BF885080);
type
  TU1PWRCbits = bitpacked record
    USBPWR : 0..1;
    USUSPEND : 0..1;
    RESERVED0 : 0..1;
    USBBUSY : 0..1;
    USLPGRD : 0..1;
    RESERVED1 : 0..3;
    UACTPND : 0..1;
  end;
var
  U1PWRCbits: TU1PWRCbits absolute ($BF885080);
  U1PWRCCLR : longWord absolute ($BF885084);
  U1PWRCSET : longWord absolute ($BF885088);
  U1PWRCINV : longWord absolute ($BF88508C);
  U1IR : longWord absolute ($BF885200);
type
  TU1IRbits = bitpacked record
  case integer of
  0 : (
    URSTIF_DETACHIF : 0..1;
    UERRIF : 0..1;
    SOFIF : 0..1;
    TRNIF : 0..1;
    IDLEIF : 0..1;
    RESUMEIF : 0..1;
    ATTACHIF : 0..1;
    STALLIF : 0..1;
  );
  1 : (
    DETACHIF : 0..1;
  );
  2 : (
    URSTIF : 0..1;
  );
  end;
var
  U1IRbits: TU1IRbits absolute ($BF885200);
  U1IRCLR : longWord absolute ($BF885204);
  U1IE : longWord absolute ($BF885210);
type
  TU1IEbits = bitpacked record
  case integer of
  0 : (
    URSTIE_DETACHIE : 0..1;
    UERRIE : 0..1;
    SOFIE : 0..1;
    TRNIE : 0..1;
    IDLEIE : 0..1;
    RESUMEIE : 0..1;
    ATTACHIE : 0..1;
    STALLIE : 0..1;
  );
  1 : (
    DETACHIE : 0..1;
  );
  2 : (
    URSTIE : 0..1;
  );
  end;
var
  U1IEbits: TU1IEbits absolute ($BF885210);
  U1IECLR : longWord absolute ($BF885214);
  U1IESET : longWord absolute ($BF885218);
  U1IEINV : longWord absolute ($BF88521C);
  U1EIR : longWord absolute ($BF885220);
type
  TU1EIRbits = bitpacked record
  case integer of
  0 : (
    PIDEF : 0..1;
    CRC5EF_EOFEF : 0..1;
    CRC16EF : 0..1;
    DFN8EF : 0..1;
    BTOEF : 0..1;
    DMAEF : 0..1;
    BMXEF : 0..1;
    BTSEF : 0..1;
  );
  1 : (
    RESERVED0 : 0..1;
    CRC5EF : 0..1;
  );
  2 : (
    RESERVED1 : 0..1;
    EOFEF : 0..1;
  );
  end;
var
  U1EIRbits: TU1EIRbits absolute ($BF885220);
  U1EIRCLR : longWord absolute ($BF885224);
  U1EIE : longWord absolute ($BF885230);
type
  TU1EIEbits = bitpacked record
  case integer of
  0 : (
    PIDEE : 0..1;
    CRC5EE_EOFEE : 0..1;
    CRC16EE : 0..1;
    DFN8EE : 0..1;
    BTOEE : 0..1;
    DMAEE : 0..1;
    BMXEE : 0..1;
    BTSEE : 0..1;
  );
  1 : (
    RESERVED0 : 0..1;
    CRC5EE : 0..1;
  );
  2 : (
    RESERVED1 : 0..1;
    EOFEE : 0..1;
  );
  end;
var
  U1EIEbits: TU1EIEbits absolute ($BF885230);
  U1EIECLR : longWord absolute ($BF885234);
  U1EIESET : longWord absolute ($BF885238);
  U1EIEINV : longWord absolute ($BF88523C);
  U1STAT : longWord absolute ($BF885240);
type
  TU1STATbits = bitpacked record
  case integer of
  0 : (
    RESERVED0 : 0..3;
    PPBI : 0..1;
    DIR : 0..1;
    ENDPT : 0..15;
  );
  1 : (
    RESERVED1 : 0..15;
    ENDPT0 : 0..1;
    ENDPT1 : 0..1;
    ENDPT2 : 0..1;
    ENDPT3 : 0..1;
  );
  end;
var
  U1STATbits: TU1STATbits absolute ($BF885240);
  U1CON : longWord absolute ($BF885250);
type
  TU1CONbits = bitpacked record
  case integer of
  0 : (
    USBEN_SOFEN : 0..1;
    PPBRST : 0..1;
    RESUME : 0..1;
    HOSTEN : 0..1;
    USBRST : 0..1;
    PKTDIS_TOKBUSY : 0..1;
    SE0 : 0..1;
    JSTATE : 0..1;
  );
  1 : (
    USBEN : 0..1;
  );
  2 : (
    SOFEN : 0..1;
    RESERVED0 : 0..15;
    PKTDIS : 0..1;
  );
  3 : (
    RESERVED1 : 0..31;
    TOKBUSY : 0..1;
  );
  end;
var
  U1CONbits: TU1CONbits absolute ($BF885250);
  U1CONCLR : longWord absolute ($BF885254);
  U1CONSET : longWord absolute ($BF885258);
  U1CONINV : longWord absolute ($BF88525C);
  U1ADDR : longWord absolute ($BF885260);
type
  TU1ADDRbits = bitpacked record
  case integer of
  0 : (
    DEVADDR : 0..127;
    LSPDEN : 0..1;
  );
  1 : (
    DEVADDR0 : 0..1;
    DEVADDR1 : 0..1;
    DEVADDR2 : 0..1;
    DEVADDR3 : 0..1;
    DEVADDR4 : 0..1;
    DEVADDR5 : 0..1;
    DEVADDR6 : 0..1;
  );
  end;
var
  U1ADDRbits: TU1ADDRbits absolute ($BF885260);
  U1ADDRCLR : longWord absolute ($BF885264);
  U1ADDRSET : longWord absolute ($BF885268);
  U1ADDRINV : longWord absolute ($BF88526C);
  U1BDTP1 : longWord absolute ($BF885270);
type
  TU1BDTP1bits = bitpacked record
    RESERVED0 : 0..1;
    BDTPTRL : 0..127;
  end;
var
  U1BDTP1bits: TU1BDTP1bits absolute ($BF885270);
  U1BDTP1CLR : longWord absolute ($BF885274);
  U1BDTP1SET : longWord absolute ($BF885278);
  U1BDTP1INV : longWord absolute ($BF88527C);
  U1FRML : longWord absolute ($BF885280);
type
  TU1FRMLbits = bitpacked record
  case integer of
  0 : (
    FRML : 0..255;
  );
  1 : (
    FRM0 : 0..1;
    FRM1 : 0..1;
    FRM2 : 0..1;
    FRM3 : 0..1;
    FRM4 : 0..1;
    FRM5 : 0..1;
    FRM6 : 0..1;
    FRM7 : 0..1;
  );
  end;
var
  U1FRMLbits: TU1FRMLbits absolute ($BF885280);
  U1FRMH : longWord absolute ($BF885290);
type
  TU1FRMHbits = bitpacked record
  case integer of
  0 : (
    FRMH : 0..7;
  );
  1 : (
    FRM8 : 0..1;
    FRM9 : 0..1;
    FRM10 : 0..1;
  );
  end;
var
  U1FRMHbits: TU1FRMHbits absolute ($BF885290);
  U1TOK : longWord absolute ($BF8852A0);
type
  TU1TOKbits = bitpacked record
  case integer of
  0 : (
    EP : 0..15;
    PID : 0..15;
  );
  1 : (
    EP0 : 0..1;
  );
  2 : (
    RESERVED0 : 0..1;
    EP1 : 0..1;
    EP2 : 0..1;
    EP3 : 0..1;
    PID0 : 0..1;
  );
  3 : (
    RESERVED1 : 0..31;
    PID1 : 0..1;
    PID2 : 0..1;
    PID3 : 0..1;
  );
  end;
var
  U1TOKbits: TU1TOKbits absolute ($BF8852A0);
  U1TOKCLR : longWord absolute ($BF8852A4);
  U1TOKSET : longWord absolute ($BF8852A8);
  U1TOKINV : longWord absolute ($BF8852AC);
  U1SOF : longWord absolute ($BF8852B0);
type
  TU1SOFbits = bitpacked record
    CNT : 0..255;
  end;
var
  U1SOFbits: TU1SOFbits absolute ($BF8852B0);
  U1SOFCLR : longWord absolute ($BF8852B4);
  U1SOFSET : longWord absolute ($BF8852B8);
  U1SOFINV : longWord absolute ($BF8852BC);
  U1BDTP2 : longWord absolute ($BF8852C0);
type
  TU1BDTP2bits = bitpacked record
    BDTPTRH : 0..255;
  end;
var
  U1BDTP2bits: TU1BDTP2bits absolute ($BF8852C0);
  U1BDTP2CLR : longWord absolute ($BF8852C4);
  U1BDTP2SET : longWord absolute ($BF8852C8);
  U1BDTP2INV : longWord absolute ($BF8852CC);
  U1BDTP3 : longWord absolute ($BF8852D0);
type
  TU1BDTP3bits = bitpacked record
    BDTPTRU : 0..255;
  end;
var
  U1BDTP3bits: TU1BDTP3bits absolute ($BF8852D0);
  U1BDTP3CLR : longWord absolute ($BF8852D4);
  U1BDTP3SET : longWord absolute ($BF8852D8);
  U1BDTP3INV : longWord absolute ($BF8852DC);
  U1CNFG1 : longWord absolute ($BF8852E0);
type
  TU1CNFG1bits = bitpacked record
    UASUSPND : 0..1;
    RESERVED0 : 0..7;
    USBSIDL : 0..1;
    RESERVED1 : 0..1;
    UOEMON : 0..1;
    UTEYE : 0..1;
  end;
var
  U1CNFG1bits: TU1CNFG1bits absolute ($BF8852E0);
  U1CNFG1CLR : longWord absolute ($BF8852E4);
  U1CNFG1SET : longWord absolute ($BF8852E8);
  U1CNFG1INV : longWord absolute ($BF8852EC);
  U1EP0 : longWord absolute ($BF885300);
type
  TU1EP0bits = bitpacked record
    EPHSHK : 0..1;
    EPSTALL : 0..1;
    EPTXEN : 0..1;
    EPRXEN : 0..1;
    EPCONDIS : 0..1;
    RESERVED0 : 0..1;
    RETRYDIS : 0..1;
    LSPD : 0..1;
  end;
var
  U1EP0bits: TU1EP0bits absolute ($BF885300);
  U1EP0CLR : longWord absolute ($BF885304);
  U1EP0SET : longWord absolute ($BF885308);
  U1EP0INV : longWord absolute ($BF88530C);
  U1EP1 : longWord absolute ($BF885310);
type
  TU1EP1bits = bitpacked record
    EPHSHK : 0..1;
    EPSTALL : 0..1;
    EPTXEN : 0..1;
    EPRXEN : 0..1;
    EPCONDIS : 0..1;
  end;
var
  U1EP1bits: TU1EP1bits absolute ($BF885310);
  U1EP1CLR : longWord absolute ($BF885314);
  U1EP1SET : longWord absolute ($BF885318);
  U1EP1INV : longWord absolute ($BF88531C);
  U1EP2 : longWord absolute ($BF885320);
type
  TU1EP2bits = bitpacked record
    EPHSHK : 0..1;
    EPSTALL : 0..1;
    EPTXEN : 0..1;
    EPRXEN : 0..1;
    EPCONDIS : 0..1;
  end;
var
  U1EP2bits: TU1EP2bits absolute ($BF885320);
  U1EP2CLR : longWord absolute ($BF885324);
  U1EP2SET : longWord absolute ($BF885328);
  U1EP2INV : longWord absolute ($BF88532C);
  U1EP3 : longWord absolute ($BF885330);
type
  TU1EP3bits = bitpacked record
    EPHSHK : 0..1;
    EPSTALL : 0..1;
    EPTXEN : 0..1;
    EPRXEN : 0..1;
    EPCONDIS : 0..1;
  end;
var
  U1EP3bits: TU1EP3bits absolute ($BF885330);
  U1EP3CLR : longWord absolute ($BF885334);
  U1EP3SET : longWord absolute ($BF885338);
  U1EP3INV : longWord absolute ($BF88533C);
  U1EP4 : longWord absolute ($BF885340);
type
  TU1EP4bits = bitpacked record
    EPHSHK : 0..1;
    EPSTALL : 0..1;
    EPTXEN : 0..1;
    EPRXEN : 0..1;
    EPCONDIS : 0..1;
  end;
var
  U1EP4bits: TU1EP4bits absolute ($BF885340);
  U1EP4CLR : longWord absolute ($BF885344);
  U1EP4SET : longWord absolute ($BF885348);
  U1EP4INV : longWord absolute ($BF88534C);
  U1EP5 : longWord absolute ($BF885350);
type
  TU1EP5bits = bitpacked record
    EPHSHK : 0..1;
    EPSTALL : 0..1;
    EPTXEN : 0..1;
    EPRXEN : 0..1;
    EPCONDIS : 0..1;
  end;
var
  U1EP5bits: TU1EP5bits absolute ($BF885350);
  U1EP5CLR : longWord absolute ($BF885354);
  U1EP5SET : longWord absolute ($BF885358);
  U1EP5INV : longWord absolute ($BF88535C);
  U1EP6 : longWord absolute ($BF885360);
type
  TU1EP6bits = bitpacked record
    EPHSHK : 0..1;
    EPSTALL : 0..1;
    EPTXEN : 0..1;
    EPRXEN : 0..1;
    EPCONDIS : 0..1;
  end;
var
  U1EP6bits: TU1EP6bits absolute ($BF885360);
  U1EP6CLR : longWord absolute ($BF885364);
  U1EP6SET : longWord absolute ($BF885368);
  U1EP6INV : longWord absolute ($BF88536C);
  U1EP7 : longWord absolute ($BF885370);
type
  TU1EP7bits = bitpacked record
    EPHSHK : 0..1;
    EPSTALL : 0..1;
    EPTXEN : 0..1;
    EPRXEN : 0..1;
    EPCONDIS : 0..1;
  end;
var
  U1EP7bits: TU1EP7bits absolute ($BF885370);
  U1EP7CLR : longWord absolute ($BF885374);
  U1EP7SET : longWord absolute ($BF885378);
  U1EP7INV : longWord absolute ($BF88537C);
  U1EP8 : longWord absolute ($BF885380);
type
  TU1EP8bits = bitpacked record
    EPHSHK : 0..1;
    EPSTALL : 0..1;
    EPTXEN : 0..1;
    EPRXEN : 0..1;
    EPCONDIS : 0..1;
  end;
var
  U1EP8bits: TU1EP8bits absolute ($BF885380);
  U1EP8CLR : longWord absolute ($BF885384);
  U1EP8SET : longWord absolute ($BF885388);
  U1EP8INV : longWord absolute ($BF88538C);
  U1EP9 : longWord absolute ($BF885390);
type
  TU1EP9bits = bitpacked record
    EPHSHK : 0..1;
    EPSTALL : 0..1;
    EPTXEN : 0..1;
    EPRXEN : 0..1;
    EPCONDIS : 0..1;
  end;
var
  U1EP9bits: TU1EP9bits absolute ($BF885390);
  U1EP9CLR : longWord absolute ($BF885394);
  U1EP9SET : longWord absolute ($BF885398);
  U1EP9INV : longWord absolute ($BF88539C);
  U1EP10 : longWord absolute ($BF8853A0);
type
  TU1EP10bits = bitpacked record
    EPHSHK : 0..1;
    EPSTALL : 0..1;
    EPTXEN : 0..1;
    EPRXEN : 0..1;
    EPCONDIS : 0..1;
  end;
var
  U1EP10bits: TU1EP10bits absolute ($BF8853A0);
  U1EP10CLR : longWord absolute ($BF8853A4);
  U1EP10SET : longWord absolute ($BF8853A8);
  U1EP10INV : longWord absolute ($BF8853AC);
  U1EP11 : longWord absolute ($BF8853B0);
type
  TU1EP11bits = bitpacked record
    EPHSHK : 0..1;
    EPSTALL : 0..1;
    EPTXEN : 0..1;
    EPRXEN : 0..1;
    EPCONDIS : 0..1;
  end;
var
  U1EP11bits: TU1EP11bits absolute ($BF8853B0);
  U1EP11CLR : longWord absolute ($BF8853B4);
  U1EP11SET : longWord absolute ($BF8853B8);
  U1EP11INV : longWord absolute ($BF8853BC);
  U1EP12 : longWord absolute ($BF8853C0);
type
  TU1EP12bits = bitpacked record
    EPHSHK : 0..1;
    EPSTALL : 0..1;
    EPTXEN : 0..1;
    EPRXEN : 0..1;
    EPCONDIS : 0..1;
  end;
var
  U1EP12bits: TU1EP12bits absolute ($BF8853C0);
  U1EP12CLR : longWord absolute ($BF8853C4);
  U1EP12SET : longWord absolute ($BF8853C8);
  U1EP12INV : longWord absolute ($BF8853CC);
  U1EP13 : longWord absolute ($BF8853D0);
type
  TU1EP13bits = bitpacked record
    EPHSHK : 0..1;
    EPSTALL : 0..1;
    EPTXEN : 0..1;
    EPRXEN : 0..1;
    EPCONDIS : 0..1;
  end;
var
  U1EP13bits: TU1EP13bits absolute ($BF8853D0);
  U1EP13CLR : longWord absolute ($BF8853D4);
  U1EP13SET : longWord absolute ($BF8853D8);
  U1EP13INV : longWord absolute ($BF8853DC);
  U1EP14 : longWord absolute ($BF8853E0);
type
  TU1EP14bits = bitpacked record
    EPHSHK : 0..1;
    EPSTALL : 0..1;
    EPTXEN : 0..1;
    EPRXEN : 0..1;
    EPCONDIS : 0..1;
  end;
var
  U1EP14bits: TU1EP14bits absolute ($BF8853E0);
  U1EP14CLR : longWord absolute ($BF8853E4);
  U1EP14SET : longWord absolute ($BF8853E8);
  U1EP14INV : longWord absolute ($BF8853EC);
  U1EP15 : longWord absolute ($BF8853F0);
type
  TU1EP15bits = bitpacked record
    EPHSHK : 0..1;
    EPSTALL : 0..1;
    EPTXEN : 0..1;
    EPRXEN : 0..1;
    EPCONDIS : 0..1;
  end;
var
  U1EP15bits: TU1EP15bits absolute ($BF8853F0);
  U1EP15CLR : longWord absolute ($BF8853F4);
  U1EP15SET : longWord absolute ($BF8853F8);
  U1EP15INV : longWord absolute ($BF8853FC);
  TRISA : longWord absolute ($BF886000);
type
  TTRISAbits = bitpacked record
  case integer of
  0 : (
    TRISA0 : 0..1;
    TRISA1 : 0..1;
    TRISA2 : 0..1;
    TRISA3 : 0..1;
    TRISA4 : 0..1;
    TRISA5 : 0..1;
    TRISA6 : 0..1;
    TRISA7 : 0..1;
    RESERVED0 : 0..1;
    TRISA9 : 0..1;
    TRISA10 : 0..1;
    RESERVED1 : 0..7;
    TRISA14 : 0..1;
    TRISA15 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  TRISAbits: TTRISAbits absolute ($BF886000);
  TRISACLR : longWord absolute ($BF886004);
  TRISASET : longWord absolute ($BF886008);
  TRISAINV : longWord absolute ($BF88600C);
  PORTA : longWord absolute ($BF886010);
type
  TPORTAbits = bitpacked record
  case integer of
  0 : (
    RA0 : 0..1;
    RA1 : 0..1;
    RA2 : 0..1;
    RA3 : 0..1;
    RA4 : 0..1;
    RA5 : 0..1;
    RA6 : 0..1;
    RA7 : 0..1;
    RESERVED0 : 0..1;
    RA9 : 0..1;
    RA10 : 0..1;
    RESERVED1 : 0..7;
    RA14 : 0..1;
    RA15 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  PORTAbits: TPORTAbits absolute ($BF886010);
  PORTACLR : longWord absolute ($BF886014);
  PORTASET : longWord absolute ($BF886018);
  PORTAINV : longWord absolute ($BF88601C);
  LATA : longWord absolute ($BF886020);
type
  TLATAbits = bitpacked record
  case integer of
  0 : (
    LATA0 : 0..1;
    LATA1 : 0..1;
    LATA2 : 0..1;
    LATA3 : 0..1;
    LATA4 : 0..1;
    LATA5 : 0..1;
    LATA6 : 0..1;
    LATA7 : 0..1;
    RESERVED0 : 0..1;
    LATA9 : 0..1;
    LATA10 : 0..1;
    RESERVED1 : 0..7;
    LATA14 : 0..1;
    LATA15 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  LATAbits: TLATAbits absolute ($BF886020);
  LATACLR : longWord absolute ($BF886024);
  LATASET : longWord absolute ($BF886028);
  LATAINV : longWord absolute ($BF88602C);
  ODCA : longWord absolute ($BF886030);
type
  TODCAbits = bitpacked record
  case integer of
  0 : (
    ODCA0 : 0..1;
    ODCA1 : 0..1;
    ODCA2 : 0..1;
    ODCA3 : 0..1;
    ODCA4 : 0..1;
    ODCA5 : 0..1;
    ODCA6 : 0..1;
    ODCA7 : 0..1;
    RESERVED0 : 0..1;
    ODCA9 : 0..1;
    ODCA10 : 0..1;
    RESERVED1 : 0..7;
    ODCA14 : 0..1;
    ODCA15 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  ODCAbits: TODCAbits absolute ($BF886030);
  ODCACLR : longWord absolute ($BF886034);
  ODCASET : longWord absolute ($BF886038);
  ODCAINV : longWord absolute ($BF88603C);
  TRISB : longWord absolute ($BF886040);
type
  TTRISBbits = bitpacked record
  case integer of
  0 : (
    TRISB0 : 0..1;
    TRISB1 : 0..1;
    TRISB2 : 0..1;
    TRISB3 : 0..1;
    TRISB4 : 0..1;
    TRISB5 : 0..1;
    TRISB6 : 0..1;
    TRISB7 : 0..1;
    TRISB8 : 0..1;
    TRISB9 : 0..1;
    TRISB10 : 0..1;
    TRISB11 : 0..1;
    TRISB12 : 0..1;
    TRISB13 : 0..1;
    TRISB14 : 0..1;
    TRISB15 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  TRISBbits: TTRISBbits absolute ($BF886040);
  TRISBCLR : longWord absolute ($BF886044);
  TRISBSET : longWord absolute ($BF886048);
  TRISBINV : longWord absolute ($BF88604C);
  PORTB : longWord absolute ($BF886050);
type
  TPORTBbits = bitpacked record
  case integer of
  0 : (
    RB0 : 0..1;
    RB1 : 0..1;
    RB2 : 0..1;
    RB3 : 0..1;
    RB4 : 0..1;
    RB5 : 0..1;
    RB6 : 0..1;
    RB7 : 0..1;
    RB8 : 0..1;
    RB9 : 0..1;
    RB10 : 0..1;
    RB11 : 0..1;
    RB12 : 0..1;
    RB13 : 0..1;
    RB14 : 0..1;
    RB15 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  PORTBbits: TPORTBbits absolute ($BF886050);
  PORTBCLR : longWord absolute ($BF886054);
  PORTBSET : longWord absolute ($BF886058);
  PORTBINV : longWord absolute ($BF88605C);
  LATB : longWord absolute ($BF886060);
type
  TLATBbits = bitpacked record
  case integer of
  0 : (
    LATB0 : 0..1;
    LATB1 : 0..1;
    LATB2 : 0..1;
    LATB3 : 0..1;
    LATB4 : 0..1;
    LATB5 : 0..1;
    LATB6 : 0..1;
    LATB7 : 0..1;
    LATB8 : 0..1;
    LATB9 : 0..1;
    LATB10 : 0..1;
    LATB11 : 0..1;
    LATB12 : 0..1;
    LATB13 : 0..1;
    LATB14 : 0..1;
    LATB15 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  LATBbits: TLATBbits absolute ($BF886060);
  LATBCLR : longWord absolute ($BF886064);
  LATBSET : longWord absolute ($BF886068);
  LATBINV : longWord absolute ($BF88606C);
  ODCB : longWord absolute ($BF886070);
type
  TODCBbits = bitpacked record
  case integer of
  0 : (
    ODCB0 : 0..1;
    ODCB1 : 0..1;
    ODCB2 : 0..1;
    ODCB3 : 0..1;
    ODCB4 : 0..1;
    ODCB5 : 0..1;
    ODCB6 : 0..1;
    ODCB7 : 0..1;
    ODCB8 : 0..1;
    ODCB9 : 0..1;
    ODCB10 : 0..1;
    ODCB11 : 0..1;
    ODCB12 : 0..1;
    ODCB13 : 0..1;
    ODCB14 : 0..1;
    ODCB15 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  ODCBbits: TODCBbits absolute ($BF886070);
  ODCBCLR : longWord absolute ($BF886074);
  ODCBSET : longWord absolute ($BF886078);
  ODCBINV : longWord absolute ($BF88607C);
  TRISC : longWord absolute ($BF886080);
type
  TTRISCbits = bitpacked record
  case integer of
  0 : (
    RESERVED0 : 0..1;
    TRISC1 : 0..1;
    TRISC2 : 0..1;
    TRISC3 : 0..1;
    TRISC4 : 0..1;
    RESERVED1 : 0..127;
    TRISC12 : 0..1;
    TRISC13 : 0..1;
    TRISC14 : 0..1;
    TRISC15 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  TRISCbits: TTRISCbits absolute ($BF886080);
  TRISCCLR : longWord absolute ($BF886084);
  TRISCSET : longWord absolute ($BF886088);
  TRISCINV : longWord absolute ($BF88608C);
  PORTC : longWord absolute ($BF886090);
type
  TPORTCbits = bitpacked record
  case integer of
  0 : (
    RESERVED0 : 0..1;
    RC1 : 0..1;
    RC2 : 0..1;
    RC3 : 0..1;
    RC4 : 0..1;
    RESERVED1 : 0..127;
    RC12 : 0..1;
    RC13 : 0..1;
    RC14 : 0..1;
    RC15 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  PORTCbits: TPORTCbits absolute ($BF886090);
  PORTCCLR : longWord absolute ($BF886094);
  PORTCSET : longWord absolute ($BF886098);
  PORTCINV : longWord absolute ($BF88609C);
  LATC : longWord absolute ($BF8860A0);
type
  TLATCbits = bitpacked record
  case integer of
  0 : (
    RESERVED0 : 0..1;
    LATC1 : 0..1;
    LATC2 : 0..1;
    LATC3 : 0..1;
    LATC4 : 0..1;
    RESERVED1 : 0..127;
    LATC12 : 0..1;
    LATC13 : 0..1;
    LATC14 : 0..1;
    LATC15 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  LATCbits: TLATCbits absolute ($BF8860A0);
  LATCCLR : longWord absolute ($BF8860A4);
  LATCSET : longWord absolute ($BF8860A8);
  LATCINV : longWord absolute ($BF8860AC);
  ODCC : longWord absolute ($BF8860B0);
type
  TODCCbits = bitpacked record
  case integer of
  0 : (
    RESERVED0 : 0..1;
    ODCC1 : 0..1;
    ODCC2 : 0..1;
    ODCC3 : 0..1;
    ODCC4 : 0..1;
    RESERVED1 : 0..127;
    ODCC12 : 0..1;
    ODCC13 : 0..1;
    ODCC14 : 0..1;
    ODCC15 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  ODCCbits: TODCCbits absolute ($BF8860B0);
  ODCCCLR : longWord absolute ($BF8860B4);
  ODCCSET : longWord absolute ($BF8860B8);
  ODCCINV : longWord absolute ($BF8860BC);
  TRISD : longWord absolute ($BF8860C0);
type
  TTRISDbits = bitpacked record
  case integer of
  0 : (
    TRISD0 : 0..1;
    TRISD1 : 0..1;
    TRISD2 : 0..1;
    TRISD3 : 0..1;
    TRISD4 : 0..1;
    TRISD5 : 0..1;
    TRISD6 : 0..1;
    TRISD7 : 0..1;
    TRISD8 : 0..1;
    TRISD9 : 0..1;
    TRISD10 : 0..1;
    TRISD11 : 0..1;
    TRISD12 : 0..1;
    TRISD13 : 0..1;
    TRISD14 : 0..1;
    TRISD15 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  TRISDbits: TTRISDbits absolute ($BF8860C0);
  TRISDCLR : longWord absolute ($BF8860C4);
  TRISDSET : longWord absolute ($BF8860C8);
  TRISDINV : longWord absolute ($BF8860CC);
  PORTD : longWord absolute ($BF8860D0);
type
  TPORTDbits = bitpacked record
  case integer of
  0 : (
    RD0 : 0..1;
    RD1 : 0..1;
    RD2 : 0..1;
    RD3 : 0..1;
    RD4 : 0..1;
    RD5 : 0..1;
    RD6 : 0..1;
    RD7 : 0..1;
    RD8 : 0..1;
    RD9 : 0..1;
    RD10 : 0..1;
    RD11 : 0..1;
    RD12 : 0..1;
    RD13 : 0..1;
    RD14 : 0..1;
    RD15 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  PORTDbits: TPORTDbits absolute ($BF8860D0);
  PORTDCLR : longWord absolute ($BF8860D4);
  PORTDSET : longWord absolute ($BF8860D8);
  PORTDINV : longWord absolute ($BF8860DC);
  LATD : longWord absolute ($BF8860E0);
type
  TLATDbits = bitpacked record
  case integer of
  0 : (
    LATD0 : 0..1;
    LATD1 : 0..1;
    LATD2 : 0..1;
    LATD3 : 0..1;
    LATD4 : 0..1;
    LATD5 : 0..1;
    LATD6 : 0..1;
    LATD7 : 0..1;
    LATD8 : 0..1;
    LATD9 : 0..1;
    LATD10 : 0..1;
    LATD11 : 0..1;
    LATD12 : 0..1;
    LATD13 : 0..1;
    LATD14 : 0..1;
    LATD15 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  LATDbits: TLATDbits absolute ($BF8860E0);
  LATDCLR : longWord absolute ($BF8860E4);
  LATDSET : longWord absolute ($BF8860E8);
  LATDINV : longWord absolute ($BF8860EC);
  ODCD : longWord absolute ($BF8860F0);
type
  TODCDbits = bitpacked record
  case integer of
  0 : (
    ODCD0 : 0..1;
    ODCD1 : 0..1;
    ODCD2 : 0..1;
    ODCD3 : 0..1;
    ODCD4 : 0..1;
    ODCD5 : 0..1;
    ODCD6 : 0..1;
    ODCD7 : 0..1;
    ODCD8 : 0..1;
    ODCD9 : 0..1;
    ODCD10 : 0..1;
    ODCD11 : 0..1;
    ODCD12 : 0..1;
    ODCD13 : 0..1;
    ODCD14 : 0..1;
    ODCD15 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  ODCDbits: TODCDbits absolute ($BF8860F0);
  ODCDCLR : longWord absolute ($BF8860F4);
  ODCDSET : longWord absolute ($BF8860F8);
  ODCDINV : longWord absolute ($BF8860FC);
  TRISE : longWord absolute ($BF886100);
type
  TTRISEbits = bitpacked record
  case integer of
  0 : (
    TRISE0 : 0..1;
    TRISE1 : 0..1;
    TRISE2 : 0..1;
    TRISE3 : 0..1;
    TRISE4 : 0..1;
    TRISE5 : 0..1;
    TRISE6 : 0..1;
    TRISE7 : 0..1;
    TRISE8 : 0..1;
    TRISE9 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  TRISEbits: TTRISEbits absolute ($BF886100);
  TRISECLR : longWord absolute ($BF886104);
  TRISESET : longWord absolute ($BF886108);
  TRISEINV : longWord absolute ($BF88610C);
  PORTE : longWord absolute ($BF886110);
type
  TPORTEbits = bitpacked record
  case integer of
  0 : (
    RE0 : 0..1;
    RE1 : 0..1;
    RE2 : 0..1;
    RE3 : 0..1;
    RE4 : 0..1;
    RE5 : 0..1;
    RE6 : 0..1;
    RE7 : 0..1;
    RE8 : 0..1;
    RE9 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  PORTEbits: TPORTEbits absolute ($BF886110);
  PORTECLR : longWord absolute ($BF886114);
  PORTESET : longWord absolute ($BF886118);
  PORTEINV : longWord absolute ($BF88611C);
  LATE : longWord absolute ($BF886120);
type
  TLATEbits = bitpacked record
  case integer of
  0 : (
    LATE0 : 0..1;
    LATE1 : 0..1;
    LATE2 : 0..1;
    LATE3 : 0..1;
    LATE4 : 0..1;
    LATE5 : 0..1;
    LATE6 : 0..1;
    LATE7 : 0..1;
    LATE8 : 0..1;
    LATE9 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  LATEbits: TLATEbits absolute ($BF886120);
  LATECLR : longWord absolute ($BF886124);
  LATESET : longWord absolute ($BF886128);
  LATEINV : longWord absolute ($BF88612C);
  ODCE : longWord absolute ($BF886130);
type
  TODCEbits = bitpacked record
  case integer of
  0 : (
    ODCE0 : 0..1;
    ODCE1 : 0..1;
    ODCE2 : 0..1;
    ODCE3 : 0..1;
    ODCE4 : 0..1;
    ODCE5 : 0..1;
    ODCE6 : 0..1;
    ODCE7 : 0..1;
    ODCE8 : 0..1;
    ODCE9 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  ODCEbits: TODCEbits absolute ($BF886130);
  ODCECLR : longWord absolute ($BF886134);
  ODCESET : longWord absolute ($BF886138);
  ODCEINV : longWord absolute ($BF88613C);
  TRISF : longWord absolute ($BF886140);
type
  TTRISFbits = bitpacked record
  case integer of
  0 : (
    TRISF0 : 0..1;
    TRISF1 : 0..1;
    TRISF2 : 0..1;
    TRISF3 : 0..1;
    TRISF4 : 0..1;
    TRISF5 : 0..1;
    RESERVED0 : 0..3;
    TRISF8 : 0..1;
    RESERVED1 : 0..7;
    TRISF12 : 0..1;
    TRISF13 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  TRISFbits: TTRISFbits absolute ($BF886140);
  TRISFCLR : longWord absolute ($BF886144);
  TRISFSET : longWord absolute ($BF886148);
  TRISFINV : longWord absolute ($BF88614C);
  PORTF : longWord absolute ($BF886150);
type
  TPORTFbits = bitpacked record
  case integer of
  0 : (
    RF0 : 0..1;
    RF1 : 0..1;
    RF2 : 0..1;
    RF3 : 0..1;
    RF4 : 0..1;
    RF5 : 0..1;
    RESERVED0 : 0..3;
    RF8 : 0..1;
    RESERVED1 : 0..7;
    RF12 : 0..1;
    RF13 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  PORTFbits: TPORTFbits absolute ($BF886150);
  PORTFCLR : longWord absolute ($BF886154);
  PORTFSET : longWord absolute ($BF886158);
  PORTFINV : longWord absolute ($BF88615C);
  LATF : longWord absolute ($BF886160);
type
  TLATFbits = bitpacked record
  case integer of
  0 : (
    LATF0 : 0..1;
    LATF1 : 0..1;
    LATF2 : 0..1;
    LATF3 : 0..1;
    LATF4 : 0..1;
    LATF5 : 0..1;
    RESERVED0 : 0..3;
    LATF8 : 0..1;
    RESERVED1 : 0..7;
    LATF12 : 0..1;
    LATF13 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  LATFbits: TLATFbits absolute ($BF886160);
  LATFCLR : longWord absolute ($BF886164);
  LATFSET : longWord absolute ($BF886168);
  LATFINV : longWord absolute ($BF88616C);
  ODCF : longWord absolute ($BF886170);
type
  TODCFbits = bitpacked record
  case integer of
  0 : (
    ODCF0 : 0..1;
    ODCF1 : 0..1;
    ODCF2 : 0..1;
    ODCF3 : 0..1;
    ODCF4 : 0..1;
    ODCF5 : 0..1;
    RESERVED0 : 0..3;
    ODCF8 : 0..1;
    RESERVED1 : 0..7;
    ODCF12 : 0..1;
    ODCF13 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  ODCFbits: TODCFbits absolute ($BF886170);
  ODCFCLR : longWord absolute ($BF886174);
  ODCFSET : longWord absolute ($BF886178);
  ODCFINV : longWord absolute ($BF88617C);
  TRISG : longWord absolute ($BF886180);
type
  TTRISGbits = bitpacked record
  case integer of
  0 : (
    TRISG0 : 0..1;
    TRISG1 : 0..1;
    TRISG2 : 0..1;
    TRISG3 : 0..1;
    RESERVED0 : 0..3;
    TRISG6 : 0..1;
    TRISG7 : 0..1;
    TRISG8 : 0..1;
    TRISG9 : 0..1;
    RESERVED1 : 0..3;
    TRISG12 : 0..1;
    TRISG13 : 0..1;
    TRISG14 : 0..1;
    TRISG15 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  TRISGbits: TTRISGbits absolute ($BF886180);
  TRISGCLR : longWord absolute ($BF886184);
  TRISGSET : longWord absolute ($BF886188);
  TRISGINV : longWord absolute ($BF88618C);
  PORTG : longWord absolute ($BF886190);
type
  TPORTGbits = bitpacked record
  case integer of
  0 : (
    RG0 : 0..1;
    RG1 : 0..1;
    RG2 : 0..1;
    RG3 : 0..1;
    RESERVED0 : 0..3;
    RG6 : 0..1;
    RG7 : 0..1;
    RG8 : 0..1;
    RG9 : 0..1;
    RESERVED1 : 0..3;
    RG12 : 0..1;
    RG13 : 0..1;
    RG14 : 0..1;
    RG15 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  PORTGbits: TPORTGbits absolute ($BF886190);
  PORTGCLR : longWord absolute ($BF886194);
  PORTGSET : longWord absolute ($BF886198);
  PORTGINV : longWord absolute ($BF88619C);
  LATG : longWord absolute ($BF8861A0);
type
  TLATGbits = bitpacked record
  case integer of
  0 : (
    LATG0 : 0..1;
    LATG1 : 0..1;
    LATG2 : 0..1;
    LATG3 : 0..1;
    RESERVED0 : 0..3;
    LATG6 : 0..1;
    LATG7 : 0..1;
    LATG8 : 0..1;
    LATG9 : 0..1;
    RESERVED1 : 0..3;
    LATG12 : 0..1;
    LATG13 : 0..1;
    LATG14 : 0..1;
    LATG15 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  LATGbits: TLATGbits absolute ($BF8861A0);
  LATGCLR : longWord absolute ($BF8861A4);
  LATGSET : longWord absolute ($BF8861A8);
  LATGINV : longWord absolute ($BF8861AC);
  ODCG : longWord absolute ($BF8861B0);
type
  TODCGbits = bitpacked record
  case integer of
  0 : (
    ODCG0 : 0..1;
    ODCG1 : 0..1;
    ODCG2 : 0..1;
    ODCG3 : 0..1;
    RESERVED0 : 0..3;
    ODCG6 : 0..1;
    ODCG7 : 0..1;
    ODCG8 : 0..1;
    ODCG9 : 0..1;
    RESERVED1 : 0..3;
    ODCG12 : 0..1;
    ODCG13 : 0..1;
    ODCG14 : 0..1;
    ODCG15 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  ODCGbits: TODCGbits absolute ($BF8861B0);
  ODCGCLR : longWord absolute ($BF8861B4);
  ODCGSET : longWord absolute ($BF8861B8);
  ODCGINV : longWord absolute ($BF8861BC);
  CNCON : longWord absolute ($BF8861C0);
type
  TCNCONbits = bitpacked record
  case integer of
  0 : (
    RESERVED0 : 0..8191;
    SIDL : 0..1;
    RESERVED1 : 0..1;
    ON : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  CNCONbits: TCNCONbits absolute ($BF8861C0);
  CNCONCLR : longWord absolute ($BF8861C4);
  CNCONSET : longWord absolute ($BF8861C8);
  CNCONINV : longWord absolute ($BF8861CC);
  CNEN : longWord absolute ($BF8861D0);
type
  TCNENbits = bitpacked record
  case integer of
  0 : (
    CNEN0 : 0..1;
    CNEN1 : 0..1;
    CNEN2 : 0..1;
    CNEN3 : 0..1;
    CNEN4 : 0..1;
    CNEN5 : 0..1;
    CNEN6 : 0..1;
    CNEN7 : 0..1;
    CNEN8 : 0..1;
    CNEN9 : 0..1;
    CNEN10 : 0..1;
    CNEN11 : 0..1;
    CNEN12 : 0..1;
    CNEN13 : 0..1;
    CNEN14 : 0..1;
    CNEN15 : 0..1;
    CNEN16 : 0..1;
    CNEN17 : 0..1;
    CNEN18 : 0..1;
    CNEN19 : 0..1;
    CNEN20 : 0..1;
    CNEN21 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  CNENbits: TCNENbits absolute ($BF8861D0);
  CNENCLR : longWord absolute ($BF8861D4);
  CNENSET : longWord absolute ($BF8861D8);
  CNENINV : longWord absolute ($BF8861DC);
  CNPUE : longWord absolute ($BF8861E0);
type
  TCNPUEbits = bitpacked record
  case integer of
  0 : (
    CNPUE0 : 0..1;
    CNPUE1 : 0..1;
    CNPUE2 : 0..1;
    CNPUE3 : 0..1;
    CNPUE4 : 0..1;
    CNPUE5 : 0..1;
    CNPUE6 : 0..1;
    CNPUE7 : 0..1;
    CNPUE8 : 0..1;
    CNPUE9 : 0..1;
    CNPUE10 : 0..1;
    CNPUE11 : 0..1;
    CNPUE12 : 0..1;
    CNPUE13 : 0..1;
    CNPUE14 : 0..1;
    CNPUE15 : 0..1;
    CNPUE16 : 0..1;
    CNPUE17 : 0..1;
    CNPUE18 : 0..1;
    CNPUE19 : 0..1;
    CNPUE20 : 0..1;
    CNPUE21 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  CNPUEbits: TCNPUEbits absolute ($BF8861E0);
  CNPUECLR : longWord absolute ($BF8861E4);
  CNPUESET : longWord absolute ($BF8861E8);
  CNPUEINV : longWord absolute ($BF8861EC);
  ETHCON1 : longWord absolute ($BF889000);
type
  TETHCON1bits = bitpacked record
  case integer of
  0 : (
    BUFCDEC : 0..1;
    RESERVED0 : 0..7;
    MANFC : 0..1;
    RESERVED1 : 0..3;
    AUTOFC : 0..1;
    RXEN : 0..1;
    TXRTS : 0..1;
    RESERVED2 : 0..7;
    SIDL : 0..1;
    RESERVED3 : 0..1;
    ON : 0..1;
    PTV : 0..65535;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  ETHCON1bits: TETHCON1bits absolute ($BF889000);
  ETHCON1CLR : longWord absolute ($BF889004);
  ETHCON1SET : longWord absolute ($BF889008);
  ETHCON1INV : longWord absolute ($BF88900C);
  ETHCON2 : longWord absolute ($BF889010);
type
  TETHCON2bits = bitpacked record
  case integer of
  0 : (
    RESERVED0 : 0..15;
    RXBUF_SZ : 0..127;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  ETHCON2bits: TETHCON2bits absolute ($BF889010);
  ETHCON2CLR : longWord absolute ($BF889014);
  ETHCON2SET : longWord absolute ($BF889018);
  ETHCON2INV : longWord absolute ($BF88901C);
  ETHTXST : longWord absolute ($BF889020);
type
  TETHTXSTbits = bitpacked record
  case integer of
  0 : (
    RESERVED0 : 0..3;
    TXSTADDR : 0..1073741823;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  ETHTXSTbits: TETHTXSTbits absolute ($BF889020);
  ETHTXSTCLR : longWord absolute ($BF889024);
  ETHTXSTSET : longWord absolute ($BF889028);
  ETHTXSTINV : longWord absolute ($BF88902C);
  ETHRXST : longWord absolute ($BF889030);
type
  TETHRXSTbits = bitpacked record
  case integer of
  0 : (
    RESERVED0 : 0..3;
    RXSTADDR : 0..1073741823;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  ETHRXSTbits: TETHRXSTbits absolute ($BF889030);
  ETHRXSTCLR : longWord absolute ($BF889034);
  ETHRXSTSET : longWord absolute ($BF889038);
  ETHRXSTINV : longWord absolute ($BF88903C);
  ETHHT0 : longWord absolute ($BF889040);
type
  TETHHT0bits = bitpacked record
  case integer of
  0 : (
    w : 0..4294967295;
  );
  1 : (
    HTLOWER : 0..4294967295;
  );
  end;
var
  ETHHT0bits: TETHHT0bits absolute ($BF889040);
  ETHHT0CLR : longWord absolute ($BF889044);
  ETHHT0SET : longWord absolute ($BF889048);
  ETHHT0INV : longWord absolute ($BF88904C);
  ETHHT1 : longWord absolute ($BF889050);
type
  TETHHT1bits = bitpacked record
  case integer of
  0 : (
    w : 0..4294967295;
  );
  1 : (
    HTUPPER : 0..4294967295;
  );
  end;
var
  ETHHT1bits: TETHHT1bits absolute ($BF889050);
  ETHHT1CLR : longWord absolute ($BF889054);
  ETHHT1SET : longWord absolute ($BF889058);
  ETHHT1INV : longWord absolute ($BF88905C);
  ETHPMM0 : longWord absolute ($BF889060);
type
  TETHPMM0bits = bitpacked record
  case integer of
  0 : (
    w : 0..4294967295;
  );
  1 : (
    PMMLOWER : 0..4294967295;
  );
  end;
var
  ETHPMM0bits: TETHPMM0bits absolute ($BF889060);
  ETHPMM0CLR : longWord absolute ($BF889064);
  ETHPMM0SET : longWord absolute ($BF889068);
  ETHPMM0INV : longWord absolute ($BF88906C);
  ETHPMM1 : longWord absolute ($BF889070);
type
  TETHPMM1bits = bitpacked record
  case integer of
  0 : (
    w : 0..4294967295;
  );
  1 : (
    PMMUPPER : 0..4294967295;
  );
  end;
var
  ETHPMM1bits: TETHPMM1bits absolute ($BF889070);
  ETHPMM1CLR : longWord absolute ($BF889074);
  ETHPMM1SET : longWord absolute ($BF889078);
  ETHPMM1INV : longWord absolute ($BF88907C);
  ETHPMCS : longWord absolute ($BF889080);
type
  TETHPMCSbits = bitpacked record
  case integer of
  0 : (
    PMCS : 0..65535;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  ETHPMCSbits: TETHPMCSbits absolute ($BF889080);
  ETHPMCSCLR : longWord absolute ($BF889084);
  ETHPMCSSET : longWord absolute ($BF889088);
  ETHPMCSINV : longWord absolute ($BF88908C);
  ETHPMO : longWord absolute ($BF889090);
type
  TETHPMObits = bitpacked record
  case integer of
  0 : (
    PMO : 0..65535;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  ETHPMObits: TETHPMObits absolute ($BF889090);
  ETHPMOCLR : longWord absolute ($BF889094);
  ETHPMOSET : longWord absolute ($BF889098);
  ETHPMOINV : longWord absolute ($BF88909C);
  ETHRXFC : longWord absolute ($BF8890A0);
type
  TETHRXFCbits = bitpacked record
  case integer of
  0 : (
    BCEN : 0..1;
    MCEN : 0..1;
    NOTMEEN : 0..1;
    UCEN : 0..1;
    RUNTEN : 0..1;
    RUNTERREN : 0..1;
    CRCOKEN : 0..1;
    CRCERREN : 0..1;
    PMMODE : 0..15;
    NOTPM : 0..1;
    RESERVED0 : 0..1;
    MPEN : 0..1;
    HTEN : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  ETHRXFCbits: TETHRXFCbits absolute ($BF8890A0);
  ETHRXFCCLR : longWord absolute ($BF8890A4);
  ETHRXFCSET : longWord absolute ($BF8890A8);
  ETHRXFCINV : longWord absolute ($BF8890AC);
  ETHRXWM : longWord absolute ($BF8890B0);
type
  TETHRXWMbits = bitpacked record
  case integer of
  0 : (
    RXEWM : 0..255;
    RESERVED0 : 0..255;
    RXFWM : 0..255;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  ETHRXWMbits: TETHRXWMbits absolute ($BF8890B0);
  ETHRXWMCLR : longWord absolute ($BF8890B4);
  ETHRXWMSET : longWord absolute ($BF8890B8);
  ETHRXWMINV : longWord absolute ($BF8890BC);
  ETHIEN : longWord absolute ($BF8890C0);
type
  TETHIENbits = bitpacked record
  case integer of
  0 : (
    RXOVFLWIE : 0..1;
    RXBUFNAIE : 0..1;
    TXABORTIE : 0..1;
    TXDONEIE : 0..1;
    RESERVED0 : 0..1;
    RXACTIE : 0..1;
    PKTPENDIE : 0..1;
    RXDONEIE : 0..1;
    FWMARKIE : 0..1;
    EWMARKIE : 0..1;
    RESERVED1 : 0..7;
    RXBUSEIE : 0..1;
    TXBUSEIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  ETHIENbits: TETHIENbits absolute ($BF8890C0);
  ETHIENCLR : longWord absolute ($BF8890C4);
  ETHIENSET : longWord absolute ($BF8890C8);
  ETHIENINV : longWord absolute ($BF8890CC);
  ETHIRQ : longWord absolute ($BF8890D0);
type
  TETHIRQbits = bitpacked record
  case integer of
  0 : (
    RXOVFLW : 0..1;
    RXBUFNA : 0..1;
    TXABORT : 0..1;
    TXDONE : 0..1;
    RESERVED0 : 0..1;
    RXACT : 0..1;
    PKTPEND : 0..1;
    RXDONE : 0..1;
    FWMARK : 0..1;
    EWMARK : 0..1;
    RESERVED1 : 0..7;
    RXBUSE : 0..1;
    TXBUSE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  ETHIRQbits: TETHIRQbits absolute ($BF8890D0);
  ETHIRQCLR : longWord absolute ($BF8890D4);
  ETHIRQSET : longWord absolute ($BF8890D8);
  ETHIRQINV : longWord absolute ($BF8890DC);
  ETHSTAT : longWord absolute ($BF8890E0);
type
  TETHSTATbits = bitpacked record
  case integer of
  0 : (
    RESERVED0 : 0..31;
    RXBUSY : 0..1;
    TXBUSY : 0..1;
    BUSY : 0..1;
    RESERVED1 : 0..255;
    BUFCNT : 0..255;
  );
  1 : (
    RESERVED2 : 0..127;
    ETHBUSY : 0..1;
  );
  2 : (
    w : 0..4294967295;
  );
  end;
var
  ETHSTATbits: TETHSTATbits absolute ($BF8890E0);
  ETHSTATCLR : longWord absolute ($BF8890E4);
  ETHSTATSET : longWord absolute ($BF8890E8);
  ETHSTATINV : longWord absolute ($BF8890EC);
  ETHRXOVFLOW : longWord absolute ($BF889100);
type
  TETHRXOVFLOWbits = bitpacked record
  case integer of
  0 : (
    RXOVFLWCNT : 0..65535;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  ETHRXOVFLOWbits: TETHRXOVFLOWbits absolute ($BF889100);
  ETHRXOVFLOWCLR : longWord absolute ($BF889104);
  ETHRXOVFLOWSET : longWord absolute ($BF889108);
  ETHRXOVFLOWINV : longWord absolute ($BF88910C);
  ETHFRMTXOK : longWord absolute ($BF889110);
type
  TETHFRMTXOKbits = bitpacked record
  case integer of
  0 : (
    FRMTXOKCNT : 0..65535;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  ETHFRMTXOKbits: TETHFRMTXOKbits absolute ($BF889110);
  ETHFRMTXOKCLR : longWord absolute ($BF889114);
  ETHFRMTXOKSET : longWord absolute ($BF889118);
  ETHFRMTXOKINV : longWord absolute ($BF88911C);
  ETHSCOLFRM : longWord absolute ($BF889120);
type
  TETHSCOLFRMbits = bitpacked record
  case integer of
  0 : (
    SCOLFRMCNT : 0..65535;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  ETHSCOLFRMbits: TETHSCOLFRMbits absolute ($BF889120);
  ETHSCOLFRMCLR : longWord absolute ($BF889124);
  ETHSCOLFRMSET : longWord absolute ($BF889128);
  ETHSCOLFRMINV : longWord absolute ($BF88912C);
  ETHMCOLFRM : longWord absolute ($BF889130);
type
  TETHMCOLFRMbits = bitpacked record
  case integer of
  0 : (
    MCOLFRMCNT : 0..65535;
  );
  1 : (
    MCOLFRM_CNT : 0..65535;
  );
  2 : (
    w : 0..4294967295;
  );
  end;
var
  ETHMCOLFRMbits: TETHMCOLFRMbits absolute ($BF889130);
  ETHMCOLFRMCLR : longWord absolute ($BF889134);
  ETHMCOLFRMSET : longWord absolute ($BF889138);
  ETHMCOLFRMINV : longWord absolute ($BF88913C);
  ETHFRMRXOK : longWord absolute ($BF889140);
type
  TETHFRMRXOKbits = bitpacked record
  case integer of
  0 : (
    FRMRXOKCNT : 0..65535;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  ETHFRMRXOKbits: TETHFRMRXOKbits absolute ($BF889140);
  ETHFRMRXOKCLR : longWord absolute ($BF889144);
  ETHFRMRXOKSET : longWord absolute ($BF889148);
  ETHFRMRXOKINV : longWord absolute ($BF88914C);
  ETHFCSERR : longWord absolute ($BF889150);
type
  TETHFCSERRbits = bitpacked record
  case integer of
  0 : (
    FCSERRCNT : 0..65535;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  ETHFCSERRbits: TETHFCSERRbits absolute ($BF889150);
  ETHFCSERRCLR : longWord absolute ($BF889154);
  ETHFCSERRSET : longWord absolute ($BF889158);
  ETHFCSERRINV : longWord absolute ($BF88915C);
  ETHALGNERR : longWord absolute ($BF889160);
type
  TETHALGNERRbits = bitpacked record
  case integer of
  0 : (
    ALGNERRCNT : 0..65535;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  ETHALGNERRbits: TETHALGNERRbits absolute ($BF889160);
  ETHALGNERRCLR : longWord absolute ($BF889164);
  ETHALGNERRSET : longWord absolute ($BF889168);
  ETHALGNERRINV : longWord absolute ($BF88916C);
  EMAC1CFG1 : longWord absolute ($BF889200);
type
  TEMAC1CFG1bits = bitpacked record
  case integer of
  0 : (
    RXENABLE : 0..1;
    PASSALL : 0..1;
    RXPAUSE : 0..1;
    TXPAUSE : 0..1;
    LOOPBACK : 0..1;
    RESERVED0 : 0..7;
    RESETTFUN : 0..1;
    RESETTMCS : 0..1;
    RESETRFUN : 0..1;
    RESETRMCS : 0..1;
    RESERVED1 : 0..3;
    SIMRESET : 0..1;
    SOFTRESET : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  EMAC1CFG1bits: TEMAC1CFG1bits absolute ($BF889200);
  EMACxCFG1 : longWord absolute ($BF889200);
type
  TEMACxCFG1bits = bitpacked record
  case integer of
  0 : (
    RXENABLE : 0..1;
    PASSALL : 0..1;
    RXPAUSE : 0..1;
    TXPAUSE : 0..1;
    LOOPBACK : 0..1;
    RESERVED0 : 0..7;
    RESETTFUN : 0..1;
    RESETTMCS : 0..1;
    RESETRFUN : 0..1;
    RESETRMCS : 0..1;
    RESERVED1 : 0..3;
    SIMRESET : 0..1;
    SOFTRESET : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  EMACxCFG1bits: TEMACxCFG1bits absolute ($BF889200);
  EMAC1CFG1CLR : longWord absolute ($BF889204);
  EMACxCFG1CLR : longWord absolute ($BF889204);
  EMAC1CFG1SET : longWord absolute ($BF889208);
  EMACxCFG1SET : longWord absolute ($BF889208);
  EMAC1CFG1INV : longWord absolute ($BF88920C);
  EMACxCFG1INV : longWord absolute ($BF88920C);
  EMAC1CFG2 : longWord absolute ($BF889210);
type
  TEMAC1CFG2bits = bitpacked record
  case integer of
  0 : (
    FULLDPLX : 0..1;
    LENGTHCK : 0..1;
    HUGEFRM : 0..1;
    DELAYCRC : 0..1;
    CRCENABLE : 0..1;
    PADENABLE : 0..1;
    VLANPAD : 0..1;
    AUTOPAD : 0..1;
    PUREPRE : 0..1;
    LONGPRE : 0..1;
    RESERVED0 : 0..3;
    NOBKOFF : 0..1;
    BPNOBKOFF : 0..1;
    EXCESSDFR : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  EMAC1CFG2bits: TEMAC1CFG2bits absolute ($BF889210);
  EMACxCFG2 : longWord absolute ($BF889210);
type
  TEMACxCFG2bits = bitpacked record
  case integer of
  0 : (
    FULLDPLX : 0..1;
    LENGTHCK : 0..1;
    HUGEFRM : 0..1;
    DELAYCRC : 0..1;
    CRCENABLE : 0..1;
    PADENABLE : 0..1;
    VLANPAD : 0..1;
    AUTOPAD : 0..1;
    PUREPRE : 0..1;
    LONGPRE : 0..1;
    RESERVED0 : 0..3;
    NOBKOFF : 0..1;
    BPNOBKOFF : 0..1;
    EXCESSDFR : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  EMACxCFG2bits: TEMACxCFG2bits absolute ($BF889210);
  EMAC1CFG2CLR : longWord absolute ($BF889214);
  EMACxCFG2CLR : longWord absolute ($BF889214);
  EMAC1CFG2SET : longWord absolute ($BF889218);
  EMACxCFG2SET : longWord absolute ($BF889218);
  EMAC1CFG2INV : longWord absolute ($BF88921C);
  EMACxCFG2INV : longWord absolute ($BF88921C);
  EMAC1IPGT : longWord absolute ($BF889220);
type
  TEMAC1IPGTbits = bitpacked record
  case integer of
  0 : (
    B2BIPKTGP : 0..127;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  EMAC1IPGTbits: TEMAC1IPGTbits absolute ($BF889220);
  EMACxIPGT : longWord absolute ($BF889220);
type
  TEMACxIPGTbits = bitpacked record
  case integer of
  0 : (
    B2BIPKTGP : 0..127;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  EMACxIPGTbits: TEMACxIPGTbits absolute ($BF889220);
  EMAC1IPGTCLR : longWord absolute ($BF889224);
  EMACxIPGTCLR : longWord absolute ($BF889224);
  EMAC1IPGTSET : longWord absolute ($BF889228);
  EMACxIPGTSET : longWord absolute ($BF889228);
  EMAC1IPGTINV : longWord absolute ($BF88922C);
  EMACxIPGTINV : longWord absolute ($BF88922C);
  EMAC1IPGR : longWord absolute ($BF889230);
type
  TEMAC1IPGRbits = bitpacked record
  case integer of
  0 : (
    NB2BIPKTGP2 : 0..127;
    RESERVED0 : 0..1;
    NB2BIPKTGP1 : 0..127;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  EMAC1IPGRbits: TEMAC1IPGRbits absolute ($BF889230);
  EMACxIPGR : longWord absolute ($BF889230);
type
  TEMACxIPGRbits = bitpacked record
  case integer of
  0 : (
    NB2BIPKTGP2 : 0..127;
    RESERVED0 : 0..1;
    NB2BIPKTGP1 : 0..127;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  EMACxIPGRbits: TEMACxIPGRbits absolute ($BF889230);
  EMAC1IPGRCLR : longWord absolute ($BF889234);
  EMACxIPGRCLR : longWord absolute ($BF889234);
  EMAC1IPGRSET : longWord absolute ($BF889238);
  EMACxIPGRSET : longWord absolute ($BF889238);
  EMAC1IPGRINV : longWord absolute ($BF88923C);
  EMACxIPGRINV : longWord absolute ($BF88923C);
  EMAC1CLRT : longWord absolute ($BF889240);
type
  TEMAC1CLRTbits = bitpacked record
  case integer of
  0 : (
    RETX : 0..15;
    RESERVED0 : 0..15;
    CWINDOW : 0..63;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  EMAC1CLRTbits: TEMAC1CLRTbits absolute ($BF889240);
  EMACxCLRT : longWord absolute ($BF889240);
type
  TEMACxCLRTbits = bitpacked record
  case integer of
  0 : (
    RETX : 0..15;
    RESERVED0 : 0..15;
    CWINDOW : 0..63;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  EMACxCLRTbits: TEMACxCLRTbits absolute ($BF889240);
  EMAC1CLRTCLR : longWord absolute ($BF889244);
  EMACxCLRTCLR : longWord absolute ($BF889244);
  EMAC1CLRTSET : longWord absolute ($BF889248);
  EMACxCLRTSET : longWord absolute ($BF889248);
  EMAC1CLRTINV : longWord absolute ($BF88924C);
  EMACxCLRTINV : longWord absolute ($BF88924C);
  EMAC1MAXF : longWord absolute ($BF889250);
type
  TEMAC1MAXFbits = bitpacked record
  case integer of
  0 : (
    MACMAXF : 0..65535;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  EMAC1MAXFbits: TEMAC1MAXFbits absolute ($BF889250);
  EMACxMAXF : longWord absolute ($BF889250);
type
  TEMACxMAXFbits = bitpacked record
  case integer of
  0 : (
    MACMAXF : 0..65535;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  EMACxMAXFbits: TEMACxMAXFbits absolute ($BF889250);
  EMAC1MAXFCLR : longWord absolute ($BF889254);
  EMACxMAXFCLR : longWord absolute ($BF889254);
  EMAC1MAXFSET : longWord absolute ($BF889258);
  EMACxMAXFSET : longWord absolute ($BF889258);
  EMAC1MAXFINV : longWord absolute ($BF88925C);
  EMACxMAXFINV : longWord absolute ($BF88925C);
  EMAC1SUPP : longWord absolute ($BF889260);
type
  TEMAC1SUPPbits = bitpacked record
  case integer of
  0 : (
    RESERVED0 : 0..255;
    SPEEDRMII : 0..1;
    RESERVED1 : 0..3;
    RESETRMII : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  EMAC1SUPPbits: TEMAC1SUPPbits absolute ($BF889260);
  EMACxSUPP : longWord absolute ($BF889260);
type
  TEMACxSUPPbits = bitpacked record
  case integer of
  0 : (
    RESERVED0 : 0..255;
    SPEEDRMII : 0..1;
    RESERVED1 : 0..3;
    RESETRMII : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  EMACxSUPPbits: TEMACxSUPPbits absolute ($BF889260);
  EMAC1SUPPCLR : longWord absolute ($BF889264);
  EMACxSUPPCLR : longWord absolute ($BF889264);
  EMAC1SUPPSET : longWord absolute ($BF889268);
  EMACxSUPPSET : longWord absolute ($BF889268);
  EMAC1SUPPINV : longWord absolute ($BF88926C);
  EMACxSUPPINV : longWord absolute ($BF88926C);
  EMAC1TEST : longWord absolute ($BF889270);
type
  TEMAC1TESTbits = bitpacked record
  case integer of
  0 : (
    SHRTQNTA : 0..1;
    TESTPAUSE : 0..1;
    TESTBP : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  EMAC1TESTbits: TEMAC1TESTbits absolute ($BF889270);
  EMACxTEST : longWord absolute ($BF889270);
type
  TEMACxTESTbits = bitpacked record
  case integer of
  0 : (
    SHRTQNTA : 0..1;
    TESTPAUSE : 0..1;
    TESTBP : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  EMACxTESTbits: TEMACxTESTbits absolute ($BF889270);
  EMAC1TESTCLR : longWord absolute ($BF889274);
  EMACxTESTCLR : longWord absolute ($BF889274);
  EMAC1TESTSET : longWord absolute ($BF889278);
  EMACxTESTSET : longWord absolute ($BF889278);
  EMAC1TESTINV : longWord absolute ($BF88927C);
  EMACxTESTINV : longWord absolute ($BF88927C);
  EMAC1MCFG : longWord absolute ($BF889280);
type
  TEMAC1MCFGbits = bitpacked record
  case integer of
  0 : (
    SCANINC : 0..1;
    NOPRE : 0..1;
    CLKSEL : 0..15;
    RESERVED0 : 0..511;
    RESETMGMT : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  EMAC1MCFGbits: TEMAC1MCFGbits absolute ($BF889280);
  EMACxMCFG : longWord absolute ($BF889280);
type
  TEMACxMCFGbits = bitpacked record
  case integer of
  0 : (
    SCANINC : 0..1;
    NOPRE : 0..1;
    CLKSEL : 0..15;
    RESERVED0 : 0..511;
    RESETMGMT : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  EMACxMCFGbits: TEMACxMCFGbits absolute ($BF889280);
  EMAC1MCFGCLR : longWord absolute ($BF889284);
  EMACxMCFGCLR : longWord absolute ($BF889284);
  EMAC1MCFGSET : longWord absolute ($BF889288);
  EMACxMCFGSET : longWord absolute ($BF889288);
  EMAC1MCFGINV : longWord absolute ($BF88928C);
  EMACxMCFGINV : longWord absolute ($BF88928C);
  EMAC1MCMD : longWord absolute ($BF889290);
type
  TEMAC1MCMDbits = bitpacked record
  case integer of
  0 : (
    READ : 0..1;
    SCAN : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  EMAC1MCMDbits: TEMAC1MCMDbits absolute ($BF889290);
  EMACxMCMD : longWord absolute ($BF889290);
type
  TEMACxMCMDbits = bitpacked record
  case integer of
  0 : (
    READ : 0..1;
    SCAN : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  EMACxMCMDbits: TEMACxMCMDbits absolute ($BF889290);
  EMAC1MCMDCLR : longWord absolute ($BF889294);
  EMACxMCMDCLR : longWord absolute ($BF889294);
  EMAC1MCMDSET : longWord absolute ($BF889298);
  EMACxMCMDSET : longWord absolute ($BF889298);
  EMAC1MCMDINV : longWord absolute ($BF88929C);
  EMACxMCMDINV : longWord absolute ($BF88929C);
  EMAC1MADR : longWord absolute ($BF8892A0);
type
  TEMAC1MADRbits = bitpacked record
  case integer of
  0 : (
    REGADDR : 0..31;
    RESERVED0 : 0..7;
    PHYADDR : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  EMAC1MADRbits: TEMAC1MADRbits absolute ($BF8892A0);
  EMACxMADR : longWord absolute ($BF8892A0);
type
  TEMACxMADRbits = bitpacked record
  case integer of
  0 : (
    REGADDR : 0..31;
    RESERVED0 : 0..7;
    PHYADDR : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  EMACxMADRbits: TEMACxMADRbits absolute ($BF8892A0);
  EMAC1MADRCLR : longWord absolute ($BF8892A4);
  EMACxMADRCLR : longWord absolute ($BF8892A4);
  EMAC1MADRSET : longWord absolute ($BF8892A8);
  EMACxMADRSET : longWord absolute ($BF8892A8);
  EMAC1MADRINV : longWord absolute ($BF8892AC);
  EMACxMADRINV : longWord absolute ($BF8892AC);
  EMAC1MWTD : longWord absolute ($BF8892B0);
type
  TEMAC1MWTDbits = bitpacked record
  case integer of
  0 : (
    MWTD : 0..65535;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  EMAC1MWTDbits: TEMAC1MWTDbits absolute ($BF8892B0);
  EMACxMWTD : longWord absolute ($BF8892B0);
type
  TEMACxMWTDbits = bitpacked record
  case integer of
  0 : (
    MWTD : 0..65535;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  EMACxMWTDbits: TEMACxMWTDbits absolute ($BF8892B0);
  EMAC1MWTDCLR : longWord absolute ($BF8892B4);
  EMACxMWTDCLR : longWord absolute ($BF8892B4);
  EMAC1MWTDSET : longWord absolute ($BF8892B8);
  EMACxMWTDSET : longWord absolute ($BF8892B8);
  EMAC1MWTDINV : longWord absolute ($BF8892BC);
  EMACxMWTDINV : longWord absolute ($BF8892BC);
  EMAC1MRDD : longWord absolute ($BF8892C0);
type
  TEMAC1MRDDbits = bitpacked record
  case integer of
  0 : (
    MRDD : 0..65535;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  EMAC1MRDDbits: TEMAC1MRDDbits absolute ($BF8892C0);
  EMACxMRDD : longWord absolute ($BF8892C0);
type
  TEMACxMRDDbits = bitpacked record
  case integer of
  0 : (
    MRDD : 0..65535;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  EMACxMRDDbits: TEMACxMRDDbits absolute ($BF8892C0);
  EMAC1MRDDCLR : longWord absolute ($BF8892C4);
  EMACxMRDDCLR : longWord absolute ($BF8892C4);
  EMAC1MRDDSET : longWord absolute ($BF8892C8);
  EMACxMRDDSET : longWord absolute ($BF8892C8);
  EMAC1MRDDINV : longWord absolute ($BF8892CC);
  EMACxMRDDINV : longWord absolute ($BF8892CC);
  EMAC1MIND : longWord absolute ($BF8892D0);
type
  TEMAC1MINDbits = bitpacked record
  case integer of
  0 : (
    MIIMBUSY : 0..1;
    SCAN : 0..1;
    NOTVALID : 0..1;
    LINKFAIL : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  EMAC1MINDbits: TEMAC1MINDbits absolute ($BF8892D0);
  EMACxMIND : longWord absolute ($BF8892D0);
type
  TEMACxMINDbits = bitpacked record
  case integer of
  0 : (
    MIIMBUSY : 0..1;
    SCAN : 0..1;
    NOTVALID : 0..1;
    LINKFAIL : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  EMACxMINDbits: TEMACxMINDbits absolute ($BF8892D0);
  EMAC1MINDCLR : longWord absolute ($BF8892D4);
  EMACxMINDCLR : longWord absolute ($BF8892D4);
  EMAC1MINDSET : longWord absolute ($BF8892D8);
  EMACxMINDSET : longWord absolute ($BF8892D8);
  EMAC1MINDINV : longWord absolute ($BF8892DC);
  EMACxMINDINV : longWord absolute ($BF8892DC);
  EMAC1SA0 : longWord absolute ($BF889300);
type
  TEMAC1SA0bits = bitpacked record
  case integer of
  0 : (
    STNADDR5 : 0..255;
    STNADDR6 : 0..255;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  EMAC1SA0bits: TEMAC1SA0bits absolute ($BF889300);
  EMACxSA0 : longWord absolute ($BF889300);
type
  TEMACxSA0bits = bitpacked record
  case integer of
  0 : (
    STNADDR5 : 0..255;
    STNADDR6 : 0..255;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  EMACxSA0bits: TEMACxSA0bits absolute ($BF889300);
  EMAC1SA0CLR : longWord absolute ($BF889304);
  EMACxSA0CLR : longWord absolute ($BF889304);
  EMAC1SA0SET : longWord absolute ($BF889308);
  EMACxSA0SET : longWord absolute ($BF889308);
  EMAC1SA0INV : longWord absolute ($BF88930C);
  EMACxSA0INV : longWord absolute ($BF88930C);
  EMAC1SA1 : longWord absolute ($BF889310);
type
  TEMAC1SA1bits = bitpacked record
  case integer of
  0 : (
    STNADDR3 : 0..255;
    STNADDR4 : 0..255;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  EMAC1SA1bits: TEMAC1SA1bits absolute ($BF889310);
  EMACxSA1 : longWord absolute ($BF889310);
type
  TEMACxSA1bits = bitpacked record
  case integer of
  0 : (
    STNADDR3 : 0..255;
    STNADDR4 : 0..255;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  EMACxSA1bits: TEMACxSA1bits absolute ($BF889310);
  EMAC1SA1CLR : longWord absolute ($BF889314);
  EMACxSA1CLR : longWord absolute ($BF889314);
  EMAC1SA1SET : longWord absolute ($BF889318);
  EMACxSA1SET : longWord absolute ($BF889318);
  EMAC1SA1INV : longWord absolute ($BF88931C);
  EMACxSA1INV : longWord absolute ($BF88931C);
  EMAC1SA2 : longWord absolute ($BF889320);
type
  TEMAC1SA2bits = bitpacked record
  case integer of
  0 : (
    STNADDR1 : 0..255;
    STNADDR2 : 0..255;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  EMAC1SA2bits: TEMAC1SA2bits absolute ($BF889320);
  EMACxSA2 : longWord absolute ($BF889320);
type
  TEMACxSA2bits = bitpacked record
  case integer of
  0 : (
    STNADDR1 : 0..255;
    STNADDR2 : 0..255;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  EMACxSA2bits: TEMACxSA2bits absolute ($BF889320);
  EMAC1SA2CLR : longWord absolute ($BF889324);
  EMACxSA2CLR : longWord absolute ($BF889324);
  EMAC1SA2SET : longWord absolute ($BF889328);
  EMACxSA2SET : longWord absolute ($BF889328);
  EMAC1SA2INV : longWord absolute ($BF88932C);
  EMACxSA2INV : longWord absolute ($BF88932C);
  C1CON : longWord absolute ($BF88B000);
type
  TC1CONbits = bitpacked record
  case integer of
  0 : (
    DNCNT : 0..31;
    RESERVED0 : 0..63;
    CANBUSY : 0..1;
    RESERVED1 : 0..1;
    SIDL : 0..1;
    RESERVED2 : 0..1;
    ON : 0..1;
    RESERVED3 : 0..15;
    CANCAP : 0..1;
    OPMOD : 0..7;
    REQOP : 0..7;
    ABAT : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1CONbits: TC1CONbits absolute ($BF88B000);
  C1CONCLR : longWord absolute ($BF88B004);
  C1CONSET : longWord absolute ($BF88B008);
  C1CONINV : longWord absolute ($BF88B00C);
  C1CFG : longWord absolute ($BF88B010);
type
  TC1CFGbits = bitpacked record
  case integer of
  0 : (
    BRP : 0..63;
    SJW : 0..3;
    PRSEG : 0..7;
    SEG1PH : 0..7;
    SAM : 0..1;
    SEG2PHTS : 0..1;
    SEG2PH : 0..7;
    RESERVED0 : 0..7;
    WAKFIL : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1CFGbits: TC1CFGbits absolute ($BF88B010);
  C1CFGCLR : longWord absolute ($BF88B014);
  C1CFGSET : longWord absolute ($BF88B018);
  C1CFGINV : longWord absolute ($BF88B01C);
  C1INT : longWord absolute ($BF88B020);
type
  TC1INTbits = bitpacked record
  case integer of
  0 : (
    TBIF : 0..1;
    RBIF : 0..1;
    CTMRIF : 0..1;
    MODIF : 0..1;
    RESERVED0 : 0..127;
    RBOVIF : 0..1;
    SERRIF : 0..1;
    CERRIF : 0..1;
    WAKIF : 0..1;
    IVRIF : 0..1;
    TBIE : 0..1;
    RBIE : 0..1;
    CTMRIE : 0..1;
    MODIE : 0..1;
    RESERVED1 : 0..127;
    RBOVIE : 0..1;
    SERRIE : 0..1;
    CERRIE : 0..1;
    WAKIE : 0..1;
    IVRIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1INTbits: TC1INTbits absolute ($BF88B020);
  C1INTCLR : longWord absolute ($BF88B024);
  C1INTSET : longWord absolute ($BF88B028);
  C1INTINV : longWord absolute ($BF88B02C);
  C1VEC : longWord absolute ($BF88B030);
type
  TC1VECbits = bitpacked record
  case integer of
  0 : (
    ICODE : 0..127;
    RESERVED0 : 0..1;
    FILHIT : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1VECbits: TC1VECbits absolute ($BF88B030);
  C1VECCLR : longWord absolute ($BF88B034);
  C1VECSET : longWord absolute ($BF88B038);
  C1VECINV : longWord absolute ($BF88B03C);
  C1TREC : longWord absolute ($BF88B040);
type
  TC1TRECbits = bitpacked record
  case integer of
  0 : (
    RERRCNT : 0..255;
    TERRCNT : 0..255;
    EWARN : 0..1;
    RXWARN : 0..1;
    TXWARN : 0..1;
    RXBP : 0..1;
    TXBP : 0..1;
    TXBO : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1TRECbits: TC1TRECbits absolute ($BF88B040);
  C1TRECCLR : longWord absolute ($BF88B044);
  C1TRECSET : longWord absolute ($BF88B048);
  C1TRECINV : longWord absolute ($BF88B04C);
  C1FSTAT : longWord absolute ($BF88B050);
type
  TC1FSTATbits = bitpacked record
  case integer of
  0 : (
    FIFOIP : 0..4294967295;
  );
  1 : (
    FIFOIP0 : 0..1;
    FIFOIP1 : 0..1;
    FIFOIP2 : 0..1;
    FIFOIP3 : 0..1;
    FIFOIP4 : 0..1;
    FIFOIP5 : 0..1;
    FIFOIP6 : 0..1;
    FIFOIP7 : 0..1;
    FIFOIP8 : 0..1;
    FIFOIP9 : 0..1;
    FIFOIP10 : 0..1;
    FIFOIP11 : 0..1;
    FIFOIP12 : 0..1;
    FIFOIP13 : 0..1;
    FIFOIP14 : 0..1;
    FIFOIP15 : 0..1;
    FIFOIP16 : 0..1;
    FIFOIP17 : 0..1;
    FIFOIP18 : 0..1;
    FIFOIP19 : 0..1;
    FIFOIP20 : 0..1;
    FIFOIP21 : 0..1;
    FIFOIP22 : 0..1;
    FIFOIP23 : 0..1;
    FIFOIP24 : 0..1;
    FIFOIP25 : 0..1;
    FIFOIP26 : 0..1;
    FIFOIP27 : 0..1;
    FIFOIP28 : 0..1;
    FIFOIP29 : 0..1;
    FIFOIP30 : 0..1;
    FIFOIP31 : 0..1;
  );
  2 : (
    w : 0..4294967295;
  );
  end;
var
  C1FSTATbits: TC1FSTATbits absolute ($BF88B050);
  C1FSTATCLR : longWord absolute ($BF88B054);
  C1FSTATSET : longWord absolute ($BF88B058);
  C1FSTATINV : longWord absolute ($BF88B05C);
  C1RXOVF : longWord absolute ($BF88B060);
type
  TC1RXOVFbits = bitpacked record
  case integer of
  0 : (
    RXOVF : 0..4294967295;
  );
  1 : (
    RXOVF0 : 0..1;
    RXOVF1 : 0..1;
    RXOVF2 : 0..1;
    RXOVF3 : 0..1;
    RXOVF4 : 0..1;
    RXOVF5 : 0..1;
    RXOVF6 : 0..1;
    RXOVF7 : 0..1;
    RXOVF8 : 0..1;
    RXOVF9 : 0..1;
    RXOVF10 : 0..1;
    RXOVF11 : 0..1;
    RXOVF12 : 0..1;
    RXOVF13 : 0..1;
    RXOVF14 : 0..1;
    RXOVF15 : 0..1;
    RXOVF16 : 0..1;
    RXOVF17 : 0..1;
    RXOVF18 : 0..1;
    RXOVF19 : 0..1;
    RXOVF20 : 0..1;
    RXOVF21 : 0..1;
    RXOVF22 : 0..1;
    RXOVF23 : 0..1;
    RXOVF24 : 0..1;
    RXOVF25 : 0..1;
    RXOVF26 : 0..1;
    RXOVF27 : 0..1;
    RXOVF28 : 0..1;
    RXOVF29 : 0..1;
    RXOVF30 : 0..1;
    RXOVF31 : 0..1;
  );
  2 : (
    w : 0..4294967295;
  );
  end;
var
  C1RXOVFbits: TC1RXOVFbits absolute ($BF88B060);
  C1RXOVFCLR : longWord absolute ($BF88B064);
  C1RXOVFSET : longWord absolute ($BF88B068);
  C1RXOVFINV : longWord absolute ($BF88B06C);
  C1TMR : longWord absolute ($BF88B070);
type
  TC1TMRbits = bitpacked record
  case integer of
  0 : (
    CANTSPRE : 0..65535;
    CANTS : 0..65535;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1TMRbits: TC1TMRbits absolute ($BF88B070);
  C1TMRCLR : longWord absolute ($BF88B074);
  C1TMRSET : longWord absolute ($BF88B078);
  C1TMRINV : longWord absolute ($BF88B07C);
  C1RXM0 : longWord absolute ($BF88B080);
type
  TC1RXM0bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    MIDE : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1RXM0bits: TC1RXM0bits absolute ($BF88B080);
  C1RXM0CLR : longWord absolute ($BF88B084);
  C1RXM0SET : longWord absolute ($BF88B088);
  C1RXM0INV : longWord absolute ($BF88B08C);
  C1RXM1 : longWord absolute ($BF88B090);
type
  TC1RXM1bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    MIDE : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1RXM1bits: TC1RXM1bits absolute ($BF88B090);
  C1RXM1CLR : longWord absolute ($BF88B094);
  C1RXM1SET : longWord absolute ($BF88B098);
  C1RXM1INV : longWord absolute ($BF88B09C);
  C1RXM2 : longWord absolute ($BF88B0A0);
type
  TC1RXM2bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    MIDE : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1RXM2bits: TC1RXM2bits absolute ($BF88B0A0);
  C1RXM2CLR : longWord absolute ($BF88B0A4);
  C1RXM2SET : longWord absolute ($BF88B0A8);
  C1RXM2INV : longWord absolute ($BF88B0AC);
  C1RXM3 : longWord absolute ($BF88B0B0);
type
  TC1RXM3bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    MIDE : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1RXM3bits: TC1RXM3bits absolute ($BF88B0B0);
  C1RXM3CLR : longWord absolute ($BF88B0B4);
  C1RXM3SET : longWord absolute ($BF88B0B8);
  C1RXM3INV : longWord absolute ($BF88B0BC);
  C1FLTCON0 : longWord absolute ($BF88B0C0);
type
  TC1FLTCON0bits = bitpacked record
  case integer of
  0 : (
    FSEL0 : 0..31;
    MSEL0 : 0..3;
    FLTEN0 : 0..1;
    FSEL1 : 0..31;
    MSEL1 : 0..3;
    FLTEN1 : 0..1;
    FSEL2 : 0..31;
    MSEL2 : 0..3;
    FLTEN2 : 0..1;
    FSEL3 : 0..31;
    MSEL3 : 0..3;
    FLTEN3 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FLTCON0bits: TC1FLTCON0bits absolute ($BF88B0C0);
  C1FLTCON0CLR : longWord absolute ($BF88B0C4);
  C1FLTCON0SET : longWord absolute ($BF88B0C8);
  C1FLTCON0INV : longWord absolute ($BF88B0CC);
  C1FLTCON1 : longWord absolute ($BF88B0D0);
type
  TC1FLTCON1bits = bitpacked record
  case integer of
  0 : (
    FSEL4 : 0..31;
    MSEL4 : 0..3;
    FLTEN4 : 0..1;
    FSEL5 : 0..31;
    MSEL5 : 0..3;
    FLTEN5 : 0..1;
    FSEL6 : 0..31;
    MSEL6 : 0..3;
    FLTEN6 : 0..1;
    FSEL7 : 0..31;
    MSEL7 : 0..3;
    FLTEN7 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FLTCON1bits: TC1FLTCON1bits absolute ($BF88B0D0);
  C1FLTCON1CLR : longWord absolute ($BF88B0D4);
  C1FLTCON1SET : longWord absolute ($BF88B0D8);
  C1FLTCON1INV : longWord absolute ($BF88B0DC);
  C1FLTCON2 : longWord absolute ($BF88B0E0);
type
  TC1FLTCON2bits = bitpacked record
  case integer of
  0 : (
    FSEL8 : 0..31;
    MSEL8 : 0..3;
    FLTEN8 : 0..1;
    FSEL9 : 0..31;
    MSEL9 : 0..3;
    FLTEN9 : 0..1;
    FSEL10 : 0..31;
    MSEL10 : 0..3;
    FLTEN10 : 0..1;
    FSEL11 : 0..31;
    MSEL11 : 0..3;
    FLTEN11 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FLTCON2bits: TC1FLTCON2bits absolute ($BF88B0E0);
  C1FLTCON2CLR : longWord absolute ($BF88B0E4);
  C1FLTCON2SET : longWord absolute ($BF88B0E8);
  C1FLTCON2INV : longWord absolute ($BF88B0EC);
  C1FLTCON3 : longWord absolute ($BF88B0F0);
type
  TC1FLTCON3bits = bitpacked record
  case integer of
  0 : (
    FSEL12 : 0..31;
    MSEL12 : 0..3;
    FLTEN12 : 0..1;
    FSEL13 : 0..31;
    MSEL13 : 0..3;
    FLTEN13 : 0..1;
    FSEL14 : 0..31;
    MSEL14 : 0..3;
    FLTEN14 : 0..1;
    FSEL15 : 0..31;
    MSEL15 : 0..3;
    FLTEN15 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FLTCON3bits: TC1FLTCON3bits absolute ($BF88B0F0);
  C1FLTCON3CLR : longWord absolute ($BF88B0F4);
  C1FLTCON3SET : longWord absolute ($BF88B0F8);
  C1FLTCON3INV : longWord absolute ($BF88B0FC);
  C1FLTCON4 : longWord absolute ($BF88B100);
type
  TC1FLTCON4bits = bitpacked record
  case integer of
  0 : (
    FSEL16 : 0..31;
    MSEL16 : 0..3;
    FLTEN16 : 0..1;
    FSEL17 : 0..31;
    MSEL17 : 0..3;
    FLTEN17 : 0..1;
    FSEL18 : 0..31;
    MSEL18 : 0..3;
    FLTEN18 : 0..1;
    FSEL19 : 0..31;
    MSEL19 : 0..3;
    FLTEN19 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FLTCON4bits: TC1FLTCON4bits absolute ($BF88B100);
  C1FLTCON4CLR : longWord absolute ($BF88B104);
  C1FLTCON4SET : longWord absolute ($BF88B108);
  C1FLTCON4INV : longWord absolute ($BF88B10C);
  C1FLTCON5 : longWord absolute ($BF88B110);
type
  TC1FLTCON5bits = bitpacked record
  case integer of
  0 : (
    FSEL20 : 0..31;
    MSEL20 : 0..3;
    FLTEN20 : 0..1;
    FSEL21 : 0..31;
    MSEL21 : 0..3;
    FLTEN21 : 0..1;
    FSEL22 : 0..31;
    MSEL22 : 0..3;
    FLTEN22 : 0..1;
    FSEL23 : 0..31;
    MSEL23 : 0..3;
    FLTEN23 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FLTCON5bits: TC1FLTCON5bits absolute ($BF88B110);
  C1FLTCON5CLR : longWord absolute ($BF88B114);
  C1FLTCON5SET : longWord absolute ($BF88B118);
  C1FLTCON5INV : longWord absolute ($BF88B11C);
  C1FLTCON6 : longWord absolute ($BF88B120);
type
  TC1FLTCON6bits = bitpacked record
  case integer of
  0 : (
    FSEL24 : 0..31;
    MSEL24 : 0..3;
    FLTEN24 : 0..1;
    FSEL25 : 0..31;
    MSEL25 : 0..3;
    FLTEN25 : 0..1;
    FSEL26 : 0..31;
    MSEL26 : 0..3;
    FLTEN26 : 0..1;
    FSEL27 : 0..31;
    MSEL27 : 0..3;
    FLTEN27 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FLTCON6bits: TC1FLTCON6bits absolute ($BF88B120);
  C1FLTCON6CLR : longWord absolute ($BF88B124);
  C1FLTCON6SET : longWord absolute ($BF88B128);
  C1FLTCON6INV : longWord absolute ($BF88B12C);
  C1FLTCON7 : longWord absolute ($BF88B130);
type
  TC1FLTCON7bits = bitpacked record
  case integer of
  0 : (
    FSEL28 : 0..31;
    MSEL28 : 0..3;
    FLTEN28 : 0..1;
    FSEL29 : 0..31;
    MSEL29 : 0..3;
    FLTEN29 : 0..1;
    FSEL30 : 0..31;
    MSEL30 : 0..3;
    FLTEN30 : 0..1;
    FSEL31 : 0..31;
    MSEL31 : 0..3;
    FLTEN31 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FLTCON7bits: TC1FLTCON7bits absolute ($BF88B130);
  C1FLTCON7CLR : longWord absolute ($BF88B134);
  C1FLTCON7SET : longWord absolute ($BF88B138);
  C1FLTCON7INV : longWord absolute ($BF88B13C);
  C1RXF0 : longWord absolute ($BF88B140);
type
  TC1RXF0bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    EXID : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1RXF0bits: TC1RXF0bits absolute ($BF88B140);
  C1RXF0CLR : longWord absolute ($BF88B144);
  C1RXF0SET : longWord absolute ($BF88B148);
  C1RXF0INV : longWord absolute ($BF88B14C);
  C1RXF1 : longWord absolute ($BF88B150);
type
  TC1RXF1bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    EXID : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1RXF1bits: TC1RXF1bits absolute ($BF88B150);
  C1RXF1CLR : longWord absolute ($BF88B154);
  C1RXF1SET : longWord absolute ($BF88B158);
  C1RXF1INV : longWord absolute ($BF88B15C);
  C1RXF2 : longWord absolute ($BF88B160);
type
  TC1RXF2bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    EXID : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1RXF2bits: TC1RXF2bits absolute ($BF88B160);
  C1RXF2CLR : longWord absolute ($BF88B164);
  C1RXF2SET : longWord absolute ($BF88B168);
  C1RXF2INV : longWord absolute ($BF88B16C);
  C1RXF3 : longWord absolute ($BF88B170);
type
  TC1RXF3bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    EXID : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1RXF3bits: TC1RXF3bits absolute ($BF88B170);
  C1RXF3CLR : longWord absolute ($BF88B174);
  C1RXF3SET : longWord absolute ($BF88B178);
  C1RXF3INV : longWord absolute ($BF88B17C);
  C1RXF4 : longWord absolute ($BF88B180);
type
  TC1RXF4bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    EXID : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1RXF4bits: TC1RXF4bits absolute ($BF88B180);
  C1RXF4CLR : longWord absolute ($BF88B184);
  C1RXF4SET : longWord absolute ($BF88B188);
  C1RXF4INV : longWord absolute ($BF88B18C);
  C1RXF5 : longWord absolute ($BF88B190);
type
  TC1RXF5bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    EXID : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1RXF5bits: TC1RXF5bits absolute ($BF88B190);
  C1RXF5CLR : longWord absolute ($BF88B194);
  C1RXF5SET : longWord absolute ($BF88B198);
  C1RXF5INV : longWord absolute ($BF88B19C);
  C1RXF6 : longWord absolute ($BF88B1A0);
type
  TC1RXF6bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    EXID : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1RXF6bits: TC1RXF6bits absolute ($BF88B1A0);
  C1RXF6CLR : longWord absolute ($BF88B1A4);
  C1RXF6SET : longWord absolute ($BF88B1A8);
  C1RXF6INV : longWord absolute ($BF88B1AC);
  C1RXF7 : longWord absolute ($BF88B1B0);
type
  TC1RXF7bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    EXID : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1RXF7bits: TC1RXF7bits absolute ($BF88B1B0);
  C1RXF7CLR : longWord absolute ($BF88B1B4);
  C1RXF7SET : longWord absolute ($BF88B1B8);
  C1RXF7INV : longWord absolute ($BF88B1BC);
  C1RXF8 : longWord absolute ($BF88B1C0);
type
  TC1RXF8bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    EXID : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1RXF8bits: TC1RXF8bits absolute ($BF88B1C0);
  C1RXF8CLR : longWord absolute ($BF88B1C4);
  C1RXF8SET : longWord absolute ($BF88B1C8);
  C1RXF8INV : longWord absolute ($BF88B1CC);
  C1RXF9 : longWord absolute ($BF88B1D0);
type
  TC1RXF9bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    EXID : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1RXF9bits: TC1RXF9bits absolute ($BF88B1D0);
  C1RXF9CLR : longWord absolute ($BF88B1D4);
  C1RXF9SET : longWord absolute ($BF88B1D8);
  C1RXF9INV : longWord absolute ($BF88B1DC);
  C1RXF10 : longWord absolute ($BF88B1E0);
type
  TC1RXF10bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    EXID : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1RXF10bits: TC1RXF10bits absolute ($BF88B1E0);
  C1RXF10CLR : longWord absolute ($BF88B1E4);
  C1RXF10SET : longWord absolute ($BF88B1E8);
  C1RXF10INV : longWord absolute ($BF88B1EC);
  C1RXF11 : longWord absolute ($BF88B1F0);
type
  TC1RXF11bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    EXID : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1RXF11bits: TC1RXF11bits absolute ($BF88B1F0);
  C1RXF11CLR : longWord absolute ($BF88B1F4);
  C1RXF11SET : longWord absolute ($BF88B1F8);
  C1RXF11INV : longWord absolute ($BF88B1FC);
  C1RXF12 : longWord absolute ($BF88B200);
type
  TC1RXF12bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    EXID : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1RXF12bits: TC1RXF12bits absolute ($BF88B200);
  C1RXF12CLR : longWord absolute ($BF88B204);
  C1RXF12SET : longWord absolute ($BF88B208);
  C1RXF12INV : longWord absolute ($BF88B20C);
  C1RXF13 : longWord absolute ($BF88B210);
type
  TC1RXF13bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    EXID : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1RXF13bits: TC1RXF13bits absolute ($BF88B210);
  C1RXF13CLR : longWord absolute ($BF88B214);
  C1RXF13SET : longWord absolute ($BF88B218);
  C1RXF13INV : longWord absolute ($BF88B21C);
  C1RXF14 : longWord absolute ($BF88B220);
type
  TC1RXF14bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    EXID : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1RXF14bits: TC1RXF14bits absolute ($BF88B220);
  C1RXF14CLR : longWord absolute ($BF88B224);
  C1RXF14SET : longWord absolute ($BF88B228);
  C1RXF14INV : longWord absolute ($BF88B22C);
  C1RXF15 : longWord absolute ($BF88B230);
type
  TC1RXF15bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    EXID : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1RXF15bits: TC1RXF15bits absolute ($BF88B230);
  C1RXF15CLR : longWord absolute ($BF88B234);
  C1RXF15SET : longWord absolute ($BF88B238);
  C1RXF15INV : longWord absolute ($BF88B23C);
  C1RXF16 : longWord absolute ($BF88B240);
type
  TC1RXF16bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    EXID : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1RXF16bits: TC1RXF16bits absolute ($BF88B240);
  C1RXF16CLR : longWord absolute ($BF88B244);
  C1RXF16SET : longWord absolute ($BF88B248);
  C1RXF16INV : longWord absolute ($BF88B24C);
  C1RXF17 : longWord absolute ($BF88B250);
type
  TC1RXF17bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    EXID : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1RXF17bits: TC1RXF17bits absolute ($BF88B250);
  C1RXF17CLR : longWord absolute ($BF88B254);
  C1RXF17SET : longWord absolute ($BF88B258);
  C1RXF17INV : longWord absolute ($BF88B25C);
  C1RXF18 : longWord absolute ($BF88B260);
type
  TC1RXF18bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    EXID : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1RXF18bits: TC1RXF18bits absolute ($BF88B260);
  C1RXF18CLR : longWord absolute ($BF88B264);
  C1RXF18SET : longWord absolute ($BF88B268);
  C1RXF18INV : longWord absolute ($BF88B26C);
  C1RXF19 : longWord absolute ($BF88B270);
type
  TC1RXF19bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    EXID : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1RXF19bits: TC1RXF19bits absolute ($BF88B270);
  C1RXF19CLR : longWord absolute ($BF88B274);
  C1RXF19SET : longWord absolute ($BF88B278);
  C1RXF19INV : longWord absolute ($BF88B27C);
  C1RXF20 : longWord absolute ($BF88B280);
type
  TC1RXF20bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    EXID : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1RXF20bits: TC1RXF20bits absolute ($BF88B280);
  C1RXF20CLR : longWord absolute ($BF88B284);
  C1RXF20SET : longWord absolute ($BF88B288);
  C1RXF20INV : longWord absolute ($BF88B28C);
  C1RXF21 : longWord absolute ($BF88B290);
type
  TC1RXF21bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    EXID : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1RXF21bits: TC1RXF21bits absolute ($BF88B290);
  C1RXF21CLR : longWord absolute ($BF88B294);
  C1RXF21SET : longWord absolute ($BF88B298);
  C1RXF21INV : longWord absolute ($BF88B29C);
  C1RXF22 : longWord absolute ($BF88B2A0);
type
  TC1RXF22bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    EXID : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1RXF22bits: TC1RXF22bits absolute ($BF88B2A0);
  C1RXF22CLR : longWord absolute ($BF88B2A4);
  C1RXF22SET : longWord absolute ($BF88B2A8);
  C1RXF22INV : longWord absolute ($BF88B2AC);
  C1RXF23 : longWord absolute ($BF88B2B0);
type
  TC1RXF23bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    EXID : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1RXF23bits: TC1RXF23bits absolute ($BF88B2B0);
  C1RXF23CLR : longWord absolute ($BF88B2B4);
  C1RXF23SET : longWord absolute ($BF88B2B8);
  C1RXF23INV : longWord absolute ($BF88B2BC);
  C1RXF24 : longWord absolute ($BF88B2C0);
type
  TC1RXF24bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    EXID : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1RXF24bits: TC1RXF24bits absolute ($BF88B2C0);
  C1RXF24CLR : longWord absolute ($BF88B2C4);
  C1RXF24SET : longWord absolute ($BF88B2C8);
  C1RXF24INV : longWord absolute ($BF88B2CC);
  C1RXF25 : longWord absolute ($BF88B2D0);
type
  TC1RXF25bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    EXID : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1RXF25bits: TC1RXF25bits absolute ($BF88B2D0);
  C1RXF25CLR : longWord absolute ($BF88B2D4);
  C1RXF25SET : longWord absolute ($BF88B2D8);
  C1RXF25INV : longWord absolute ($BF88B2DC);
  C1RXF26 : longWord absolute ($BF88B2E0);
type
  TC1RXF26bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    EXID : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1RXF26bits: TC1RXF26bits absolute ($BF88B2E0);
  C1RXF26CLR : longWord absolute ($BF88B2E4);
  C1RXF26SET : longWord absolute ($BF88B2E8);
  C1RXF26INV : longWord absolute ($BF88B2EC);
  C1RXF27 : longWord absolute ($BF88B2F0);
type
  TC1RXF27bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    EXID : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1RXF27bits: TC1RXF27bits absolute ($BF88B2F0);
  C1RXF27CLR : longWord absolute ($BF88B2F4);
  C1RXF27SET : longWord absolute ($BF88B2F8);
  C1RXF27INV : longWord absolute ($BF88B2FC);
  C1RXF28 : longWord absolute ($BF88B300);
type
  TC1RXF28bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    EXID : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1RXF28bits: TC1RXF28bits absolute ($BF88B300);
  C1RXF28CLR : longWord absolute ($BF88B304);
  C1RXF28SET : longWord absolute ($BF88B308);
  C1RXF28INV : longWord absolute ($BF88B30C);
  C1RXF29 : longWord absolute ($BF88B310);
type
  TC1RXF29bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    EXID : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1RXF29bits: TC1RXF29bits absolute ($BF88B310);
  C1RXF29CLR : longWord absolute ($BF88B314);
  C1RXF29SET : longWord absolute ($BF88B318);
  C1RXF29INV : longWord absolute ($BF88B31C);
  C1RXF30 : longWord absolute ($BF88B320);
type
  TC1RXF30bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    EXID : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1RXF30bits: TC1RXF30bits absolute ($BF88B320);
  C1RXF30CLR : longWord absolute ($BF88B324);
  C1RXF30SET : longWord absolute ($BF88B328);
  C1RXF30INV : longWord absolute ($BF88B32C);
  C1RXF31 : longWord absolute ($BF88B330);
type
  TC1RXF31bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    EXID : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1RXF31bits: TC1RXF31bits absolute ($BF88B330);
  C1RXF31CLR : longWord absolute ($BF88B334);
  C1RXF31SET : longWord absolute ($BF88B338);
  C1RXF31INV : longWord absolute ($BF88B33C);
  C1FIFOBA : longWord absolute ($BF88B340);
  C1FIFOBACLR : longWord absolute ($BF88B344);
  C1FIFOBASET : longWord absolute ($BF88B348);
  C1FIFOBAINV : longWord absolute ($BF88B34C);
  C1FIFOCON0 : longWord absolute ($BF88B350);
type
  TC1FIFOCON0bits = bitpacked record
  case integer of
  0 : (
    TXPRI : 0..3;
    RTREN : 0..1;
    TXREQ : 0..1;
    TXERR : 0..1;
    TXLARB : 0..1;
    TXABAT : 0..1;
    TXEN : 0..1;
    RESERVED0 : 0..15;
    DONLY : 0..1;
    UINC : 0..1;
    FRESET : 0..1;
    RESERVED1 : 0..1;
    FSIZE : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOCON0bits: TC1FIFOCON0bits absolute ($BF88B350);
  C1FIFOCON0CLR : longWord absolute ($BF88B354);
  C1FIFOCON0SET : longWord absolute ($BF88B358);
  C1FIFOCON0INV : longWord absolute ($BF88B35C);
  C1FIFOINT0 : longWord absolute ($BF88B360);
type
  TC1FIFOINT0bits = bitpacked record
  case integer of
  0 : (
    RXNEMPTYIF : 0..1;
    RXHALFIF : 0..1;
    RXFULLIF : 0..1;
    RXOVFLIF : 0..1;
    RESERVED0 : 0..15;
    TXEMPTYIF : 0..1;
    TXHALFIF : 0..1;
    TXNFULLIF : 0..1;
    RESERVED1 : 0..31;
    RXNEMPTYIE : 0..1;
    RXHALFIE : 0..1;
    RXFULLIE : 0..1;
    RXOVFLIE : 0..1;
    RESERVED2 : 0..15;
    TXEMPTYIE : 0..1;
    TXHALFIE : 0..1;
    TXNFULLIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOINT0bits: TC1FIFOINT0bits absolute ($BF88B360);
  C1FIFOINT0CLR : longWord absolute ($BF88B364);
  C1FIFOINT0SET : longWord absolute ($BF88B368);
  C1FIFOINT0INV : longWord absolute ($BF88B36C);
  C1FIFOUA0 : longWord absolute ($BF88B370);
  C1FIFOUA0CLR : longWord absolute ($BF88B374);
  C1FIFOUA0SET : longWord absolute ($BF88B378);
  C1FIFOUA0INV : longWord absolute ($BF88B37C);
  C1FIFOCI0 : longWord absolute ($BF88B380);
type
  TC1FIFOCI0bits = bitpacked record
  case integer of
  0 : (
    CFIFOCI : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOCI0bits: TC1FIFOCI0bits absolute ($BF88B380);
  C1FIFOCI0CLR : longWord absolute ($BF88B384);
  C1FIFOCI0SET : longWord absolute ($BF88B388);
  C1FIFOCI0INV : longWord absolute ($BF88B38C);
  C1FIFOCON1 : longWord absolute ($BF88B390);
type
  TC1FIFOCON1bits = bitpacked record
  case integer of
  0 : (
    TXPRI : 0..3;
    RTREN : 0..1;
    TXREQ : 0..1;
    TXERR : 0..1;
    TXLARB : 0..1;
    TXABAT : 0..1;
    TXEN : 0..1;
    RESERVED0 : 0..15;
    DONLY : 0..1;
    UINC : 0..1;
    FRESET : 0..1;
    RESERVED1 : 0..1;
    FSIZE : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOCON1bits: TC1FIFOCON1bits absolute ($BF88B390);
  C1FIFOCON1CLR : longWord absolute ($BF88B394);
  C1FIFOCON1SET : longWord absolute ($BF88B398);
  C1FIFOCON1INV : longWord absolute ($BF88B39C);
  C1FIFOINT1 : longWord absolute ($BF88B3A0);
type
  TC1FIFOINT1bits = bitpacked record
  case integer of
  0 : (
    RXNEMPTYIF : 0..1;
    RXHALFIF : 0..1;
    RXFULLIF : 0..1;
    RXOVFLIF : 0..1;
    RESERVED0 : 0..15;
    TXEMPTYIF : 0..1;
    TXHALFIF : 0..1;
    TXNFULLIF : 0..1;
    RESERVED1 : 0..31;
    RXNEMPTYIE : 0..1;
    RXHALFIE : 0..1;
    RXFULLIE : 0..1;
    RXOVFLIE : 0..1;
    RESERVED2 : 0..15;
    TXEMPTYIE : 0..1;
    TXHALFIE : 0..1;
    TXNFULLIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOINT1bits: TC1FIFOINT1bits absolute ($BF88B3A0);
  C1FIFOINT1CLR : longWord absolute ($BF88B3A4);
  C1FIFOINT1SET : longWord absolute ($BF88B3A8);
  C1FIFOINT1INV : longWord absolute ($BF88B3AC);
  C1FIFOUA1 : longWord absolute ($BF88B3B0);
  C1FIFOUA1CLR : longWord absolute ($BF88B3B4);
  C1FIFOUA1SET : longWord absolute ($BF88B3B8);
  C1FIFOUA1INV : longWord absolute ($BF88B3BC);
  C1FIFOCI1 : longWord absolute ($BF88B3C0);
type
  TC1FIFOCI1bits = bitpacked record
  case integer of
  0 : (
    CFIFOCI : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOCI1bits: TC1FIFOCI1bits absolute ($BF88B3C0);
  C1FIFOCI1CLR : longWord absolute ($BF88B3C4);
  C1FIFOCI1SET : longWord absolute ($BF88B3C8);
  C1FIFOCI1INV : longWord absolute ($BF88B3CC);
  C1FIFOCON2 : longWord absolute ($BF88B3D0);
type
  TC1FIFOCON2bits = bitpacked record
  case integer of
  0 : (
    TXPRI : 0..3;
    RTREN : 0..1;
    TXREQ : 0..1;
    TXERR : 0..1;
    TXLARB : 0..1;
    TXABAT : 0..1;
    TXEN : 0..1;
    RESERVED0 : 0..15;
    DONLY : 0..1;
    UINC : 0..1;
    FRESET : 0..1;
    RESERVED1 : 0..1;
    FSIZE : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOCON2bits: TC1FIFOCON2bits absolute ($BF88B3D0);
  C1FIFOCON2CLR : longWord absolute ($BF88B3D4);
  C1FIFOCON2SET : longWord absolute ($BF88B3D8);
  C1FIFOCON2INV : longWord absolute ($BF88B3DC);
  C1FIFOINT2 : longWord absolute ($BF88B3E0);
type
  TC1FIFOINT2bits = bitpacked record
  case integer of
  0 : (
    RXNEMPTYIF : 0..1;
    RXHALFIF : 0..1;
    RXFULLIF : 0..1;
    RXOVFLIF : 0..1;
    RESERVED0 : 0..15;
    TXEMPTYIF : 0..1;
    TXHALFIF : 0..1;
    TXNFULLIF : 0..1;
    RESERVED1 : 0..31;
    RXNEMPTYIE : 0..1;
    RXHALFIE : 0..1;
    RXFULLIE : 0..1;
    RXOVFLIE : 0..1;
    RESERVED2 : 0..15;
    TXEMPTYIE : 0..1;
    TXHALFIE : 0..1;
    TXNFULLIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOINT2bits: TC1FIFOINT2bits absolute ($BF88B3E0);
  C1FIFOINT2CLR : longWord absolute ($BF88B3E4);
  C1FIFOINT2SET : longWord absolute ($BF88B3E8);
  C1FIFOINT2INV : longWord absolute ($BF88B3EC);
  C1FIFOUA2 : longWord absolute ($BF88B3F0);
  C1FIFOUA2CLR : longWord absolute ($BF88B3F4);
  C1FIFOUA2SET : longWord absolute ($BF88B3F8);
  C1FIFOUA2INV : longWord absolute ($BF88B3FC);
  C1FIFOCI2 : longWord absolute ($BF88B400);
type
  TC1FIFOCI2bits = bitpacked record
  case integer of
  0 : (
    CFIFOCI : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOCI2bits: TC1FIFOCI2bits absolute ($BF88B400);
  C1FIFOCI2CLR : longWord absolute ($BF88B404);
  C1FIFOCI2SET : longWord absolute ($BF88B408);
  C1FIFOCI2INV : longWord absolute ($BF88B40C);
  C1FIFOCON3 : longWord absolute ($BF88B410);
type
  TC1FIFOCON3bits = bitpacked record
  case integer of
  0 : (
    TXPRI : 0..3;
    RTREN : 0..1;
    TXREQ : 0..1;
    TXERR : 0..1;
    TXLARB : 0..1;
    TXABAT : 0..1;
    TXEN : 0..1;
    RESERVED0 : 0..15;
    DONLY : 0..1;
    UINC : 0..1;
    FRESET : 0..1;
    RESERVED1 : 0..1;
    FSIZE : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOCON3bits: TC1FIFOCON3bits absolute ($BF88B410);
  C1FIFOCON3CLR : longWord absolute ($BF88B414);
  C1FIFOCON3SET : longWord absolute ($BF88B418);
  C1FIFOCON3INV : longWord absolute ($BF88B41C);
  C1FIFOINT3 : longWord absolute ($BF88B420);
type
  TC1FIFOINT3bits = bitpacked record
  case integer of
  0 : (
    RXNEMPTYIF : 0..1;
    RXHALFIF : 0..1;
    RXFULLIF : 0..1;
    RXOVFLIF : 0..1;
    RESERVED0 : 0..15;
    TXEMPTYIF : 0..1;
    TXHALFIF : 0..1;
    TXNFULLIF : 0..1;
    RESERVED1 : 0..31;
    RXNEMPTYIE : 0..1;
    RXHALFIE : 0..1;
    RXFULLIE : 0..1;
    RXOVFLIE : 0..1;
    RESERVED2 : 0..15;
    TXEMPTYIE : 0..1;
    TXHALFIE : 0..1;
    TXNFULLIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOINT3bits: TC1FIFOINT3bits absolute ($BF88B420);
  C1FIFOINT3CLR : longWord absolute ($BF88B424);
  C1FIFOINT3SET : longWord absolute ($BF88B428);
  C1FIFOINT3INV : longWord absolute ($BF88B42C);
  C1FIFOUA3 : longWord absolute ($BF88B430);
  C1FIFOUA3CLR : longWord absolute ($BF88B434);
  C1FIFOUA3SET : longWord absolute ($BF88B438);
  C1FIFOUA3INV : longWord absolute ($BF88B43C);
  C1FIFOCI3 : longWord absolute ($BF88B440);
type
  TC1FIFOCI3bits = bitpacked record
  case integer of
  0 : (
    CFIFOCI : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOCI3bits: TC1FIFOCI3bits absolute ($BF88B440);
  C1FIFOCI3CLR : longWord absolute ($BF88B444);
  C1FIFOCI3SET : longWord absolute ($BF88B448);
  C1FIFOCI3INV : longWord absolute ($BF88B44C);
  C1FIFOCON4 : longWord absolute ($BF88B450);
type
  TC1FIFOCON4bits = bitpacked record
  case integer of
  0 : (
    TXPRI : 0..3;
    RTREN : 0..1;
    TXREQ : 0..1;
    TXERR : 0..1;
    TXLARB : 0..1;
    TXABAT : 0..1;
    TXEN : 0..1;
    RESERVED0 : 0..15;
    DONLY : 0..1;
    UINC : 0..1;
    FRESET : 0..1;
    RESERVED1 : 0..1;
    FSIZE : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOCON4bits: TC1FIFOCON4bits absolute ($BF88B450);
  C1FIFOCON4CLR : longWord absolute ($BF88B454);
  C1FIFOCON4SET : longWord absolute ($BF88B458);
  C1FIFOCON4INV : longWord absolute ($BF88B45C);
  C1FIFOINT4 : longWord absolute ($BF88B460);
type
  TC1FIFOINT4bits = bitpacked record
  case integer of
  0 : (
    RXNEMPTYIF : 0..1;
    RXHALFIF : 0..1;
    RXFULLIF : 0..1;
    RXOVFLIF : 0..1;
    RESERVED0 : 0..15;
    TXEMPTYIF : 0..1;
    TXHALFIF : 0..1;
    TXNFULLIF : 0..1;
    RESERVED1 : 0..31;
    RXNEMPTYIE : 0..1;
    RXHALFIE : 0..1;
    RXFULLIE : 0..1;
    RXOVFLIE : 0..1;
    RESERVED2 : 0..15;
    TXEMPTYIE : 0..1;
    TXHALFIE : 0..1;
    TXNFULLIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOINT4bits: TC1FIFOINT4bits absolute ($BF88B460);
  C1FIFOINT4CLR : longWord absolute ($BF88B464);
  C1FIFOINT4SET : longWord absolute ($BF88B468);
  C1FIFOINT4INV : longWord absolute ($BF88B46C);
  C1FIFOUA4 : longWord absolute ($BF88B470);
  C1FIFOUA4CLR : longWord absolute ($BF88B474);
  C1FIFOUA4SET : longWord absolute ($BF88B478);
  C1FIFOUA4INV : longWord absolute ($BF88B47C);
  C1FIFOCI4 : longWord absolute ($BF88B480);
type
  TC1FIFOCI4bits = bitpacked record
  case integer of
  0 : (
    CFIFOCI : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOCI4bits: TC1FIFOCI4bits absolute ($BF88B480);
  C1FIFOCI4CLR : longWord absolute ($BF88B484);
  C1FIFOCI4SET : longWord absolute ($BF88B488);
  C1FIFOCI4INV : longWord absolute ($BF88B48C);
  C1FIFOCON5 : longWord absolute ($BF88B490);
type
  TC1FIFOCON5bits = bitpacked record
  case integer of
  0 : (
    TXPRI : 0..3;
    RTREN : 0..1;
    TXREQ : 0..1;
    TXERR : 0..1;
    TXLARB : 0..1;
    TXABAT : 0..1;
    TXEN : 0..1;
    RESERVED0 : 0..15;
    DONLY : 0..1;
    UINC : 0..1;
    FRESET : 0..1;
    RESERVED1 : 0..1;
    FSIZE : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOCON5bits: TC1FIFOCON5bits absolute ($BF88B490);
  C1FIFOCON5CLR : longWord absolute ($BF88B494);
  C1FIFOCON5SET : longWord absolute ($BF88B498);
  C1FIFOCON5INV : longWord absolute ($BF88B49C);
  C1FIFOINT5 : longWord absolute ($BF88B4A0);
type
  TC1FIFOINT5bits = bitpacked record
  case integer of
  0 : (
    RXNEMPTYIF : 0..1;
    RXHALFIF : 0..1;
    RXFULLIF : 0..1;
    RXOVFLIF : 0..1;
    RESERVED0 : 0..15;
    TXEMPTYIF : 0..1;
    TXHALFIF : 0..1;
    TXNFULLIF : 0..1;
    RESERVED1 : 0..31;
    RXNEMPTYIE : 0..1;
    RXHALFIE : 0..1;
    RXFULLIE : 0..1;
    RXOVFLIE : 0..1;
    RESERVED2 : 0..15;
    TXEMPTYIE : 0..1;
    TXHALFIE : 0..1;
    TXNFULLIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOINT5bits: TC1FIFOINT5bits absolute ($BF88B4A0);
  C1FIFOINT5CLR : longWord absolute ($BF88B4A4);
  C1FIFOINT5SET : longWord absolute ($BF88B4A8);
  C1FIFOINT5INV : longWord absolute ($BF88B4AC);
  C1FIFOUA5 : longWord absolute ($BF88B4B0);
  C1FIFOUA5CLR : longWord absolute ($BF88B4B4);
  C1FIFOUA5SET : longWord absolute ($BF88B4B8);
  C1FIFOUA5INV : longWord absolute ($BF88B4BC);
  C1FIFOCI5 : longWord absolute ($BF88B4C0);
type
  TC1FIFOCI5bits = bitpacked record
  case integer of
  0 : (
    CFIFOCI : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOCI5bits: TC1FIFOCI5bits absolute ($BF88B4C0);
  C1FIFOCI5CLR : longWord absolute ($BF88B4C4);
  C1FIFOCI5SET : longWord absolute ($BF88B4C8);
  C1FIFOCI5INV : longWord absolute ($BF88B4CC);
  C1FIFOCON6 : longWord absolute ($BF88B4D0);
type
  TC1FIFOCON6bits = bitpacked record
  case integer of
  0 : (
    TXPRI : 0..3;
    RTREN : 0..1;
    TXREQ : 0..1;
    TXERR : 0..1;
    TXLARB : 0..1;
    TXABAT : 0..1;
    TXEN : 0..1;
    RESERVED0 : 0..15;
    DONLY : 0..1;
    UINC : 0..1;
    FRESET : 0..1;
    RESERVED1 : 0..1;
    FSIZE : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOCON6bits: TC1FIFOCON6bits absolute ($BF88B4D0);
  C1FIFOCON6CLR : longWord absolute ($BF88B4D4);
  C1FIFOCON6SET : longWord absolute ($BF88B4D8);
  C1FIFOCON6INV : longWord absolute ($BF88B4DC);
  C1FIFOINT6 : longWord absolute ($BF88B4E0);
type
  TC1FIFOINT6bits = bitpacked record
  case integer of
  0 : (
    RXNEMPTYIF : 0..1;
    RXHALFIF : 0..1;
    RXFULLIF : 0..1;
    RXOVFLIF : 0..1;
    RESERVED0 : 0..15;
    TXEMPTYIF : 0..1;
    TXHALFIF : 0..1;
    TXNFULLIF : 0..1;
    RESERVED1 : 0..31;
    RXNEMPTYIE : 0..1;
    RXHALFIE : 0..1;
    RXFULLIE : 0..1;
    RXOVFLIE : 0..1;
    RESERVED2 : 0..15;
    TXEMPTYIE : 0..1;
    TXHALFIE : 0..1;
    TXNFULLIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOINT6bits: TC1FIFOINT6bits absolute ($BF88B4E0);
  C1FIFOINT6CLR : longWord absolute ($BF88B4E4);
  C1FIFOINT6SET : longWord absolute ($BF88B4E8);
  C1FIFOINT6INV : longWord absolute ($BF88B4EC);
  C1FIFOUA6 : longWord absolute ($BF88B4F0);
  C1FIFOUA6CLR : longWord absolute ($BF88B4F4);
  C1FIFOUA6SET : longWord absolute ($BF88B4F8);
  C1FIFOUA6INV : longWord absolute ($BF88B4FC);
  C1FIFOCI6 : longWord absolute ($BF88B500);
type
  TC1FIFOCI6bits = bitpacked record
  case integer of
  0 : (
    CFIFOCI : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOCI6bits: TC1FIFOCI6bits absolute ($BF88B500);
  C1FIFOCI6CLR : longWord absolute ($BF88B504);
  C1FIFOCI6SET : longWord absolute ($BF88B508);
  C1FIFOCI6INV : longWord absolute ($BF88B50C);
  C1FIFOCON7 : longWord absolute ($BF88B510);
type
  TC1FIFOCON7bits = bitpacked record
  case integer of
  0 : (
    TXPRI : 0..3;
    RTREN : 0..1;
    TXREQ : 0..1;
    TXERR : 0..1;
    TXLARB : 0..1;
    TXABAT : 0..1;
    TXEN : 0..1;
    RESERVED0 : 0..15;
    DONLY : 0..1;
    UINC : 0..1;
    FRESET : 0..1;
    RESERVED1 : 0..1;
    FSIZE : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOCON7bits: TC1FIFOCON7bits absolute ($BF88B510);
  C1FIFOCON7CLR : longWord absolute ($BF88B514);
  C1FIFOCON7SET : longWord absolute ($BF88B518);
  C1FIFOCON7INV : longWord absolute ($BF88B51C);
  C1FIFOINT7 : longWord absolute ($BF88B520);
type
  TC1FIFOINT7bits = bitpacked record
  case integer of
  0 : (
    RXNEMPTYIF : 0..1;
    RXHALFIF : 0..1;
    RXFULLIF : 0..1;
    RXOVFLIF : 0..1;
    RESERVED0 : 0..15;
    TXEMPTYIF : 0..1;
    TXHALFIF : 0..1;
    TXNFULLIF : 0..1;
    RESERVED1 : 0..31;
    RXNEMPTYIE : 0..1;
    RXHALFIE : 0..1;
    RXFULLIE : 0..1;
    RXOVFLIE : 0..1;
    RESERVED2 : 0..15;
    TXEMPTYIE : 0..1;
    TXHALFIE : 0..1;
    TXNFULLIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOINT7bits: TC1FIFOINT7bits absolute ($BF88B520);
  C1FIFOINT7CLR : longWord absolute ($BF88B524);
  C1FIFOINT7SET : longWord absolute ($BF88B528);
  C1FIFOINT7INV : longWord absolute ($BF88B52C);
  C1FIFOUA7 : longWord absolute ($BF88B530);
  C1FIFOUA7CLR : longWord absolute ($BF88B534);
  C1FIFOUA7SET : longWord absolute ($BF88B538);
  C1FIFOUA7INV : longWord absolute ($BF88B53C);
  C1FIFOCI7 : longWord absolute ($BF88B540);
type
  TC1FIFOCI7bits = bitpacked record
  case integer of
  0 : (
    CFIFOCI : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOCI7bits: TC1FIFOCI7bits absolute ($BF88B540);
  C1FIFOCI7CLR : longWord absolute ($BF88B544);
  C1FIFOCI7SET : longWord absolute ($BF88B548);
  C1FIFOCI7INV : longWord absolute ($BF88B54C);
  C1FIFOCON8 : longWord absolute ($BF88B550);
type
  TC1FIFOCON8bits = bitpacked record
  case integer of
  0 : (
    TXPRI : 0..3;
    RTREN : 0..1;
    TXREQ : 0..1;
    TXERR : 0..1;
    TXLARB : 0..1;
    TXABAT : 0..1;
    TXEN : 0..1;
    RESERVED0 : 0..15;
    DONLY : 0..1;
    UINC : 0..1;
    FRESET : 0..1;
    RESERVED1 : 0..1;
    FSIZE : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOCON8bits: TC1FIFOCON8bits absolute ($BF88B550);
  C1FIFOCON8CLR : longWord absolute ($BF88B554);
  C1FIFOCON8SET : longWord absolute ($BF88B558);
  C1FIFOCON8INV : longWord absolute ($BF88B55C);
  C1FIFOINT8 : longWord absolute ($BF88B560);
type
  TC1FIFOINT8bits = bitpacked record
  case integer of
  0 : (
    RXNEMPTYIF : 0..1;
    RXHALFIF : 0..1;
    RXFULLIF : 0..1;
    RXOVFLIF : 0..1;
    RESERVED0 : 0..15;
    TXEMPTYIF : 0..1;
    TXHALFIF : 0..1;
    TXNFULLIF : 0..1;
    RESERVED1 : 0..31;
    RXNEMPTYIE : 0..1;
    RXHALFIE : 0..1;
    RXFULLIE : 0..1;
    RXOVFLIE : 0..1;
    RESERVED2 : 0..15;
    TXEMPTYIE : 0..1;
    TXHALFIE : 0..1;
    TXNFULLIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOINT8bits: TC1FIFOINT8bits absolute ($BF88B560);
  C1FIFOINT8CLR : longWord absolute ($BF88B564);
  C1FIFOINT8SET : longWord absolute ($BF88B568);
  C1FIFOINT8INV : longWord absolute ($BF88B56C);
  C1FIFOUA8 : longWord absolute ($BF88B570);
  C1FIFOUA8CLR : longWord absolute ($BF88B574);
  C1FIFOUA8SET : longWord absolute ($BF88B578);
  C1FIFOUA8INV : longWord absolute ($BF88B57C);
  C1FIFOCI8 : longWord absolute ($BF88B580);
type
  TC1FIFOCI8bits = bitpacked record
  case integer of
  0 : (
    CFIFOCI : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOCI8bits: TC1FIFOCI8bits absolute ($BF88B580);
  C1FIFOCI8CLR : longWord absolute ($BF88B584);
  C1FIFOCI8SET : longWord absolute ($BF88B588);
  C1FIFOCI8INV : longWord absolute ($BF88B58C);
  C1FIFOCON9 : longWord absolute ($BF88B590);
type
  TC1FIFOCON9bits = bitpacked record
  case integer of
  0 : (
    TXPRI : 0..3;
    RTREN : 0..1;
    TXREQ : 0..1;
    TXERR : 0..1;
    TXLARB : 0..1;
    TXABAT : 0..1;
    TXEN : 0..1;
    RESERVED0 : 0..15;
    DONLY : 0..1;
    UINC : 0..1;
    FRESET : 0..1;
    RESERVED1 : 0..1;
    FSIZE : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOCON9bits: TC1FIFOCON9bits absolute ($BF88B590);
  C1FIFOCON9CLR : longWord absolute ($BF88B594);
  C1FIFOCON9SET : longWord absolute ($BF88B598);
  C1FIFOCON9INV : longWord absolute ($BF88B59C);
  C1FIFOINT9 : longWord absolute ($BF88B5A0);
type
  TC1FIFOINT9bits = bitpacked record
  case integer of
  0 : (
    RXNEMPTYIF : 0..1;
    RXHALFIF : 0..1;
    RXFULLIF : 0..1;
    RXOVFLIF : 0..1;
    RESERVED0 : 0..15;
    TXEMPTYIF : 0..1;
    TXHALFIF : 0..1;
    TXNFULLIF : 0..1;
    RESERVED1 : 0..31;
    RXNEMPTYIE : 0..1;
    RXHALFIE : 0..1;
    RXFULLIE : 0..1;
    RXOVFLIE : 0..1;
    RESERVED2 : 0..15;
    TXEMPTYIE : 0..1;
    TXHALFIE : 0..1;
    TXNFULLIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOINT9bits: TC1FIFOINT9bits absolute ($BF88B5A0);
  C1FIFOINT9CLR : longWord absolute ($BF88B5A4);
  C1FIFOINT9SET : longWord absolute ($BF88B5A8);
  C1FIFOINT9INV : longWord absolute ($BF88B5AC);
  C1FIFOUA9 : longWord absolute ($BF88B5B0);
  C1FIFOUA9CLR : longWord absolute ($BF88B5B4);
  C1FIFOUA9SET : longWord absolute ($BF88B5B8);
  C1FIFOUA9INV : longWord absolute ($BF88B5BC);
  C1FIFOCI9 : longWord absolute ($BF88B5C0);
type
  TC1FIFOCI9bits = bitpacked record
  case integer of
  0 : (
    CFIFOCI : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOCI9bits: TC1FIFOCI9bits absolute ($BF88B5C0);
  C1FIFOCI9CLR : longWord absolute ($BF88B5C4);
  C1FIFOCI9SET : longWord absolute ($BF88B5C8);
  C1FIFOCI9INV : longWord absolute ($BF88B5CC);
  C1FIFOCON10 : longWord absolute ($BF88B5D0);
type
  TC1FIFOCON10bits = bitpacked record
  case integer of
  0 : (
    TXPRI : 0..3;
    RTREN : 0..1;
    TXREQ : 0..1;
    TXERR : 0..1;
    TXLARB : 0..1;
    TXABAT : 0..1;
    TXEN : 0..1;
    RESERVED0 : 0..15;
    DONLY : 0..1;
    UINC : 0..1;
    FRESET : 0..1;
    RESERVED1 : 0..1;
    FSIZE : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOCON10bits: TC1FIFOCON10bits absolute ($BF88B5D0);
  C1FIFOCON10CLR : longWord absolute ($BF88B5D4);
  C1FIFOCON10SET : longWord absolute ($BF88B5D8);
  C1FIFOCON10INV : longWord absolute ($BF88B5DC);
  C1FIFOINT10 : longWord absolute ($BF88B5E0);
type
  TC1FIFOINT10bits = bitpacked record
  case integer of
  0 : (
    RXNEMPTYIF : 0..1;
    RXHALFIF : 0..1;
    RXFULLIF : 0..1;
    RXOVFLIF : 0..1;
    RESERVED0 : 0..15;
    TXEMPTYIF : 0..1;
    TXHALFIF : 0..1;
    TXNFULLIF : 0..1;
    RESERVED1 : 0..31;
    RXNEMPTYIE : 0..1;
    RXHALFIE : 0..1;
    RXFULLIE : 0..1;
    RXOVFLIE : 0..1;
    RESERVED2 : 0..15;
    TXEMPTYIE : 0..1;
    TXHALFIE : 0..1;
    TXNFULLIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOINT10bits: TC1FIFOINT10bits absolute ($BF88B5E0);
  C1FIFOINT10CLR : longWord absolute ($BF88B5E4);
  C1FIFOINT10SET : longWord absolute ($BF88B5E8);
  C1FIFOINT10INV : longWord absolute ($BF88B5EC);
  C1FIFOUA10 : longWord absolute ($BF88B5F0);
  C1FIFOUA10CLR : longWord absolute ($BF88B5F4);
  C1FIFOUA10SET : longWord absolute ($BF88B5F8);
  C1FIFOUA10INV : longWord absolute ($BF88B5FC);
  C1FIFOCI10 : longWord absolute ($BF88B600);
type
  TC1FIFOCI10bits = bitpacked record
  case integer of
  0 : (
    CFIFOCI : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOCI10bits: TC1FIFOCI10bits absolute ($BF88B600);
  C1FIFOCI10CLR : longWord absolute ($BF88B604);
  C1FIFOCI10SET : longWord absolute ($BF88B608);
  C1FIFOCI10INV : longWord absolute ($BF88B60C);
  C1FIFOCON11 : longWord absolute ($BF88B610);
type
  TC1FIFOCON11bits = bitpacked record
  case integer of
  0 : (
    TXPRI : 0..3;
    RTREN : 0..1;
    TXREQ : 0..1;
    TXERR : 0..1;
    TXLARB : 0..1;
    TXABAT : 0..1;
    TXEN : 0..1;
    RESERVED0 : 0..15;
    DONLY : 0..1;
    UINC : 0..1;
    FRESET : 0..1;
    RESERVED1 : 0..1;
    FSIZE : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOCON11bits: TC1FIFOCON11bits absolute ($BF88B610);
  C1FIFOCON11CLR : longWord absolute ($BF88B614);
  C1FIFOCON11SET : longWord absolute ($BF88B618);
  C1FIFOCON11INV : longWord absolute ($BF88B61C);
  C1FIFOINT11 : longWord absolute ($BF88B620);
type
  TC1FIFOINT11bits = bitpacked record
  case integer of
  0 : (
    RXNEMPTYIF : 0..1;
    RXHALFIF : 0..1;
    RXFULLIF : 0..1;
    RXOVFLIF : 0..1;
    RESERVED0 : 0..15;
    TXEMPTYIF : 0..1;
    TXHALFIF : 0..1;
    TXNFULLIF : 0..1;
    RESERVED1 : 0..31;
    RXNEMPTYIE : 0..1;
    RXHALFIE : 0..1;
    RXFULLIE : 0..1;
    RXOVFLIE : 0..1;
    RESERVED2 : 0..15;
    TXEMPTYIE : 0..1;
    TXHALFIE : 0..1;
    TXNFULLIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOINT11bits: TC1FIFOINT11bits absolute ($BF88B620);
  C1FIFOINT11CLR : longWord absolute ($BF88B624);
  C1FIFOINT11SET : longWord absolute ($BF88B628);
  C1FIFOINT11INV : longWord absolute ($BF88B62C);
  C1FIFOUA11 : longWord absolute ($BF88B630);
  C1FIFOUA11CLR : longWord absolute ($BF88B634);
  C1FIFOUA11SET : longWord absolute ($BF88B638);
  C1FIFOUA11INV : longWord absolute ($BF88B63C);
  C1FIFOCI11 : longWord absolute ($BF88B640);
type
  TC1FIFOCI11bits = bitpacked record
  case integer of
  0 : (
    CFIFOCI : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOCI11bits: TC1FIFOCI11bits absolute ($BF88B640);
  C1FIFOCI11CLR : longWord absolute ($BF88B644);
  C1FIFOCI11SET : longWord absolute ($BF88B648);
  C1FIFOCI11INV : longWord absolute ($BF88B64C);
  C1FIFOCON12 : longWord absolute ($BF88B650);
type
  TC1FIFOCON12bits = bitpacked record
  case integer of
  0 : (
    TXPRI : 0..3;
    RTREN : 0..1;
    TXREQ : 0..1;
    TXERR : 0..1;
    TXLARB : 0..1;
    TXABAT : 0..1;
    TXEN : 0..1;
    RESERVED0 : 0..15;
    DONLY : 0..1;
    UINC : 0..1;
    FRESET : 0..1;
    RESERVED1 : 0..1;
    FSIZE : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOCON12bits: TC1FIFOCON12bits absolute ($BF88B650);
  C1FIFOCON12CLR : longWord absolute ($BF88B654);
  C1FIFOCON12SET : longWord absolute ($BF88B658);
  C1FIFOCON12INV : longWord absolute ($BF88B65C);
  C1FIFOINT12 : longWord absolute ($BF88B660);
type
  TC1FIFOINT12bits = bitpacked record
  case integer of
  0 : (
    RXNEMPTYIF : 0..1;
    RXHALFIF : 0..1;
    RXFULLIF : 0..1;
    RXOVFLIF : 0..1;
    RESERVED0 : 0..15;
    TXEMPTYIF : 0..1;
    TXHALFIF : 0..1;
    TXNFULLIF : 0..1;
    RESERVED1 : 0..31;
    RXNEMPTYIE : 0..1;
    RXHALFIE : 0..1;
    RXFULLIE : 0..1;
    RXOVFLIE : 0..1;
    RESERVED2 : 0..15;
    TXEMPTYIE : 0..1;
    TXHALFIE : 0..1;
    TXNFULLIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOINT12bits: TC1FIFOINT12bits absolute ($BF88B660);
  C1FIFOINT12CLR : longWord absolute ($BF88B664);
  C1FIFOINT12SET : longWord absolute ($BF88B668);
  C1FIFOINT12INV : longWord absolute ($BF88B66C);
  C1FIFOUA12 : longWord absolute ($BF88B670);
  C1FIFOUA12CLR : longWord absolute ($BF88B674);
  C1FIFOUA12SET : longWord absolute ($BF88B678);
  C1FIFOUA12INV : longWord absolute ($BF88B67C);
  C1FIFOCI12 : longWord absolute ($BF88B680);
type
  TC1FIFOCI12bits = bitpacked record
  case integer of
  0 : (
    CFIFOCI : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOCI12bits: TC1FIFOCI12bits absolute ($BF88B680);
  C1FIFOCI12CLR : longWord absolute ($BF88B684);
  C1FIFOCI12SET : longWord absolute ($BF88B688);
  C1FIFOCI12INV : longWord absolute ($BF88B68C);
  C1FIFOCON13 : longWord absolute ($BF88B690);
type
  TC1FIFOCON13bits = bitpacked record
  case integer of
  0 : (
    TXPRI : 0..3;
    RTREN : 0..1;
    TXREQ : 0..1;
    TXERR : 0..1;
    TXLARB : 0..1;
    TXABAT : 0..1;
    TXEN : 0..1;
    RESERVED0 : 0..15;
    DONLY : 0..1;
    UINC : 0..1;
    FRESET : 0..1;
    RESERVED1 : 0..1;
    FSIZE : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOCON13bits: TC1FIFOCON13bits absolute ($BF88B690);
  C1FIFOCON13CLR : longWord absolute ($BF88B694);
  C1FIFOCON13SET : longWord absolute ($BF88B698);
  C1FIFOCON13INV : longWord absolute ($BF88B69C);
  C1FIFOINT13 : longWord absolute ($BF88B6A0);
type
  TC1FIFOINT13bits = bitpacked record
  case integer of
  0 : (
    RXNEMPTYIF : 0..1;
    RXHALFIF : 0..1;
    RXFULLIF : 0..1;
    RXOVFLIF : 0..1;
    RESERVED0 : 0..15;
    TXEMPTYIF : 0..1;
    TXHALFIF : 0..1;
    TXNFULLIF : 0..1;
    RESERVED1 : 0..31;
    RXNEMPTYIE : 0..1;
    RXHALFIE : 0..1;
    RXFULLIE : 0..1;
    RXOVFLIE : 0..1;
    RESERVED2 : 0..15;
    TXEMPTYIE : 0..1;
    TXHALFIE : 0..1;
    TXNFULLIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOINT13bits: TC1FIFOINT13bits absolute ($BF88B6A0);
  C1FIFOINT13CLR : longWord absolute ($BF88B6A4);
  C1FIFOINT13SET : longWord absolute ($BF88B6A8);
  C1FIFOINT13INV : longWord absolute ($BF88B6AC);
  C1FIFOUA13 : longWord absolute ($BF88B6B0);
  C1FIFOUA13CLR : longWord absolute ($BF88B6B4);
  C1FIFOUA13SET : longWord absolute ($BF88B6B8);
  C1FIFOUA13INV : longWord absolute ($BF88B6BC);
  C1FIFOCI13 : longWord absolute ($BF88B6C0);
type
  TC1FIFOCI13bits = bitpacked record
  case integer of
  0 : (
    CFIFOCI : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOCI13bits: TC1FIFOCI13bits absolute ($BF88B6C0);
  C1FIFOCI13CLR : longWord absolute ($BF88B6C4);
  C1FIFOCI13SET : longWord absolute ($BF88B6C8);
  C1FIFOCI13INV : longWord absolute ($BF88B6CC);
  C1FIFOCON14 : longWord absolute ($BF88B6D0);
type
  TC1FIFOCON14bits = bitpacked record
  case integer of
  0 : (
    TXPRI : 0..3;
    RTREN : 0..1;
    TXREQ : 0..1;
    TXERR : 0..1;
    TXLARB : 0..1;
    TXABAT : 0..1;
    TXEN : 0..1;
    RESERVED0 : 0..15;
    DONLY : 0..1;
    UINC : 0..1;
    FRESET : 0..1;
    RESERVED1 : 0..1;
    FSIZE : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOCON14bits: TC1FIFOCON14bits absolute ($BF88B6D0);
  C1FIFOCON14CLR : longWord absolute ($BF88B6D4);
  C1FIFOCON14SET : longWord absolute ($BF88B6D8);
  C1FIFOCON14INV : longWord absolute ($BF88B6DC);
  C1FIFOINT14 : longWord absolute ($BF88B6E0);
type
  TC1FIFOINT14bits = bitpacked record
  case integer of
  0 : (
    RXNEMPTYIF : 0..1;
    RXHALFIF : 0..1;
    RXFULLIF : 0..1;
    RXOVFLIF : 0..1;
    RESERVED0 : 0..15;
    TXEMPTYIF : 0..1;
    TXHALFIF : 0..1;
    TXNFULLIF : 0..1;
    RESERVED1 : 0..31;
    RXNEMPTYIE : 0..1;
    RXHALFIE : 0..1;
    RXFULLIE : 0..1;
    RXOVFLIE : 0..1;
    RESERVED2 : 0..15;
    TXEMPTYIE : 0..1;
    TXHALFIE : 0..1;
    TXNFULLIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOINT14bits: TC1FIFOINT14bits absolute ($BF88B6E0);
  C1FIFOINT14CLR : longWord absolute ($BF88B6E4);
  C1FIFOINT14SET : longWord absolute ($BF88B6E8);
  C1FIFOINT14INV : longWord absolute ($BF88B6EC);
  C1FIFOUA14 : longWord absolute ($BF88B6F0);
  C1FIFOUA14CLR : longWord absolute ($BF88B6F4);
  C1FIFOUA14SET : longWord absolute ($BF88B6F8);
  C1FIFOUA14INV : longWord absolute ($BF88B6FC);
  C1FIFOCI14 : longWord absolute ($BF88B700);
type
  TC1FIFOCI14bits = bitpacked record
  case integer of
  0 : (
    CFIFOCI : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOCI14bits: TC1FIFOCI14bits absolute ($BF88B700);
  C1FIFOCI14CLR : longWord absolute ($BF88B704);
  C1FIFOCI14SET : longWord absolute ($BF88B708);
  C1FIFOCI14INV : longWord absolute ($BF88B70C);
  C1FIFOCON15 : longWord absolute ($BF88B710);
type
  TC1FIFOCON15bits = bitpacked record
  case integer of
  0 : (
    TXPRI : 0..3;
    RTREN : 0..1;
    TXREQ : 0..1;
    TXERR : 0..1;
    TXLARB : 0..1;
    TXABAT : 0..1;
    TXEN : 0..1;
    RESERVED0 : 0..15;
    DONLY : 0..1;
    UINC : 0..1;
    FRESET : 0..1;
    RESERVED1 : 0..1;
    FSIZE : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOCON15bits: TC1FIFOCON15bits absolute ($BF88B710);
  C1FIFOCON15CLR : longWord absolute ($BF88B714);
  C1FIFOCON15SET : longWord absolute ($BF88B718);
  C1FIFOCON15INV : longWord absolute ($BF88B71C);
  C1FIFOINT15 : longWord absolute ($BF88B720);
type
  TC1FIFOINT15bits = bitpacked record
  case integer of
  0 : (
    RXNEMPTYIF : 0..1;
    RXHALFIF : 0..1;
    RXFULLIF : 0..1;
    RXOVFLIF : 0..1;
    RESERVED0 : 0..15;
    TXEMPTYIF : 0..1;
    TXHALFIF : 0..1;
    TXNFULLIF : 0..1;
    RESERVED1 : 0..31;
    RXNEMPTYIE : 0..1;
    RXHALFIE : 0..1;
    RXFULLIE : 0..1;
    RXOVFLIE : 0..1;
    RESERVED2 : 0..15;
    TXEMPTYIE : 0..1;
    TXHALFIE : 0..1;
    TXNFULLIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOINT15bits: TC1FIFOINT15bits absolute ($BF88B720);
  C1FIFOINT15CLR : longWord absolute ($BF88B724);
  C1FIFOINT15SET : longWord absolute ($BF88B728);
  C1FIFOINT15INV : longWord absolute ($BF88B72C);
  C1FIFOUA15 : longWord absolute ($BF88B730);
  C1FIFOUA15CLR : longWord absolute ($BF88B734);
  C1FIFOUA15SET : longWord absolute ($BF88B738);
  C1FIFOUA15INV : longWord absolute ($BF88B73C);
  C1FIFOCI15 : longWord absolute ($BF88B740);
type
  TC1FIFOCI15bits = bitpacked record
  case integer of
  0 : (
    CFIFOCI : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOCI15bits: TC1FIFOCI15bits absolute ($BF88B740);
  C1FIFOCI15CLR : longWord absolute ($BF88B744);
  C1FIFOCI15SET : longWord absolute ($BF88B748);
  C1FIFOCI15INV : longWord absolute ($BF88B74C);
  C1FIFOCON16 : longWord absolute ($BF88B750);
type
  TC1FIFOCON16bits = bitpacked record
  case integer of
  0 : (
    TXPRI : 0..3;
    RTREN : 0..1;
    TXREQ : 0..1;
    TXERR : 0..1;
    TXLARB : 0..1;
    TXABAT : 0..1;
    TXEN : 0..1;
    RESERVED0 : 0..15;
    DONLY : 0..1;
    UINC : 0..1;
    FRESET : 0..1;
    RESERVED1 : 0..1;
    FSIZE : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOCON16bits: TC1FIFOCON16bits absolute ($BF88B750);
  C1FIFOCON16CLR : longWord absolute ($BF88B754);
  C1FIFOCON16SET : longWord absolute ($BF88B758);
  C1FIFOCON16INV : longWord absolute ($BF88B75C);
  C1FIFOINT16 : longWord absolute ($BF88B760);
type
  TC1FIFOINT16bits = bitpacked record
  case integer of
  0 : (
    RXNEMPTYIF : 0..1;
    RXHALFIF : 0..1;
    RXFULLIF : 0..1;
    RXOVFLIF : 0..1;
    RESERVED0 : 0..15;
    TXEMPTYIF : 0..1;
    TXHALFIF : 0..1;
    TXNFULLIF : 0..1;
    RESERVED1 : 0..31;
    RXNEMPTYIE : 0..1;
    RXHALFIE : 0..1;
    RXFULLIE : 0..1;
    RXOVFLIE : 0..1;
    RESERVED2 : 0..15;
    TXEMPTYIE : 0..1;
    TXHALFIE : 0..1;
    TXNFULLIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOINT16bits: TC1FIFOINT16bits absolute ($BF88B760);
  C1FIFOINT16CLR : longWord absolute ($BF88B764);
  C1FIFOINT16SET : longWord absolute ($BF88B768);
  C1FIFOINT16INV : longWord absolute ($BF88B76C);
  C1FIFOUA16 : longWord absolute ($BF88B770);
  C1FIFOUA16CLR : longWord absolute ($BF88B774);
  C1FIFOUA16SET : longWord absolute ($BF88B778);
  C1FIFOUA16INV : longWord absolute ($BF88B77C);
  C1FIFOCI16 : longWord absolute ($BF88B780);
type
  TC1FIFOCI16bits = bitpacked record
  case integer of
  0 : (
    CFIFOCI : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOCI16bits: TC1FIFOCI16bits absolute ($BF88B780);
  C1FIFOCI16CLR : longWord absolute ($BF88B784);
  C1FIFOCI16SET : longWord absolute ($BF88B788);
  C1FIFOCI16INV : longWord absolute ($BF88B78C);
  C1FIFOCON17 : longWord absolute ($BF88B790);
type
  TC1FIFOCON17bits = bitpacked record
  case integer of
  0 : (
    TXPRI : 0..3;
    RTREN : 0..1;
    TXREQ : 0..1;
    TXERR : 0..1;
    TXLARB : 0..1;
    TXABAT : 0..1;
    TXEN : 0..1;
    RESERVED0 : 0..15;
    DONLY : 0..1;
    UINC : 0..1;
    FRESET : 0..1;
    RESERVED1 : 0..1;
    FSIZE : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOCON17bits: TC1FIFOCON17bits absolute ($BF88B790);
  C1FIFOCON17CLR : longWord absolute ($BF88B794);
  C1FIFOCON17SET : longWord absolute ($BF88B798);
  C1FIFOCON17INV : longWord absolute ($BF88B79C);
  C1FIFOINT17 : longWord absolute ($BF88B7A0);
type
  TC1FIFOINT17bits = bitpacked record
  case integer of
  0 : (
    RXNEMPTYIF : 0..1;
    RXHALFIF : 0..1;
    RXFULLIF : 0..1;
    RXOVFLIF : 0..1;
    RESERVED0 : 0..15;
    TXEMPTYIF : 0..1;
    TXHALFIF : 0..1;
    TXNFULLIF : 0..1;
    RESERVED1 : 0..31;
    RXNEMPTYIE : 0..1;
    RXHALFIE : 0..1;
    RXFULLIE : 0..1;
    RXOVFLIE : 0..1;
    RESERVED2 : 0..15;
    TXEMPTYIE : 0..1;
    TXHALFIE : 0..1;
    TXNFULLIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOINT17bits: TC1FIFOINT17bits absolute ($BF88B7A0);
  C1FIFOINT17CLR : longWord absolute ($BF88B7A4);
  C1FIFOINT17SET : longWord absolute ($BF88B7A8);
  C1FIFOINT17INV : longWord absolute ($BF88B7AC);
  C1FIFOUA17 : longWord absolute ($BF88B7B0);
  C1FIFOUA17CLR : longWord absolute ($BF88B7B4);
  C1FIFOUA17SET : longWord absolute ($BF88B7B8);
  C1FIFOUA17INV : longWord absolute ($BF88B7BC);
  C1FIFOCI17 : longWord absolute ($BF88B7C0);
type
  TC1FIFOCI17bits = bitpacked record
  case integer of
  0 : (
    CFIFOCI : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOCI17bits: TC1FIFOCI17bits absolute ($BF88B7C0);
  C1FIFOCI17CLR : longWord absolute ($BF88B7C4);
  C1FIFOCI17SET : longWord absolute ($BF88B7C8);
  C1FIFOCI17INV : longWord absolute ($BF88B7CC);
  C1FIFOCON18 : longWord absolute ($BF88B7D0);
type
  TC1FIFOCON18bits = bitpacked record
  case integer of
  0 : (
    TXPRI : 0..3;
    RTREN : 0..1;
    TXREQ : 0..1;
    TXERR : 0..1;
    TXLARB : 0..1;
    TXABAT : 0..1;
    TXEN : 0..1;
    RESERVED0 : 0..15;
    DONLY : 0..1;
    UINC : 0..1;
    FRESET : 0..1;
    RESERVED1 : 0..1;
    FSIZE : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOCON18bits: TC1FIFOCON18bits absolute ($BF88B7D0);
  C1FIFOCON18CLR : longWord absolute ($BF88B7D4);
  C1FIFOCON18SET : longWord absolute ($BF88B7D8);
  C1FIFOCON18INV : longWord absolute ($BF88B7DC);
  C1FIFOINT18 : longWord absolute ($BF88B7E0);
type
  TC1FIFOINT18bits = bitpacked record
  case integer of
  0 : (
    RXNEMPTYIF : 0..1;
    RXHALFIF : 0..1;
    RXFULLIF : 0..1;
    RXOVFLIF : 0..1;
    RESERVED0 : 0..15;
    TXEMPTYIF : 0..1;
    TXHALFIF : 0..1;
    TXNFULLIF : 0..1;
    RESERVED1 : 0..31;
    RXNEMPTYIE : 0..1;
    RXHALFIE : 0..1;
    RXFULLIE : 0..1;
    RXOVFLIE : 0..1;
    RESERVED2 : 0..15;
    TXEMPTYIE : 0..1;
    TXHALFIE : 0..1;
    TXNFULLIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOINT18bits: TC1FIFOINT18bits absolute ($BF88B7E0);
  C1FIFOINT18CLR : longWord absolute ($BF88B7E4);
  C1FIFOINT18SET : longWord absolute ($BF88B7E8);
  C1FIFOINT18INV : longWord absolute ($BF88B7EC);
  C1FIFOUA18 : longWord absolute ($BF88B7F0);
  C1FIFOUA18CLR : longWord absolute ($BF88B7F4);
  C1FIFOUA18SET : longWord absolute ($BF88B7F8);
  C1FIFOUA18INV : longWord absolute ($BF88B7FC);
  C1FIFOCI18 : longWord absolute ($BF88B800);
type
  TC1FIFOCI18bits = bitpacked record
  case integer of
  0 : (
    CFIFOCI : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOCI18bits: TC1FIFOCI18bits absolute ($BF88B800);
  C1FIFOCI18CLR : longWord absolute ($BF88B804);
  C1FIFOCI18SET : longWord absolute ($BF88B808);
  C1FIFOCI18INV : longWord absolute ($BF88B80C);
  C1FIFOCON19 : longWord absolute ($BF88B810);
type
  TC1FIFOCON19bits = bitpacked record
  case integer of
  0 : (
    TXPRI : 0..3;
    RTREN : 0..1;
    TXREQ : 0..1;
    TXERR : 0..1;
    TXLARB : 0..1;
    TXABAT : 0..1;
    TXEN : 0..1;
    RESERVED0 : 0..15;
    DONLY : 0..1;
    UINC : 0..1;
    FRESET : 0..1;
    RESERVED1 : 0..1;
    FSIZE : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOCON19bits: TC1FIFOCON19bits absolute ($BF88B810);
  C1FIFOCON19CLR : longWord absolute ($BF88B814);
  C1FIFOCON19SET : longWord absolute ($BF88B818);
  C1FIFOCON19INV : longWord absolute ($BF88B81C);
  C1FIFOINT19 : longWord absolute ($BF88B820);
type
  TC1FIFOINT19bits = bitpacked record
  case integer of
  0 : (
    RXNEMPTYIF : 0..1;
    RXHALFIF : 0..1;
    RXFULLIF : 0..1;
    RXOVFLIF : 0..1;
    RESERVED0 : 0..15;
    TXEMPTYIF : 0..1;
    TXHALFIF : 0..1;
    TXNFULLIF : 0..1;
    RESERVED1 : 0..31;
    RXNEMPTYIE : 0..1;
    RXHALFIE : 0..1;
    RXFULLIE : 0..1;
    RXOVFLIE : 0..1;
    RESERVED2 : 0..15;
    TXEMPTYIE : 0..1;
    TXHALFIE : 0..1;
    TXNFULLIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOINT19bits: TC1FIFOINT19bits absolute ($BF88B820);
  C1FIFOINT19CLR : longWord absolute ($BF88B824);
  C1FIFOINT19SET : longWord absolute ($BF88B828);
  C1FIFOINT19INV : longWord absolute ($BF88B82C);
  C1FIFOUA19 : longWord absolute ($BF88B830);
  C1FIFOUA19CLR : longWord absolute ($BF88B834);
  C1FIFOUA19SET : longWord absolute ($BF88B838);
  C1FIFOUA19INV : longWord absolute ($BF88B83C);
  C1FIFOCI19 : longWord absolute ($BF88B840);
type
  TC1FIFOCI19bits = bitpacked record
  case integer of
  0 : (
    CFIFOCI : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOCI19bits: TC1FIFOCI19bits absolute ($BF88B840);
  C1FIFOCI19CLR : longWord absolute ($BF88B844);
  C1FIFOCI19SET : longWord absolute ($BF88B848);
  C1FIFOCI19INV : longWord absolute ($BF88B84C);
  C1FIFOCON20 : longWord absolute ($BF88B850);
type
  TC1FIFOCON20bits = bitpacked record
  case integer of
  0 : (
    TXPRI : 0..3;
    RTREN : 0..1;
    TXREQ : 0..1;
    TXERR : 0..1;
    TXLARB : 0..1;
    TXABAT : 0..1;
    TXEN : 0..1;
    RESERVED0 : 0..15;
    DONLY : 0..1;
    UINC : 0..1;
    FRESET : 0..1;
    RESERVED1 : 0..1;
    FSIZE : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOCON20bits: TC1FIFOCON20bits absolute ($BF88B850);
  C1FIFOCON20CLR : longWord absolute ($BF88B854);
  C1FIFOCON20SET : longWord absolute ($BF88B858);
  C1FIFOCON20INV : longWord absolute ($BF88B85C);
  C1FIFOINT20 : longWord absolute ($BF88B860);
type
  TC1FIFOINT20bits = bitpacked record
  case integer of
  0 : (
    RXNEMPTYIF : 0..1;
    RXHALFIF : 0..1;
    RXFULLIF : 0..1;
    RXOVFLIF : 0..1;
    RESERVED0 : 0..15;
    TXEMPTYIF : 0..1;
    TXHALFIF : 0..1;
    TXNFULLIF : 0..1;
    RESERVED1 : 0..31;
    RXNEMPTYIE : 0..1;
    RXHALFIE : 0..1;
    RXFULLIE : 0..1;
    RXOVFLIE : 0..1;
    RESERVED2 : 0..15;
    TXEMPTYIE : 0..1;
    TXHALFIE : 0..1;
    TXNFULLIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOINT20bits: TC1FIFOINT20bits absolute ($BF88B860);
  C1FIFOINT20CLR : longWord absolute ($BF88B864);
  C1FIFOINT20SET : longWord absolute ($BF88B868);
  C1FIFOINT20INV : longWord absolute ($BF88B86C);
  C1FIFOUA20 : longWord absolute ($BF88B870);
  C1FIFOUA20CLR : longWord absolute ($BF88B874);
  C1FIFOUA20SET : longWord absolute ($BF88B878);
  C1FIFOUA20INV : longWord absolute ($BF88B87C);
  C1FIFOCI20 : longWord absolute ($BF88B880);
type
  TC1FIFOCI20bits = bitpacked record
  case integer of
  0 : (
    CFIFOCI : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOCI20bits: TC1FIFOCI20bits absolute ($BF88B880);
  C1FIFOCI20CLR : longWord absolute ($BF88B884);
  C1FIFOCI20SET : longWord absolute ($BF88B888);
  C1FIFOCI20INV : longWord absolute ($BF88B88C);
  C1FIFOCON21 : longWord absolute ($BF88B890);
type
  TC1FIFOCON21bits = bitpacked record
  case integer of
  0 : (
    TXPRI : 0..3;
    RTREN : 0..1;
    TXREQ : 0..1;
    TXERR : 0..1;
    TXLARB : 0..1;
    TXABAT : 0..1;
    TXEN : 0..1;
    RESERVED0 : 0..15;
    DONLY : 0..1;
    UINC : 0..1;
    FRESET : 0..1;
    RESERVED1 : 0..1;
    FSIZE : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOCON21bits: TC1FIFOCON21bits absolute ($BF88B890);
  C1FIFOCON21CLR : longWord absolute ($BF88B894);
  C1FIFOCON21SET : longWord absolute ($BF88B898);
  C1FIFOCON21INV : longWord absolute ($BF88B89C);
  C1FIFOINT21 : longWord absolute ($BF88B8A0);
type
  TC1FIFOINT21bits = bitpacked record
  case integer of
  0 : (
    RXNEMPTYIF : 0..1;
    RXHALFIF : 0..1;
    RXFULLIF : 0..1;
    RXOVFLIF : 0..1;
    RESERVED0 : 0..15;
    TXEMPTYIF : 0..1;
    TXHALFIF : 0..1;
    TXNFULLIF : 0..1;
    RESERVED1 : 0..31;
    RXNEMPTYIE : 0..1;
    RXHALFIE : 0..1;
    RXFULLIE : 0..1;
    RXOVFLIE : 0..1;
    RESERVED2 : 0..15;
    TXEMPTYIE : 0..1;
    TXHALFIE : 0..1;
    TXNFULLIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOINT21bits: TC1FIFOINT21bits absolute ($BF88B8A0);
  C1FIFOINT21CLR : longWord absolute ($BF88B8A4);
  C1FIFOINT21SET : longWord absolute ($BF88B8A8);
  C1FIFOINT21INV : longWord absolute ($BF88B8AC);
  C1FIFOUA21 : longWord absolute ($BF88B8B0);
  C1FIFOUA21CLR : longWord absolute ($BF88B8B4);
  C1FIFOUA21SET : longWord absolute ($BF88B8B8);
  C1FIFOUA21INV : longWord absolute ($BF88B8BC);
  C1FIFOCI21 : longWord absolute ($BF88B8C0);
type
  TC1FIFOCI21bits = bitpacked record
  case integer of
  0 : (
    CFIFOCI : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOCI21bits: TC1FIFOCI21bits absolute ($BF88B8C0);
  C1FIFOCI21CLR : longWord absolute ($BF88B8C4);
  C1FIFOCI21SET : longWord absolute ($BF88B8C8);
  C1FIFOCI21INV : longWord absolute ($BF88B8CC);
  C1FIFOCON22 : longWord absolute ($BF88B8D0);
type
  TC1FIFOCON22bits = bitpacked record
  case integer of
  0 : (
    TXPRI : 0..3;
    RTREN : 0..1;
    TXREQ : 0..1;
    TXERR : 0..1;
    TXLARB : 0..1;
    TXABAT : 0..1;
    TXEN : 0..1;
    RESERVED0 : 0..15;
    DONLY : 0..1;
    UINC : 0..1;
    FRESET : 0..1;
    RESERVED1 : 0..1;
    FSIZE : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOCON22bits: TC1FIFOCON22bits absolute ($BF88B8D0);
  C1FIFOCON22CLR : longWord absolute ($BF88B8D4);
  C1FIFOCON22SET : longWord absolute ($BF88B8D8);
  C1FIFOCON22INV : longWord absolute ($BF88B8DC);
  C1FIFOINT22 : longWord absolute ($BF88B8E0);
type
  TC1FIFOINT22bits = bitpacked record
  case integer of
  0 : (
    RXNEMPTYIF : 0..1;
    RXHALFIF : 0..1;
    RXFULLIF : 0..1;
    RXOVFLIF : 0..1;
    RESERVED0 : 0..15;
    TXEMPTYIF : 0..1;
    TXHALFIF : 0..1;
    TXNFULLIF : 0..1;
    RESERVED1 : 0..31;
    RXNEMPTYIE : 0..1;
    RXHALFIE : 0..1;
    RXFULLIE : 0..1;
    RXOVFLIE : 0..1;
    RESERVED2 : 0..15;
    TXEMPTYIE : 0..1;
    TXHALFIE : 0..1;
    TXNFULLIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOINT22bits: TC1FIFOINT22bits absolute ($BF88B8E0);
  C1FIFOINT22CLR : longWord absolute ($BF88B8E4);
  C1FIFOINT22SET : longWord absolute ($BF88B8E8);
  C1FIFOINT22INV : longWord absolute ($BF88B8EC);
  C1FIFOUA22 : longWord absolute ($BF88B8F0);
  C1FIFOUA22CLR : longWord absolute ($BF88B8F4);
  C1FIFOUA22SET : longWord absolute ($BF88B8F8);
  C1FIFOUA22INV : longWord absolute ($BF88B8FC);
  C1FIFOCI22 : longWord absolute ($BF88B900);
type
  TC1FIFOCI22bits = bitpacked record
  case integer of
  0 : (
    CFIFOCI : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOCI22bits: TC1FIFOCI22bits absolute ($BF88B900);
  C1FIFOCI22CLR : longWord absolute ($BF88B904);
  C1FIFOCI22SET : longWord absolute ($BF88B908);
  C1FIFOCI22INV : longWord absolute ($BF88B90C);
  C1FIFOCON23 : longWord absolute ($BF88B910);
type
  TC1FIFOCON23bits = bitpacked record
  case integer of
  0 : (
    TXPRI : 0..3;
    RTREN : 0..1;
    TXREQ : 0..1;
    TXERR : 0..1;
    TXLARB : 0..1;
    TXABAT : 0..1;
    TXEN : 0..1;
    RESERVED0 : 0..15;
    DONLY : 0..1;
    UINC : 0..1;
    FRESET : 0..1;
    RESERVED1 : 0..1;
    FSIZE : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOCON23bits: TC1FIFOCON23bits absolute ($BF88B910);
  C1FIFOCON23CLR : longWord absolute ($BF88B914);
  C1FIFOCON23SET : longWord absolute ($BF88B918);
  C1FIFOCON23INV : longWord absolute ($BF88B91C);
  C1FIFOINT23 : longWord absolute ($BF88B920);
type
  TC1FIFOINT23bits = bitpacked record
  case integer of
  0 : (
    RXNEMPTYIF : 0..1;
    RXHALFIF : 0..1;
    RXFULLIF : 0..1;
    RXOVFLIF : 0..1;
    RESERVED0 : 0..15;
    TXEMPTYIF : 0..1;
    TXHALFIF : 0..1;
    TXNFULLIF : 0..1;
    RESERVED1 : 0..31;
    RXNEMPTYIE : 0..1;
    RXHALFIE : 0..1;
    RXFULLIE : 0..1;
    RXOVFLIE : 0..1;
    RESERVED2 : 0..15;
    TXEMPTYIE : 0..1;
    TXHALFIE : 0..1;
    TXNFULLIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOINT23bits: TC1FIFOINT23bits absolute ($BF88B920);
  C1FIFOINT23CLR : longWord absolute ($BF88B924);
  C1FIFOINT23SET : longWord absolute ($BF88B928);
  C1FIFOINT23INV : longWord absolute ($BF88B92C);
  C1FIFOUA23 : longWord absolute ($BF88B930);
  C1FIFOUA23CLR : longWord absolute ($BF88B934);
  C1FIFOUA23SET : longWord absolute ($BF88B938);
  C1FIFOUA23INV : longWord absolute ($BF88B93C);
  C1FIFOCI23 : longWord absolute ($BF88B940);
type
  TC1FIFOCI23bits = bitpacked record
  case integer of
  0 : (
    CFIFOCI : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOCI23bits: TC1FIFOCI23bits absolute ($BF88B940);
  C1FIFOCI23CLR : longWord absolute ($BF88B944);
  C1FIFOCI23SET : longWord absolute ($BF88B948);
  C1FIFOCI23INV : longWord absolute ($BF88B94C);
  C1FIFOCON24 : longWord absolute ($BF88B950);
type
  TC1FIFOCON24bits = bitpacked record
  case integer of
  0 : (
    TXPRI : 0..3;
    RTREN : 0..1;
    TXREQ : 0..1;
    TXERR : 0..1;
    TXLARB : 0..1;
    TXABAT : 0..1;
    TXEN : 0..1;
    RESERVED0 : 0..15;
    DONLY : 0..1;
    UINC : 0..1;
    FRESET : 0..1;
    RESERVED1 : 0..1;
    FSIZE : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOCON24bits: TC1FIFOCON24bits absolute ($BF88B950);
  C1FIFOCON24CLR : longWord absolute ($BF88B954);
  C1FIFOCON24SET : longWord absolute ($BF88B958);
  C1FIFOCON24INV : longWord absolute ($BF88B95C);
  C1FIFOINT24 : longWord absolute ($BF88B960);
type
  TC1FIFOINT24bits = bitpacked record
  case integer of
  0 : (
    RXNEMPTYIF : 0..1;
    RXHALFIF : 0..1;
    RXFULLIF : 0..1;
    RXOVFLIF : 0..1;
    RESERVED0 : 0..15;
    TXEMPTYIF : 0..1;
    TXHALFIF : 0..1;
    TXNFULLIF : 0..1;
    RESERVED1 : 0..31;
    RXNEMPTYIE : 0..1;
    RXHALFIE : 0..1;
    RXFULLIE : 0..1;
    RXOVFLIE : 0..1;
    RESERVED2 : 0..15;
    TXEMPTYIE : 0..1;
    TXHALFIE : 0..1;
    TXNFULLIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOINT24bits: TC1FIFOINT24bits absolute ($BF88B960);
  C1FIFOINT24CLR : longWord absolute ($BF88B964);
  C1FIFOINT24SET : longWord absolute ($BF88B968);
  C1FIFOINT24INV : longWord absolute ($BF88B96C);
  C1FIFOUA24 : longWord absolute ($BF88B970);
  C1FIFOUA24CLR : longWord absolute ($BF88B974);
  C1FIFOUA24SET : longWord absolute ($BF88B978);
  C1FIFOUA24INV : longWord absolute ($BF88B97C);
  C1FIFOCI24 : longWord absolute ($BF88B980);
type
  TC1FIFOCI24bits = bitpacked record
  case integer of
  0 : (
    CFIFOCI : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOCI24bits: TC1FIFOCI24bits absolute ($BF88B980);
  C1FIFOCI24CLR : longWord absolute ($BF88B984);
  C1FIFOCI24SET : longWord absolute ($BF88B988);
  C1FIFOCI24INV : longWord absolute ($BF88B98C);
  C1FIFOCON25 : longWord absolute ($BF88B990);
type
  TC1FIFOCON25bits = bitpacked record
  case integer of
  0 : (
    TXPRI : 0..3;
    RTREN : 0..1;
    TXREQ : 0..1;
    TXERR : 0..1;
    TXLARB : 0..1;
    TXABAT : 0..1;
    TXEN : 0..1;
    RESERVED0 : 0..15;
    DONLY : 0..1;
    UINC : 0..1;
    FRESET : 0..1;
    RESERVED1 : 0..1;
    FSIZE : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOCON25bits: TC1FIFOCON25bits absolute ($BF88B990);
  C1FIFOCON25CLR : longWord absolute ($BF88B994);
  C1FIFOCON25SET : longWord absolute ($BF88B998);
  C1FIFOCON25INV : longWord absolute ($BF88B99C);
  C1FIFOINT25 : longWord absolute ($BF88B9A0);
type
  TC1FIFOINT25bits = bitpacked record
  case integer of
  0 : (
    RXNEMPTYIF : 0..1;
    RXHALFIF : 0..1;
    RXFULLIF : 0..1;
    RXOVFLIF : 0..1;
    RESERVED0 : 0..15;
    TXEMPTYIF : 0..1;
    TXHALFIF : 0..1;
    TXNFULLIF : 0..1;
    RESERVED1 : 0..31;
    RXNEMPTYIE : 0..1;
    RXHALFIE : 0..1;
    RXFULLIE : 0..1;
    RXOVFLIE : 0..1;
    RESERVED2 : 0..15;
    TXEMPTYIE : 0..1;
    TXHALFIE : 0..1;
    TXNFULLIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOINT25bits: TC1FIFOINT25bits absolute ($BF88B9A0);
  C1FIFOINT25CLR : longWord absolute ($BF88B9A4);
  C1FIFOINT25SET : longWord absolute ($BF88B9A8);
  C1FIFOINT25INV : longWord absolute ($BF88B9AC);
  C1FIFOUA25 : longWord absolute ($BF88B9B0);
  C1FIFOUA25CLR : longWord absolute ($BF88B9B4);
  C1FIFOUA25SET : longWord absolute ($BF88B9B8);
  C1FIFOUA25INV : longWord absolute ($BF88B9BC);
  C1FIFOCI25 : longWord absolute ($BF88B9C0);
type
  TC1FIFOCI25bits = bitpacked record
  case integer of
  0 : (
    CFIFOCI : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOCI25bits: TC1FIFOCI25bits absolute ($BF88B9C0);
  C1FIFOCI25CLR : longWord absolute ($BF88B9C4);
  C1FIFOCI25SET : longWord absolute ($BF88B9C8);
  C1FIFOCI25INV : longWord absolute ($BF88B9CC);
  C1FIFOCON26 : longWord absolute ($BF88B9D0);
type
  TC1FIFOCON26bits = bitpacked record
  case integer of
  0 : (
    TXPRI : 0..3;
    RTREN : 0..1;
    TXREQ : 0..1;
    TXERR : 0..1;
    TXLARB : 0..1;
    TXABAT : 0..1;
    TXEN : 0..1;
    RESERVED0 : 0..15;
    DONLY : 0..1;
    UINC : 0..1;
    FRESET : 0..1;
    RESERVED1 : 0..1;
    FSIZE : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOCON26bits: TC1FIFOCON26bits absolute ($BF88B9D0);
  C1FIFOCON26CLR : longWord absolute ($BF88B9D4);
  C1FIFOCON26SET : longWord absolute ($BF88B9D8);
  C1FIFOCON26INV : longWord absolute ($BF88B9DC);
  C1FIFOINT26 : longWord absolute ($BF88B9E0);
type
  TC1FIFOINT26bits = bitpacked record
  case integer of
  0 : (
    RXNEMPTYIF : 0..1;
    RXHALFIF : 0..1;
    RXFULLIF : 0..1;
    RXOVFLIF : 0..1;
    RESERVED0 : 0..15;
    TXEMPTYIF : 0..1;
    TXHALFIF : 0..1;
    TXNFULLIF : 0..1;
    RESERVED1 : 0..31;
    RXNEMPTYIE : 0..1;
    RXHALFIE : 0..1;
    RXFULLIE : 0..1;
    RXOVFLIE : 0..1;
    RESERVED2 : 0..15;
    TXEMPTYIE : 0..1;
    TXHALFIE : 0..1;
    TXNFULLIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOINT26bits: TC1FIFOINT26bits absolute ($BF88B9E0);
  C1FIFOINT26CLR : longWord absolute ($BF88B9E4);
  C1FIFOINT26SET : longWord absolute ($BF88B9E8);
  C1FIFOINT26INV : longWord absolute ($BF88B9EC);
  C1FIFOUA26 : longWord absolute ($BF88B9F0);
  C1FIFOUA26CLR : longWord absolute ($BF88B9F4);
  C1FIFOUA26SET : longWord absolute ($BF88B9F8);
  C1FIFOUA26INV : longWord absolute ($BF88B9FC);
  C1FIFOCI26 : longWord absolute ($BF88BA00);
type
  TC1FIFOCI26bits = bitpacked record
  case integer of
  0 : (
    CFIFOCI : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOCI26bits: TC1FIFOCI26bits absolute ($BF88BA00);
  C1FIFOCI26CLR : longWord absolute ($BF88BA04);
  C1FIFOCI26SET : longWord absolute ($BF88BA08);
  C1FIFOCI26INV : longWord absolute ($BF88BA0C);
  C1FIFOCON27 : longWord absolute ($BF88BA10);
type
  TC1FIFOCON27bits = bitpacked record
  case integer of
  0 : (
    TXPRI : 0..3;
    RTREN : 0..1;
    TXREQ : 0..1;
    TXERR : 0..1;
    TXLARB : 0..1;
    TXABAT : 0..1;
    TXEN : 0..1;
    RESERVED0 : 0..15;
    DONLY : 0..1;
    UINC : 0..1;
    FRESET : 0..1;
    RESERVED1 : 0..1;
    FSIZE : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOCON27bits: TC1FIFOCON27bits absolute ($BF88BA10);
  C1FIFOCON27CLR : longWord absolute ($BF88BA14);
  C1FIFOCON27SET : longWord absolute ($BF88BA18);
  C1FIFOCON27INV : longWord absolute ($BF88BA1C);
  C1FIFOINT27 : longWord absolute ($BF88BA20);
type
  TC1FIFOINT27bits = bitpacked record
  case integer of
  0 : (
    RXNEMPTYIF : 0..1;
    RXHALFIF : 0..1;
    RXFULLIF : 0..1;
    RXOVFLIF : 0..1;
    RESERVED0 : 0..15;
    TXEMPTYIF : 0..1;
    TXHALFIF : 0..1;
    TXNFULLIF : 0..1;
    RESERVED1 : 0..31;
    RXNEMPTYIE : 0..1;
    RXHALFIE : 0..1;
    RXFULLIE : 0..1;
    RXOVFLIE : 0..1;
    RESERVED2 : 0..15;
    TXEMPTYIE : 0..1;
    TXHALFIE : 0..1;
    TXNFULLIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOINT27bits: TC1FIFOINT27bits absolute ($BF88BA20);
  C1FIFOINT27CLR : longWord absolute ($BF88BA24);
  C1FIFOINT27SET : longWord absolute ($BF88BA28);
  C1FIFOINT27INV : longWord absolute ($BF88BA2C);
  C1FIFOUA27 : longWord absolute ($BF88BA30);
  C1FIFOUA27CLR : longWord absolute ($BF88BA34);
  C1FIFOUA27SET : longWord absolute ($BF88BA38);
  C1FIFOUA27INV : longWord absolute ($BF88BA3C);
  C1FIFOCI27 : longWord absolute ($BF88BA40);
type
  TC1FIFOCI27bits = bitpacked record
  case integer of
  0 : (
    CFIFOCI : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOCI27bits: TC1FIFOCI27bits absolute ($BF88BA40);
  C1FIFOCI27CLR : longWord absolute ($BF88BA44);
  C1FIFOCI27SET : longWord absolute ($BF88BA48);
  C1FIFOCI27INV : longWord absolute ($BF88BA4C);
  C1FIFOCON28 : longWord absolute ($BF88BA50);
type
  TC1FIFOCON28bits = bitpacked record
  case integer of
  0 : (
    TXPRI : 0..3;
    RTREN : 0..1;
    TXREQ : 0..1;
    TXERR : 0..1;
    TXLARB : 0..1;
    TXABAT : 0..1;
    TXEN : 0..1;
    RESERVED0 : 0..15;
    DONLY : 0..1;
    UINC : 0..1;
    FRESET : 0..1;
    RESERVED1 : 0..1;
    FSIZE : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOCON28bits: TC1FIFOCON28bits absolute ($BF88BA50);
  C1FIFOCON28CLR : longWord absolute ($BF88BA54);
  C1FIFOCON28SET : longWord absolute ($BF88BA58);
  C1FIFOCON28INV : longWord absolute ($BF88BA5C);
  C1FIFOINT28 : longWord absolute ($BF88BA60);
type
  TC1FIFOINT28bits = bitpacked record
  case integer of
  0 : (
    RXNEMPTYIF : 0..1;
    RXHALFIF : 0..1;
    RXFULLIF : 0..1;
    RXOVFLIF : 0..1;
    RESERVED0 : 0..15;
    TXEMPTYIF : 0..1;
    TXHALFIF : 0..1;
    TXNFULLIF : 0..1;
    RESERVED1 : 0..31;
    RXNEMPTYIE : 0..1;
    RXHALFIE : 0..1;
    RXFULLIE : 0..1;
    RXOVFLIE : 0..1;
    RESERVED2 : 0..15;
    TXEMPTYIE : 0..1;
    TXHALFIE : 0..1;
    TXNFULLIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOINT28bits: TC1FIFOINT28bits absolute ($BF88BA60);
  C1FIFOINT28CLR : longWord absolute ($BF88BA64);
  C1FIFOINT28SET : longWord absolute ($BF88BA68);
  C1FIFOINT28INV : longWord absolute ($BF88BA6C);
  C1FIFOUA28 : longWord absolute ($BF88BA70);
  C1FIFOUA28CLR : longWord absolute ($BF88BA74);
  C1FIFOUA28SET : longWord absolute ($BF88BA78);
  C1FIFOUA28INV : longWord absolute ($BF88BA7C);
  C1FIFOCI28 : longWord absolute ($BF88BA80);
type
  TC1FIFOCI28bits = bitpacked record
  case integer of
  0 : (
    CFIFOCI : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOCI28bits: TC1FIFOCI28bits absolute ($BF88BA80);
  C1FIFOCI28CLR : longWord absolute ($BF88BA84);
  C1FIFOCI28SET : longWord absolute ($BF88BA88);
  C1FIFOCI28INV : longWord absolute ($BF88BA8C);
  C1FIFOCON29 : longWord absolute ($BF88BA90);
type
  TC1FIFOCON29bits = bitpacked record
  case integer of
  0 : (
    TXPRI : 0..3;
    RTREN : 0..1;
    TXREQ : 0..1;
    TXERR : 0..1;
    TXLARB : 0..1;
    TXABAT : 0..1;
    TXEN : 0..1;
    RESERVED0 : 0..15;
    DONLY : 0..1;
    UINC : 0..1;
    FRESET : 0..1;
    RESERVED1 : 0..1;
    FSIZE : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOCON29bits: TC1FIFOCON29bits absolute ($BF88BA90);
  C1FIFOCON29CLR : longWord absolute ($BF88BA94);
  C1FIFOCON29SET : longWord absolute ($BF88BA98);
  C1FIFOCON29INV : longWord absolute ($BF88BA9C);
  C1FIFOINT29 : longWord absolute ($BF88BAA0);
type
  TC1FIFOINT29bits = bitpacked record
  case integer of
  0 : (
    RXNEMPTYIF : 0..1;
    RXHALFIF : 0..1;
    RXFULLIF : 0..1;
    RXOVFLIF : 0..1;
    RESERVED0 : 0..15;
    TXEMPTYIF : 0..1;
    TXHALFIF : 0..1;
    TXNFULLIF : 0..1;
    RESERVED1 : 0..31;
    RXNEMPTYIE : 0..1;
    RXHALFIE : 0..1;
    RXFULLIE : 0..1;
    RXOVFLIE : 0..1;
    RESERVED2 : 0..15;
    TXEMPTYIE : 0..1;
    TXHALFIE : 0..1;
    TXNFULLIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOINT29bits: TC1FIFOINT29bits absolute ($BF88BAA0);
  C1FIFOINT29CLR : longWord absolute ($BF88BAA4);
  C1FIFOINT29SET : longWord absolute ($BF88BAA8);
  C1FIFOINT29INV : longWord absolute ($BF88BAAC);
  C1FIFOUA29 : longWord absolute ($BF88BAB0);
  C1FIFOUA29CLR : longWord absolute ($BF88BAB4);
  C1FIFOUA29SET : longWord absolute ($BF88BAB8);
  C1FIFOUA29INV : longWord absolute ($BF88BABC);
  C1FIFOCI29 : longWord absolute ($BF88BAC0);
type
  TC1FIFOCI29bits = bitpacked record
  case integer of
  0 : (
    CFIFOCI : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOCI29bits: TC1FIFOCI29bits absolute ($BF88BAC0);
  C1FIFOCI29CLR : longWord absolute ($BF88BAC4);
  C1FIFOCI29SET : longWord absolute ($BF88BAC8);
  C1FIFOCI29INV : longWord absolute ($BF88BACC);
  C1FIFOCON30 : longWord absolute ($BF88BAD0);
type
  TC1FIFOCON30bits = bitpacked record
  case integer of
  0 : (
    TXPRI : 0..3;
    RTREN : 0..1;
    TXREQ : 0..1;
    TXERR : 0..1;
    TXLARB : 0..1;
    TXABAT : 0..1;
    TXEN : 0..1;
    RESERVED0 : 0..15;
    DONLY : 0..1;
    UINC : 0..1;
    FRESET : 0..1;
    RESERVED1 : 0..1;
    FSIZE : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOCON30bits: TC1FIFOCON30bits absolute ($BF88BAD0);
  C1FIFOCON30CLR : longWord absolute ($BF88BAD4);
  C1FIFOCON30SET : longWord absolute ($BF88BAD8);
  C1FIFOCON30INV : longWord absolute ($BF88BADC);
  C1FIFOINT30 : longWord absolute ($BF88BAE0);
type
  TC1FIFOINT30bits = bitpacked record
  case integer of
  0 : (
    RXNEMPTYIF : 0..1;
    RXHALFIF : 0..1;
    RXFULLIF : 0..1;
    RXOVFLIF : 0..1;
    RESERVED0 : 0..15;
    TXEMPTYIF : 0..1;
    TXHALFIF : 0..1;
    TXNFULLIF : 0..1;
    RESERVED1 : 0..31;
    RXNEMPTYIE : 0..1;
    RXHALFIE : 0..1;
    RXFULLIE : 0..1;
    RXOVFLIE : 0..1;
    RESERVED2 : 0..15;
    TXEMPTYIE : 0..1;
    TXHALFIE : 0..1;
    TXNFULLIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOINT30bits: TC1FIFOINT30bits absolute ($BF88BAE0);
  C1FIFOINT30CLR : longWord absolute ($BF88BAE4);
  C1FIFOINT30SET : longWord absolute ($BF88BAE8);
  C1FIFOINT30INV : longWord absolute ($BF88BAEC);
  C1FIFOUA30 : longWord absolute ($BF88BAF0);
  C1FIFOUA30CLR : longWord absolute ($BF88BAF4);
  C1FIFOUA30SET : longWord absolute ($BF88BAF8);
  C1FIFOUA30INV : longWord absolute ($BF88BAFC);
  C1FIFOCI30 : longWord absolute ($BF88BB00);
type
  TC1FIFOCI30bits = bitpacked record
  case integer of
  0 : (
    CFIFOCI : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOCI30bits: TC1FIFOCI30bits absolute ($BF88BB00);
  C1FIFOCI30CLR : longWord absolute ($BF88BB04);
  C1FIFOCI30SET : longWord absolute ($BF88BB08);
  C1FIFOCI30INV : longWord absolute ($BF88BB0C);
  C1FIFOCON31 : longWord absolute ($BF88BB10);
type
  TC1FIFOCON31bits = bitpacked record
  case integer of
  0 : (
    TXPRI : 0..3;
    RTREN : 0..1;
    TXREQ : 0..1;
    TXERR : 0..1;
    TXLARB : 0..1;
    TXABAT : 0..1;
    TXEN : 0..1;
    RESERVED0 : 0..15;
    DONLY : 0..1;
    UINC : 0..1;
    FRESET : 0..1;
    RESERVED1 : 0..1;
    FSIZE : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOCON31bits: TC1FIFOCON31bits absolute ($BF88BB10);
  C1FIFOCON31CLR : longWord absolute ($BF88BB14);
  C1FIFOCON31SET : longWord absolute ($BF88BB18);
  C1FIFOCON31INV : longWord absolute ($BF88BB1C);
  C1FIFOINT31 : longWord absolute ($BF88BB20);
type
  TC1FIFOINT31bits = bitpacked record
  case integer of
  0 : (
    RXNEMPTYIF : 0..1;
    RXHALFIF : 0..1;
    RXFULLIF : 0..1;
    RXOVFLIF : 0..1;
    RESERVED0 : 0..15;
    TXEMPTYIF : 0..1;
    TXHALFIF : 0..1;
    TXNFULLIF : 0..1;
    RESERVED1 : 0..31;
    RXNEMPTYIE : 0..1;
    RXHALFIE : 0..1;
    RXFULLIE : 0..1;
    RXOVFLIE : 0..1;
    RESERVED2 : 0..15;
    TXEMPTYIE : 0..1;
    TXHALFIE : 0..1;
    TXNFULLIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOINT31bits: TC1FIFOINT31bits absolute ($BF88BB20);
  C1FIFOINT31CLR : longWord absolute ($BF88BB24);
  C1FIFOINT31SET : longWord absolute ($BF88BB28);
  C1FIFOINT31INV : longWord absolute ($BF88BB2C);
  C1FIFOUA31 : longWord absolute ($BF88BB30);
  C1FIFOUA31CLR : longWord absolute ($BF88BB34);
  C1FIFOUA31SET : longWord absolute ($BF88BB38);
  C1FIFOUA31INV : longWord absolute ($BF88BB3C);
  C1FIFOCI31 : longWord absolute ($BF88BB40);
type
  TC1FIFOCI31bits = bitpacked record
  case integer of
  0 : (
    CFIFOCI : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C1FIFOCI31bits: TC1FIFOCI31bits absolute ($BF88BB40);
  C1FIFOCI31CLR : longWord absolute ($BF88BB44);
  C1FIFOCI31SET : longWord absolute ($BF88BB48);
  C1FIFOCI31INV : longWord absolute ($BF88BB4C);
  C2CON : longWord absolute ($BF88C000);
type
  TC2CONbits = bitpacked record
  case integer of
  0 : (
    DNCNT : 0..31;
    RESERVED0 : 0..63;
    CANBUSY : 0..1;
    RESERVED1 : 0..1;
    SIDL : 0..1;
    RESERVED2 : 0..1;
    ON : 0..1;
    RESERVED3 : 0..15;
    CANCAP : 0..1;
    OPMOD : 0..7;
    REQOP : 0..7;
    ABAT : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2CONbits: TC2CONbits absolute ($BF88C000);
  C2CONCLR : longWord absolute ($BF88C004);
  C2CONSET : longWord absolute ($BF88C008);
  C2CONINV : longWord absolute ($BF88C00C);
  C2CFG : longWord absolute ($BF88C010);
type
  TC2CFGbits = bitpacked record
  case integer of
  0 : (
    BRP : 0..63;
    SJW : 0..3;
    PRSEG : 0..7;
    SEG1PH : 0..7;
    SAM : 0..1;
    SEG2PHTS : 0..1;
    SEG2PH : 0..7;
    RESERVED0 : 0..7;
    WAKFIL : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2CFGbits: TC2CFGbits absolute ($BF88C010);
  C2CFGCLR : longWord absolute ($BF88C014);
  C2CFGSET : longWord absolute ($BF88C018);
  C2CFGINV : longWord absolute ($BF88C01C);
  C2INT : longWord absolute ($BF88C020);
type
  TC2INTbits = bitpacked record
  case integer of
  0 : (
    TBIF : 0..1;
    RBIF : 0..1;
    CTMRIF : 0..1;
    MODIF : 0..1;
    RESERVED0 : 0..127;
    RBOVIF : 0..1;
    SERRIF : 0..1;
    CERRIF : 0..1;
    WAKIF : 0..1;
    IVRIF : 0..1;
    TBIE : 0..1;
    RBIE : 0..1;
    CTMRIE : 0..1;
    MODIE : 0..1;
    RESERVED1 : 0..127;
    RBOVIE : 0..1;
    SERRIE : 0..1;
    CERRIE : 0..1;
    WAKIE : 0..1;
    IVRIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2INTbits: TC2INTbits absolute ($BF88C020);
  C2INTCLR : longWord absolute ($BF88C024);
  C2INTSET : longWord absolute ($BF88C028);
  C2INTINV : longWord absolute ($BF88C02C);
  C2VEC : longWord absolute ($BF88C030);
type
  TC2VECbits = bitpacked record
  case integer of
  0 : (
    ICODE : 0..127;
    RESERVED0 : 0..1;
    FILHIT : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2VECbits: TC2VECbits absolute ($BF88C030);
  C2VECCLR : longWord absolute ($BF88C034);
  C2VECSET : longWord absolute ($BF88C038);
  C2VECINV : longWord absolute ($BF88C03C);
  C2TREC : longWord absolute ($BF88C040);
type
  TC2TRECbits = bitpacked record
  case integer of
  0 : (
    RERRCNT : 0..255;
    TERRCNT : 0..255;
    EWARN : 0..1;
    RXWARN : 0..1;
    TXWARN : 0..1;
    RXBP : 0..1;
    TXBP : 0..1;
    TXBO : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2TRECbits: TC2TRECbits absolute ($BF88C040);
  C2TRECCLR : longWord absolute ($BF88C044);
  C2TRECSET : longWord absolute ($BF88C048);
  C2TRECINV : longWord absolute ($BF88C04C);
  C2FSTAT : longWord absolute ($BF88C050);
type
  TC2FSTATbits = bitpacked record
  case integer of
  0 : (
    FIFOIP : 0..4294967295;
  );
  1 : (
    FIFOIP0 : 0..1;
    FIFOIP1 : 0..1;
    FIFOIP2 : 0..1;
    FIFOIP3 : 0..1;
    FIFOIP4 : 0..1;
    FIFOIP5 : 0..1;
    FIFOIP6 : 0..1;
    FIFOIP7 : 0..1;
    FIFOIP8 : 0..1;
    FIFOIP9 : 0..1;
    FIFOIP10 : 0..1;
    FIFOIP11 : 0..1;
    FIFOIP12 : 0..1;
    FIFOIP13 : 0..1;
    FIFOIP14 : 0..1;
    FIFOIP15 : 0..1;
    FIFOIP16 : 0..1;
    FIFOIP17 : 0..1;
    FIFOIP18 : 0..1;
    FIFOIP19 : 0..1;
    FIFOIP20 : 0..1;
    FIFOIP21 : 0..1;
    FIFOIP22 : 0..1;
    FIFOIP23 : 0..1;
    FIFOIP24 : 0..1;
    FIFOIP25 : 0..1;
    FIFOIP26 : 0..1;
    FIFOIP27 : 0..1;
    FIFOIP28 : 0..1;
    FIFOIP29 : 0..1;
    FIFOIP30 : 0..1;
    FIFOIP31 : 0..1;
  );
  2 : (
    w : 0..4294967295;
  );
  end;
var
  C2FSTATbits: TC2FSTATbits absolute ($BF88C050);
  C2FSTATCLR : longWord absolute ($BF88C054);
  C2FSTATSET : longWord absolute ($BF88C058);
  C2FSTATINV : longWord absolute ($BF88C05C);
  C2RXOVF : longWord absolute ($BF88C060);
type
  TC2RXOVFbits = bitpacked record
  case integer of
  0 : (
    RXOVF : 0..4294967295;
  );
  1 : (
    RXOVF0 : 0..1;
    RXOVF1 : 0..1;
    RXOVF2 : 0..1;
    RXOVF3 : 0..1;
    RXOVF4 : 0..1;
    RXOVF5 : 0..1;
    RXOVF6 : 0..1;
    RXOVF7 : 0..1;
    RXOVF8 : 0..1;
    RXOVF9 : 0..1;
    RXOVF10 : 0..1;
    RXOVF11 : 0..1;
    RXOVF12 : 0..1;
    RXOVF13 : 0..1;
    RXOVF14 : 0..1;
    RXOVF15 : 0..1;
    RXOVF16 : 0..1;
    RXOVF17 : 0..1;
    RXOVF18 : 0..1;
    RXOVF19 : 0..1;
    RXOVF20 : 0..1;
    RXOVF21 : 0..1;
    RXOVF22 : 0..1;
    RXOVF23 : 0..1;
    RXOVF24 : 0..1;
    RXOVF25 : 0..1;
    RXOVF26 : 0..1;
    RXOVF27 : 0..1;
    RXOVF28 : 0..1;
    RXOVF29 : 0..1;
    RXOVF30 : 0..1;
    RXOVF31 : 0..1;
  );
  2 : (
    w : 0..4294967295;
  );
  end;
var
  C2RXOVFbits: TC2RXOVFbits absolute ($BF88C060);
  C2RXOVFCLR : longWord absolute ($BF88C064);
  C2RXOVFSET : longWord absolute ($BF88C068);
  C2RXOVFINV : longWord absolute ($BF88C06C);
  C2TMR : longWord absolute ($BF88C070);
type
  TC2TMRbits = bitpacked record
  case integer of
  0 : (
    CANTSPRE : 0..65535;
    CANTS : 0..65535;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2TMRbits: TC2TMRbits absolute ($BF88C070);
  C2TMRCLR : longWord absolute ($BF88C074);
  C2TMRSET : longWord absolute ($BF88C078);
  C2TMRINV : longWord absolute ($BF88C07C);
  C2RXM0 : longWord absolute ($BF88C080);
type
  TC2RXM0bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    MIDE : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2RXM0bits: TC2RXM0bits absolute ($BF88C080);
  C2RXM0CLR : longWord absolute ($BF88C084);
  C2RXM0SET : longWord absolute ($BF88C088);
  C2RXM0INV : longWord absolute ($BF88C08C);
  C2RXM1 : longWord absolute ($BF88C090);
type
  TC2RXM1bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    MIDE : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2RXM1bits: TC2RXM1bits absolute ($BF88C090);
  C2RXM1CLR : longWord absolute ($BF88C094);
  C2RXM1SET : longWord absolute ($BF88C098);
  C2RXM1INV : longWord absolute ($BF88C09C);
  C2RXM2 : longWord absolute ($BF88C0A0);
type
  TC2RXM2bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    MIDE : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2RXM2bits: TC2RXM2bits absolute ($BF88C0A0);
  C2RXM2CLR : longWord absolute ($BF88C0A4);
  C2RXM2SET : longWord absolute ($BF88C0A8);
  C2RXM2INV : longWord absolute ($BF88C0AC);
  C2RXM3 : longWord absolute ($BF88C0B0);
type
  TC2RXM3bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    MIDE : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2RXM3bits: TC2RXM3bits absolute ($BF88C0B0);
  C2RXM3CLR : longWord absolute ($BF88C0B4);
  C2RXM3SET : longWord absolute ($BF88C0B8);
  C2RXM3INV : longWord absolute ($BF88C0BC);
  C2FLTCON0 : longWord absolute ($BF88C0C0);
type
  TC2FLTCON0bits = bitpacked record
  case integer of
  0 : (
    FSEL0 : 0..31;
    MSEL0 : 0..3;
    FLTEN0 : 0..1;
    FSEL1 : 0..31;
    MSEL1 : 0..3;
    FLTEN1 : 0..1;
    FSEL2 : 0..31;
    MSEL2 : 0..3;
    FLTEN2 : 0..1;
    FSEL3 : 0..31;
    MSEL3 : 0..3;
    FLTEN3 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FLTCON0bits: TC2FLTCON0bits absolute ($BF88C0C0);
  C2FLTCON0CLR : longWord absolute ($BF88C0C4);
  C2FLTCON0SET : longWord absolute ($BF88C0C8);
  C2FLTCON0INV : longWord absolute ($BF88C0CC);
  C2FLTCON1 : longWord absolute ($BF88C0D0);
type
  TC2FLTCON1bits = bitpacked record
  case integer of
  0 : (
    FSEL4 : 0..31;
    MSEL4 : 0..3;
    FLTEN4 : 0..1;
    FSEL5 : 0..31;
    MSEL5 : 0..3;
    FLTEN5 : 0..1;
    FSEL6 : 0..31;
    MSEL6 : 0..3;
    FLTEN6 : 0..1;
    FSEL7 : 0..31;
    MSEL7 : 0..3;
    FLTEN7 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FLTCON1bits: TC2FLTCON1bits absolute ($BF88C0D0);
  C2FLTCON1CLR : longWord absolute ($BF88C0D4);
  C2FLTCON1SET : longWord absolute ($BF88C0D8);
  C2FLTCON1INV : longWord absolute ($BF88C0DC);
  C2FLTCON2 : longWord absolute ($BF88C0E0);
type
  TC2FLTCON2bits = bitpacked record
  case integer of
  0 : (
    FSEL8 : 0..31;
    MSEL8 : 0..3;
    FLTEN8 : 0..1;
    FSEL9 : 0..31;
    MSEL9 : 0..3;
    FLTEN9 : 0..1;
    FSEL10 : 0..31;
    MSEL10 : 0..3;
    FLTEN10 : 0..1;
    FSEL11 : 0..31;
    MSEL11 : 0..3;
    FLTEN11 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FLTCON2bits: TC2FLTCON2bits absolute ($BF88C0E0);
  C2FLTCON2CLR : longWord absolute ($BF88C0E4);
  C2FLTCON2SET : longWord absolute ($BF88C0E8);
  C2FLTCON2INV : longWord absolute ($BF88C0EC);
  C2FLTCON3 : longWord absolute ($BF88C0F0);
type
  TC2FLTCON3bits = bitpacked record
  case integer of
  0 : (
    FSEL12 : 0..31;
    MSEL12 : 0..3;
    FLTEN12 : 0..1;
    FSEL13 : 0..31;
    MSEL13 : 0..3;
    FLTEN13 : 0..1;
    FSEL14 : 0..31;
    MSEL14 : 0..3;
    FLTEN14 : 0..1;
    FSEL15 : 0..31;
    MSEL15 : 0..3;
    FLTEN15 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FLTCON3bits: TC2FLTCON3bits absolute ($BF88C0F0);
  C2FLTCON3CLR : longWord absolute ($BF88C0F4);
  C2FLTCON3SET : longWord absolute ($BF88C0F8);
  C2FLTCON3INV : longWord absolute ($BF88C0FC);
  C2FLTCON4 : longWord absolute ($BF88C100);
type
  TC2FLTCON4bits = bitpacked record
  case integer of
  0 : (
    FSEL16 : 0..31;
    MSEL16 : 0..3;
    FLTEN16 : 0..1;
    FSEL17 : 0..31;
    MSEL17 : 0..3;
    FLTEN17 : 0..1;
    FSEL18 : 0..31;
    MSEL18 : 0..3;
    FLTEN18 : 0..1;
    FSEL19 : 0..31;
    MSEL19 : 0..3;
    FLTEN19 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FLTCON4bits: TC2FLTCON4bits absolute ($BF88C100);
  C2FLTCON4CLR : longWord absolute ($BF88C104);
  C2FLTCON4SET : longWord absolute ($BF88C108);
  C2FLTCON4INV : longWord absolute ($BF88C10C);
  C2FLTCON5 : longWord absolute ($BF88C110);
type
  TC2FLTCON5bits = bitpacked record
  case integer of
  0 : (
    FSEL20 : 0..31;
    MSEL20 : 0..3;
    FLTEN20 : 0..1;
    FSEL21 : 0..31;
    MSEL21 : 0..3;
    FLTEN21 : 0..1;
    FSEL22 : 0..31;
    MSEL22 : 0..3;
    FLTEN22 : 0..1;
    FSEL23 : 0..31;
    MSEL23 : 0..3;
    FLTEN23 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FLTCON5bits: TC2FLTCON5bits absolute ($BF88C110);
  C2FLTCON5CLR : longWord absolute ($BF88C114);
  C2FLTCON5SET : longWord absolute ($BF88C118);
  C2FLTCON5INV : longWord absolute ($BF88C11C);
  C2FLTCON6 : longWord absolute ($BF88C120);
type
  TC2FLTCON6bits = bitpacked record
  case integer of
  0 : (
    FSEL24 : 0..31;
    MSEL24 : 0..3;
    FLTEN24 : 0..1;
    FSEL25 : 0..31;
    MSEL25 : 0..3;
    FLTEN25 : 0..1;
    FSEL26 : 0..31;
    MSEL26 : 0..3;
    FLTEN26 : 0..1;
    FSEL27 : 0..31;
    MSEL27 : 0..3;
    FLTEN27 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FLTCON6bits: TC2FLTCON6bits absolute ($BF88C120);
  C2FLTCON6CLR : longWord absolute ($BF88C124);
  C2FLTCON6SET : longWord absolute ($BF88C128);
  C2FLTCON6INV : longWord absolute ($BF88C12C);
  C2FLTCON7 : longWord absolute ($BF88C130);
type
  TC2FLTCON7bits = bitpacked record
  case integer of
  0 : (
    FSEL28 : 0..31;
    MSEL28 : 0..3;
    FLTEN28 : 0..1;
    FSEL29 : 0..31;
    MSEL29 : 0..3;
    FLTEN29 : 0..1;
    FSEL30 : 0..31;
    MSEL30 : 0..3;
    FLTEN30 : 0..1;
    FSEL31 : 0..31;
    MSEL31 : 0..3;
    FLTEN31 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FLTCON7bits: TC2FLTCON7bits absolute ($BF88C130);
  C2FLTCON7CLR : longWord absolute ($BF88C134);
  C2FLTCON7SET : longWord absolute ($BF88C138);
  C2FLTCON7INV : longWord absolute ($BF88C13C);
  C2RXF0 : longWord absolute ($BF88C140);
type
  TC2RXF0bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    EXID : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2RXF0bits: TC2RXF0bits absolute ($BF88C140);
  C2RXF0CLR : longWord absolute ($BF88C144);
  C2RXF0SET : longWord absolute ($BF88C148);
  C2RXF0INV : longWord absolute ($BF88C14C);
  C2RXF1 : longWord absolute ($BF88C150);
type
  TC2RXF1bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    EXID : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2RXF1bits: TC2RXF1bits absolute ($BF88C150);
  C2RXF1CLR : longWord absolute ($BF88C154);
  C2RXF1SET : longWord absolute ($BF88C158);
  C2RXF1INV : longWord absolute ($BF88C15C);
  C2RXF2 : longWord absolute ($BF88C160);
type
  TC2RXF2bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    EXID : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2RXF2bits: TC2RXF2bits absolute ($BF88C160);
  C2RXF2CLR : longWord absolute ($BF88C164);
  C2RXF2SET : longWord absolute ($BF88C168);
  C2RXF2INV : longWord absolute ($BF88C16C);
  C2RXF3 : longWord absolute ($BF88C170);
type
  TC2RXF3bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    EXID : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2RXF3bits: TC2RXF3bits absolute ($BF88C170);
  C2RXF3CLR : longWord absolute ($BF88C174);
  C2RXF3SET : longWord absolute ($BF88C178);
  C2RXF3INV : longWord absolute ($BF88C17C);
  C2RXF4 : longWord absolute ($BF88C180);
type
  TC2RXF4bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    EXID : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2RXF4bits: TC2RXF4bits absolute ($BF88C180);
  C2RXF4CLR : longWord absolute ($BF88C184);
  C2RXF4SET : longWord absolute ($BF88C188);
  C2RXF4INV : longWord absolute ($BF88C18C);
  C2RXF5 : longWord absolute ($BF88C190);
type
  TC2RXF5bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    EXID : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2RXF5bits: TC2RXF5bits absolute ($BF88C190);
  C2RXF5CLR : longWord absolute ($BF88C194);
  C2RXF5SET : longWord absolute ($BF88C198);
  C2RXF5INV : longWord absolute ($BF88C19C);
  C2RXF6 : longWord absolute ($BF88C1A0);
type
  TC2RXF6bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    EXID : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2RXF6bits: TC2RXF6bits absolute ($BF88C1A0);
  C2RXF6CLR : longWord absolute ($BF88C1A4);
  C2RXF6SET : longWord absolute ($BF88C1A8);
  C2RXF6INV : longWord absolute ($BF88C1AC);
  C2RXF7 : longWord absolute ($BF88C1B0);
type
  TC2RXF7bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    EXID : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2RXF7bits: TC2RXF7bits absolute ($BF88C1B0);
  C2RXF7CLR : longWord absolute ($BF88C1B4);
  C2RXF7SET : longWord absolute ($BF88C1B8);
  C2RXF7INV : longWord absolute ($BF88C1BC);
  C2RXF8 : longWord absolute ($BF88C1C0);
type
  TC2RXF8bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    EXID : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2RXF8bits: TC2RXF8bits absolute ($BF88C1C0);
  C2RXF8CLR : longWord absolute ($BF88C1C4);
  C2RXF8SET : longWord absolute ($BF88C1C8);
  C2RXF8INV : longWord absolute ($BF88C1CC);
  C2RXF9 : longWord absolute ($BF88C1D0);
type
  TC2RXF9bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    EXID : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2RXF9bits: TC2RXF9bits absolute ($BF88C1D0);
  C2RXF9CLR : longWord absolute ($BF88C1D4);
  C2RXF9SET : longWord absolute ($BF88C1D8);
  C2RXF9INV : longWord absolute ($BF88C1DC);
  C2RXF10 : longWord absolute ($BF88C1E0);
type
  TC2RXF10bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    EXID : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2RXF10bits: TC2RXF10bits absolute ($BF88C1E0);
  C2RXF10CLR : longWord absolute ($BF88C1E4);
  C2RXF10SET : longWord absolute ($BF88C1E8);
  C2RXF10INV : longWord absolute ($BF88C1EC);
  C2RXF11 : longWord absolute ($BF88C1F0);
type
  TC2RXF11bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    EXID : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2RXF11bits: TC2RXF11bits absolute ($BF88C1F0);
  C2RXF11CLR : longWord absolute ($BF88C1F4);
  C2RXF11SET : longWord absolute ($BF88C1F8);
  C2RXF11INV : longWord absolute ($BF88C1FC);
  C2RXF12 : longWord absolute ($BF88C200);
type
  TC2RXF12bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    EXID : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2RXF12bits: TC2RXF12bits absolute ($BF88C200);
  C2RXF12CLR : longWord absolute ($BF88C204);
  C2RXF12SET : longWord absolute ($BF88C208);
  C2RXF12INV : longWord absolute ($BF88C20C);
  C2RXF13 : longWord absolute ($BF88C210);
type
  TC2RXF13bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    EXID : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2RXF13bits: TC2RXF13bits absolute ($BF88C210);
  C2RXF13CLR : longWord absolute ($BF88C214);
  C2RXF13SET : longWord absolute ($BF88C218);
  C2RXF13INV : longWord absolute ($BF88C21C);
  C2RXF14 : longWord absolute ($BF88C220);
type
  TC2RXF14bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    EXID : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2RXF14bits: TC2RXF14bits absolute ($BF88C220);
  C2RXF14CLR : longWord absolute ($BF88C224);
  C2RXF14SET : longWord absolute ($BF88C228);
  C2RXF14INV : longWord absolute ($BF88C22C);
  C2RXF15 : longWord absolute ($BF88C230);
type
  TC2RXF15bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    EXID : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2RXF15bits: TC2RXF15bits absolute ($BF88C230);
  C2RXF15CLR : longWord absolute ($BF88C234);
  C2RXF15SET : longWord absolute ($BF88C238);
  C2RXF15INV : longWord absolute ($BF88C23C);
  C2RXF16 : longWord absolute ($BF88C240);
type
  TC2RXF16bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    EXID : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2RXF16bits: TC2RXF16bits absolute ($BF88C240);
  C2RXF16CLR : longWord absolute ($BF88C244);
  C2RXF16SET : longWord absolute ($BF88C248);
  C2RXF16INV : longWord absolute ($BF88C24C);
  C2RXF17 : longWord absolute ($BF88C250);
type
  TC2RXF17bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    EXID : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2RXF17bits: TC2RXF17bits absolute ($BF88C250);
  C2RXF17CLR : longWord absolute ($BF88C254);
  C2RXF17SET : longWord absolute ($BF88C258);
  C2RXF17INV : longWord absolute ($BF88C25C);
  C2RXF18 : longWord absolute ($BF88C260);
type
  TC2RXF18bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    EXID : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2RXF18bits: TC2RXF18bits absolute ($BF88C260);
  C2RXF18CLR : longWord absolute ($BF88C264);
  C2RXF18SET : longWord absolute ($BF88C268);
  C2RXF18INV : longWord absolute ($BF88C26C);
  C2RXF19 : longWord absolute ($BF88C270);
type
  TC2RXF19bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    EXID : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2RXF19bits: TC2RXF19bits absolute ($BF88C270);
  C2RXF19CLR : longWord absolute ($BF88C274);
  C2RXF19SET : longWord absolute ($BF88C278);
  C2RXF19INV : longWord absolute ($BF88C27C);
  C2RXF20 : longWord absolute ($BF88C280);
type
  TC2RXF20bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    EXID : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2RXF20bits: TC2RXF20bits absolute ($BF88C280);
  C2RXF20CLR : longWord absolute ($BF88C284);
  C2RXF20SET : longWord absolute ($BF88C288);
  C2RXF20INV : longWord absolute ($BF88C28C);
  C2RXF21 : longWord absolute ($BF88C290);
type
  TC2RXF21bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    EXID : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2RXF21bits: TC2RXF21bits absolute ($BF88C290);
  C2RXF21CLR : longWord absolute ($BF88C294);
  C2RXF21SET : longWord absolute ($BF88C298);
  C2RXF21INV : longWord absolute ($BF88C29C);
  C2RXF22 : longWord absolute ($BF88C2A0);
type
  TC2RXF22bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    EXID : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2RXF22bits: TC2RXF22bits absolute ($BF88C2A0);
  C2RXF22CLR : longWord absolute ($BF88C2A4);
  C2RXF22SET : longWord absolute ($BF88C2A8);
  C2RXF22INV : longWord absolute ($BF88C2AC);
  C2RXF23 : longWord absolute ($BF88C2B0);
type
  TC2RXF23bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    EXID : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2RXF23bits: TC2RXF23bits absolute ($BF88C2B0);
  C2RXF23CLR : longWord absolute ($BF88C2B4);
  C2RXF23SET : longWord absolute ($BF88C2B8);
  C2RXF23INV : longWord absolute ($BF88C2BC);
  C2RXF24 : longWord absolute ($BF88C2C0);
type
  TC2RXF24bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    EXID : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2RXF24bits: TC2RXF24bits absolute ($BF88C2C0);
  C2RXF24CLR : longWord absolute ($BF88C2C4);
  C2RXF24SET : longWord absolute ($BF88C2C8);
  C2RXF24INV : longWord absolute ($BF88C2CC);
  C2RXF25 : longWord absolute ($BF88C2D0);
type
  TC2RXF25bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    EXID : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2RXF25bits: TC2RXF25bits absolute ($BF88C2D0);
  C2RXF25CLR : longWord absolute ($BF88C2D4);
  C2RXF25SET : longWord absolute ($BF88C2D8);
  C2RXF25INV : longWord absolute ($BF88C2DC);
  C2RXF26 : longWord absolute ($BF88C2E0);
type
  TC2RXF26bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    EXID : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2RXF26bits: TC2RXF26bits absolute ($BF88C2E0);
  C2RXF26CLR : longWord absolute ($BF88C2E4);
  C2RXF26SET : longWord absolute ($BF88C2E8);
  C2RXF26INV : longWord absolute ($BF88C2EC);
  C2RXF27 : longWord absolute ($BF88C2F0);
type
  TC2RXF27bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    EXID : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2RXF27bits: TC2RXF27bits absolute ($BF88C2F0);
  C2RXF27CLR : longWord absolute ($BF88C2F4);
  C2RXF27SET : longWord absolute ($BF88C2F8);
  C2RXF27INV : longWord absolute ($BF88C2FC);
  C2RXF28 : longWord absolute ($BF88C300);
type
  TC2RXF28bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    EXID : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2RXF28bits: TC2RXF28bits absolute ($BF88C300);
  C2RXF28CLR : longWord absolute ($BF88C304);
  C2RXF28SET : longWord absolute ($BF88C308);
  C2RXF28INV : longWord absolute ($BF88C30C);
  C2RXF29 : longWord absolute ($BF88C310);
type
  TC2RXF29bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    EXID : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2RXF29bits: TC2RXF29bits absolute ($BF88C310);
  C2RXF29CLR : longWord absolute ($BF88C314);
  C2RXF29SET : longWord absolute ($BF88C318);
  C2RXF29INV : longWord absolute ($BF88C31C);
  C2RXF30 : longWord absolute ($BF88C320);
type
  TC2RXF30bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    EXID : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2RXF30bits: TC2RXF30bits absolute ($BF88C320);
  C2RXF30CLR : longWord absolute ($BF88C324);
  C2RXF30SET : longWord absolute ($BF88C328);
  C2RXF30INV : longWord absolute ($BF88C32C);
  C2RXF31 : longWord absolute ($BF88C330);
type
  TC2RXF31bits = bitpacked record
  case integer of
  0 : (
    EID : 0..262143;
    RESERVED0 : 0..1;
    EXID : 0..1;
    RESERVED1 : 0..1;
    SID : 0..2047;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2RXF31bits: TC2RXF31bits absolute ($BF88C330);
  C2RXF31CLR : longWord absolute ($BF88C334);
  C2RXF31SET : longWord absolute ($BF88C338);
  C2RXF31INV : longWord absolute ($BF88C33C);
  C2FIFOBA : longWord absolute ($BF88C340);
  C2FIFOBACLR : longWord absolute ($BF88C344);
  C2FIFOBASET : longWord absolute ($BF88C348);
  C2FIFOBAINV : longWord absolute ($BF88C34C);
  C2FIFOCON0 : longWord absolute ($BF88C350);
type
  TC2FIFOCON0bits = bitpacked record
  case integer of
  0 : (
    TXPRI : 0..3;
    RTREN : 0..1;
    TXREQ : 0..1;
    TXERR : 0..1;
    TXLARB : 0..1;
    TXABAT : 0..1;
    TXEN : 0..1;
    RESERVED0 : 0..15;
    DONLY : 0..1;
    UINC : 0..1;
    FRESET : 0..1;
    RESERVED1 : 0..1;
    FSIZE : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOCON0bits: TC2FIFOCON0bits absolute ($BF88C350);
  C2FIFOCON0CLR : longWord absolute ($BF88C354);
  C2FIFOCON0SET : longWord absolute ($BF88C358);
  C2FIFOCON0INV : longWord absolute ($BF88C35C);
  C2FIFOINT0 : longWord absolute ($BF88C360);
type
  TC2FIFOINT0bits = bitpacked record
  case integer of
  0 : (
    RXNEMPTYIF : 0..1;
    RXHALFIF : 0..1;
    RXFULLIF : 0..1;
    RXOVFLIF : 0..1;
    RESERVED0 : 0..15;
    TXEMPTYIF : 0..1;
    TXHALFIF : 0..1;
    TXNFULLIF : 0..1;
    RESERVED1 : 0..31;
    RXNEMPTYIE : 0..1;
    RXHALFIE : 0..1;
    RXFULLIE : 0..1;
    RXOVFLIE : 0..1;
    RESERVED2 : 0..15;
    TXEMPTYIE : 0..1;
    TXHALFIE : 0..1;
    TXNFULLIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOINT0bits: TC2FIFOINT0bits absolute ($BF88C360);
  C2FIFOINT0CLR : longWord absolute ($BF88C364);
  C2FIFOINT0SET : longWord absolute ($BF88C368);
  C2FIFOINT0INV : longWord absolute ($BF88C36C);
  C2FIFOUA0 : longWord absolute ($BF88C370);
  C2FIFOUA0CLR : longWord absolute ($BF88C374);
  C2FIFOUA0SET : longWord absolute ($BF88C378);
  C2FIFOUA0INV : longWord absolute ($BF88C37C);
  C2FIFOCI0 : longWord absolute ($BF88C380);
type
  TC2FIFOCI0bits = bitpacked record
  case integer of
  0 : (
    CFIFOCI : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOCI0bits: TC2FIFOCI0bits absolute ($BF88C380);
  C2FIFOCI0CLR : longWord absolute ($BF88C384);
  C2FIFOCI0SET : longWord absolute ($BF88C388);
  C2FIFOCI0INV : longWord absolute ($BF88C38C);
  C2FIFOCON1 : longWord absolute ($BF88C390);
type
  TC2FIFOCON1bits = bitpacked record
  case integer of
  0 : (
    TXPRI : 0..3;
    RTREN : 0..1;
    TXREQ : 0..1;
    TXERR : 0..1;
    TXLARB : 0..1;
    TXABAT : 0..1;
    TXEN : 0..1;
    RESERVED0 : 0..15;
    DONLY : 0..1;
    UINC : 0..1;
    FRESET : 0..1;
    RESERVED1 : 0..1;
    FSIZE : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOCON1bits: TC2FIFOCON1bits absolute ($BF88C390);
  C2FIFOCON1CLR : longWord absolute ($BF88C394);
  C2FIFOCON1SET : longWord absolute ($BF88C398);
  C2FIFOCON1INV : longWord absolute ($BF88C39C);
  C2FIFOINT1 : longWord absolute ($BF88C3A0);
type
  TC2FIFOINT1bits = bitpacked record
  case integer of
  0 : (
    RXNEMPTYIF : 0..1;
    RXHALFIF : 0..1;
    RXFULLIF : 0..1;
    RXOVFLIF : 0..1;
    RESERVED0 : 0..15;
    TXEMPTYIF : 0..1;
    TXHALFIF : 0..1;
    TXNFULLIF : 0..1;
    RESERVED1 : 0..31;
    RXNEMPTYIE : 0..1;
    RXHALFIE : 0..1;
    RXFULLIE : 0..1;
    RXOVFLIE : 0..1;
    RESERVED2 : 0..15;
    TXEMPTYIE : 0..1;
    TXHALFIE : 0..1;
    TXNFULLIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOINT1bits: TC2FIFOINT1bits absolute ($BF88C3A0);
  C2FIFOINT1CLR : longWord absolute ($BF88C3A4);
  C2FIFOINT1SET : longWord absolute ($BF88C3A8);
  C2FIFOINT1INV : longWord absolute ($BF88C3AC);
  C2FIFOUA1 : longWord absolute ($BF88C3B0);
  C2FIFOUA1CLR : longWord absolute ($BF88C3B4);
  C2FIFOUA1SET : longWord absolute ($BF88C3B8);
  C2FIFOUA1INV : longWord absolute ($BF88C3BC);
  C2FIFOCI1 : longWord absolute ($BF88C3C0);
type
  TC2FIFOCI1bits = bitpacked record
  case integer of
  0 : (
    CFIFOCI : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOCI1bits: TC2FIFOCI1bits absolute ($BF88C3C0);
  C2FIFOCI1CLR : longWord absolute ($BF88C3C4);
  C2FIFOCI1SET : longWord absolute ($BF88C3C8);
  C2FIFOCI1INV : longWord absolute ($BF88C3CC);
  C2FIFOCON2 : longWord absolute ($BF88C3D0);
type
  TC2FIFOCON2bits = bitpacked record
  case integer of
  0 : (
    TXPRI : 0..3;
    RTREN : 0..1;
    TXREQ : 0..1;
    TXERR : 0..1;
    TXLARB : 0..1;
    TXABAT : 0..1;
    TXEN : 0..1;
    RESERVED0 : 0..15;
    DONLY : 0..1;
    UINC : 0..1;
    FRESET : 0..1;
    RESERVED1 : 0..1;
    FSIZE : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOCON2bits: TC2FIFOCON2bits absolute ($BF88C3D0);
  C2FIFOCON2CLR : longWord absolute ($BF88C3D4);
  C2FIFOCON2SET : longWord absolute ($BF88C3D8);
  C2FIFOCON2INV : longWord absolute ($BF88C3DC);
  C2FIFOINT2 : longWord absolute ($BF88C3E0);
type
  TC2FIFOINT2bits = bitpacked record
  case integer of
  0 : (
    RXNEMPTYIF : 0..1;
    RXHALFIF : 0..1;
    RXFULLIF : 0..1;
    RXOVFLIF : 0..1;
    RESERVED0 : 0..15;
    TXEMPTYIF : 0..1;
    TXHALFIF : 0..1;
    TXNFULLIF : 0..1;
    RESERVED1 : 0..31;
    RXNEMPTYIE : 0..1;
    RXHALFIE : 0..1;
    RXFULLIE : 0..1;
    RXOVFLIE : 0..1;
    RESERVED2 : 0..15;
    TXEMPTYIE : 0..1;
    TXHALFIE : 0..1;
    TXNFULLIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOINT2bits: TC2FIFOINT2bits absolute ($BF88C3E0);
  C2FIFOINT2CLR : longWord absolute ($BF88C3E4);
  C2FIFOINT2SET : longWord absolute ($BF88C3E8);
  C2FIFOINT2INV : longWord absolute ($BF88C3EC);
  C2FIFOUA2 : longWord absolute ($BF88C3F0);
  C2FIFOUA2CLR : longWord absolute ($BF88C3F4);
  C2FIFOUA2SET : longWord absolute ($BF88C3F8);
  C2FIFOUA2INV : longWord absolute ($BF88C3FC);
  C2FIFOCI2 : longWord absolute ($BF88C400);
type
  TC2FIFOCI2bits = bitpacked record
  case integer of
  0 : (
    CFIFOCI : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOCI2bits: TC2FIFOCI2bits absolute ($BF88C400);
  C2FIFOCI2CLR : longWord absolute ($BF88C404);
  C2FIFOCI2SET : longWord absolute ($BF88C408);
  C2FIFOCI2INV : longWord absolute ($BF88C40C);
  C2FIFOCON3 : longWord absolute ($BF88C410);
type
  TC2FIFOCON3bits = bitpacked record
  case integer of
  0 : (
    TXPRI : 0..3;
    RTREN : 0..1;
    TXREQ : 0..1;
    TXERR : 0..1;
    TXLARB : 0..1;
    TXABAT : 0..1;
    TXEN : 0..1;
    RESERVED0 : 0..15;
    DONLY : 0..1;
    UINC : 0..1;
    FRESET : 0..1;
    RESERVED1 : 0..1;
    FSIZE : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOCON3bits: TC2FIFOCON3bits absolute ($BF88C410);
  C2FIFOCON3CLR : longWord absolute ($BF88C414);
  C2FIFOCON3SET : longWord absolute ($BF88C418);
  C2FIFOCON3INV : longWord absolute ($BF88C41C);
  C2FIFOINT3 : longWord absolute ($BF88C420);
type
  TC2FIFOINT3bits = bitpacked record
  case integer of
  0 : (
    RXNEMPTYIF : 0..1;
    RXHALFIF : 0..1;
    RXFULLIF : 0..1;
    RXOVFLIF : 0..1;
    RESERVED0 : 0..15;
    TXEMPTYIF : 0..1;
    TXHALFIF : 0..1;
    TXNFULLIF : 0..1;
    RESERVED1 : 0..31;
    RXNEMPTYIE : 0..1;
    RXHALFIE : 0..1;
    RXFULLIE : 0..1;
    RXOVFLIE : 0..1;
    RESERVED2 : 0..15;
    TXEMPTYIE : 0..1;
    TXHALFIE : 0..1;
    TXNFULLIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOINT3bits: TC2FIFOINT3bits absolute ($BF88C420);
  C2FIFOINT3CLR : longWord absolute ($BF88C424);
  C2FIFOINT3SET : longWord absolute ($BF88C428);
  C2FIFOINT3INV : longWord absolute ($BF88C42C);
  C2FIFOUA3 : longWord absolute ($BF88C430);
  C2FIFOUA3CLR : longWord absolute ($BF88C434);
  C2FIFOUA3SET : longWord absolute ($BF88C438);
  C2FIFOUA3INV : longWord absolute ($BF88C43C);
  C2FIFOCI3 : longWord absolute ($BF88C440);
type
  TC2FIFOCI3bits = bitpacked record
  case integer of
  0 : (
    CFIFOCI : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOCI3bits: TC2FIFOCI3bits absolute ($BF88C440);
  C2FIFOCI3CLR : longWord absolute ($BF88C444);
  C2FIFOCI3SET : longWord absolute ($BF88C448);
  C2FIFOCI3INV : longWord absolute ($BF88C44C);
  C2FIFOCON4 : longWord absolute ($BF88C450);
type
  TC2FIFOCON4bits = bitpacked record
  case integer of
  0 : (
    TXPRI : 0..3;
    RTREN : 0..1;
    TXREQ : 0..1;
    TXERR : 0..1;
    TXLARB : 0..1;
    TXABAT : 0..1;
    TXEN : 0..1;
    RESERVED0 : 0..15;
    DONLY : 0..1;
    UINC : 0..1;
    FRESET : 0..1;
    RESERVED1 : 0..1;
    FSIZE : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOCON4bits: TC2FIFOCON4bits absolute ($BF88C450);
  C2FIFOCON4CLR : longWord absolute ($BF88C454);
  C2FIFOCON4SET : longWord absolute ($BF88C458);
  C2FIFOCON4INV : longWord absolute ($BF88C45C);
  C2FIFOINT4 : longWord absolute ($BF88C460);
type
  TC2FIFOINT4bits = bitpacked record
  case integer of
  0 : (
    RXNEMPTYIF : 0..1;
    RXHALFIF : 0..1;
    RXFULLIF : 0..1;
    RXOVFLIF : 0..1;
    RESERVED0 : 0..15;
    TXEMPTYIF : 0..1;
    TXHALFIF : 0..1;
    TXNFULLIF : 0..1;
    RESERVED1 : 0..31;
    RXNEMPTYIE : 0..1;
    RXHALFIE : 0..1;
    RXFULLIE : 0..1;
    RXOVFLIE : 0..1;
    RESERVED2 : 0..15;
    TXEMPTYIE : 0..1;
    TXHALFIE : 0..1;
    TXNFULLIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOINT4bits: TC2FIFOINT4bits absolute ($BF88C460);
  C2FIFOINT4CLR : longWord absolute ($BF88C464);
  C2FIFOINT4SET : longWord absolute ($BF88C468);
  C2FIFOINT4INV : longWord absolute ($BF88C46C);
  C2FIFOUA4 : longWord absolute ($BF88C470);
  C2FIFOUA4CLR : longWord absolute ($BF88C474);
  C2FIFOUA4SET : longWord absolute ($BF88C478);
  C2FIFOUA4INV : longWord absolute ($BF88C47C);
  C2FIFOCI4 : longWord absolute ($BF88C480);
type
  TC2FIFOCI4bits = bitpacked record
  case integer of
  0 : (
    CFIFOCI : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOCI4bits: TC2FIFOCI4bits absolute ($BF88C480);
  C2FIFOCI4CLR : longWord absolute ($BF88C484);
  C2FIFOCI4SET : longWord absolute ($BF88C488);
  C2FIFOCI4INV : longWord absolute ($BF88C48C);
  C2FIFOCON5 : longWord absolute ($BF88C490);
type
  TC2FIFOCON5bits = bitpacked record
  case integer of
  0 : (
    TXPRI : 0..3;
    RTREN : 0..1;
    TXREQ : 0..1;
    TXERR : 0..1;
    TXLARB : 0..1;
    TXABAT : 0..1;
    TXEN : 0..1;
    RESERVED0 : 0..15;
    DONLY : 0..1;
    UINC : 0..1;
    FRESET : 0..1;
    RESERVED1 : 0..1;
    FSIZE : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOCON5bits: TC2FIFOCON5bits absolute ($BF88C490);
  C2FIFOCON5CLR : longWord absolute ($BF88C494);
  C2FIFOCON5SET : longWord absolute ($BF88C498);
  C2FIFOCON5INV : longWord absolute ($BF88C49C);
  C2FIFOINT5 : longWord absolute ($BF88C4A0);
type
  TC2FIFOINT5bits = bitpacked record
  case integer of
  0 : (
    RXNEMPTYIF : 0..1;
    RXHALFIF : 0..1;
    RXFULLIF : 0..1;
    RXOVFLIF : 0..1;
    RESERVED0 : 0..15;
    TXEMPTYIF : 0..1;
    TXHALFIF : 0..1;
    TXNFULLIF : 0..1;
    RESERVED1 : 0..31;
    RXNEMPTYIE : 0..1;
    RXHALFIE : 0..1;
    RXFULLIE : 0..1;
    RXOVFLIE : 0..1;
    RESERVED2 : 0..15;
    TXEMPTYIE : 0..1;
    TXHALFIE : 0..1;
    TXNFULLIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOINT5bits: TC2FIFOINT5bits absolute ($BF88C4A0);
  C2FIFOINT5CLR : longWord absolute ($BF88C4A4);
  C2FIFOINT5SET : longWord absolute ($BF88C4A8);
  C2FIFOINT5INV : longWord absolute ($BF88C4AC);
  C2FIFOUA5 : longWord absolute ($BF88C4B0);
  C2FIFOUA5CLR : longWord absolute ($BF88C4B4);
  C2FIFOUA5SET : longWord absolute ($BF88C4B8);
  C2FIFOUA5INV : longWord absolute ($BF88C4BC);
  C2FIFOCI5 : longWord absolute ($BF88C4C0);
type
  TC2FIFOCI5bits = bitpacked record
  case integer of
  0 : (
    CFIFOCI : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOCI5bits: TC2FIFOCI5bits absolute ($BF88C4C0);
  C2FIFOCI5CLR : longWord absolute ($BF88C4C4);
  C2FIFOCI5SET : longWord absolute ($BF88C4C8);
  C2FIFOCI5INV : longWord absolute ($BF88C4CC);
  C2FIFOCON6 : longWord absolute ($BF88C4D0);
type
  TC2FIFOCON6bits = bitpacked record
  case integer of
  0 : (
    TXPRI : 0..3;
    RTREN : 0..1;
    TXREQ : 0..1;
    TXERR : 0..1;
    TXLARB : 0..1;
    TXABAT : 0..1;
    TXEN : 0..1;
    RESERVED0 : 0..15;
    DONLY : 0..1;
    UINC : 0..1;
    FRESET : 0..1;
    RESERVED1 : 0..1;
    FSIZE : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOCON6bits: TC2FIFOCON6bits absolute ($BF88C4D0);
  C2FIFOCON6CLR : longWord absolute ($BF88C4D4);
  C2FIFOCON6SET : longWord absolute ($BF88C4D8);
  C2FIFOCON6INV : longWord absolute ($BF88C4DC);
  C2FIFOINT6 : longWord absolute ($BF88C4E0);
type
  TC2FIFOINT6bits = bitpacked record
  case integer of
  0 : (
    RXNEMPTYIF : 0..1;
    RXHALFIF : 0..1;
    RXFULLIF : 0..1;
    RXOVFLIF : 0..1;
    RESERVED0 : 0..15;
    TXEMPTYIF : 0..1;
    TXHALFIF : 0..1;
    TXNFULLIF : 0..1;
    RESERVED1 : 0..31;
    RXNEMPTYIE : 0..1;
    RXHALFIE : 0..1;
    RXFULLIE : 0..1;
    RXOVFLIE : 0..1;
    RESERVED2 : 0..15;
    TXEMPTYIE : 0..1;
    TXHALFIE : 0..1;
    TXNFULLIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOINT6bits: TC2FIFOINT6bits absolute ($BF88C4E0);
  C2FIFOINT6CLR : longWord absolute ($BF88C4E4);
  C2FIFOINT6SET : longWord absolute ($BF88C4E8);
  C2FIFOINT6INV : longWord absolute ($BF88C4EC);
  C2FIFOUA6 : longWord absolute ($BF88C4F0);
  C2FIFOUA6CLR : longWord absolute ($BF88C4F4);
  C2FIFOUA6SET : longWord absolute ($BF88C4F8);
  C2FIFOUA6INV : longWord absolute ($BF88C4FC);
  C2FIFOCI6 : longWord absolute ($BF88C500);
type
  TC2FIFOCI6bits = bitpacked record
  case integer of
  0 : (
    CFIFOCI : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOCI6bits: TC2FIFOCI6bits absolute ($BF88C500);
  C2FIFOCI6CLR : longWord absolute ($BF88C504);
  C2FIFOCI6SET : longWord absolute ($BF88C508);
  C2FIFOCI6INV : longWord absolute ($BF88C50C);
  C2FIFOCON7 : longWord absolute ($BF88C510);
type
  TC2FIFOCON7bits = bitpacked record
  case integer of
  0 : (
    TXPRI : 0..3;
    RTREN : 0..1;
    TXREQ : 0..1;
    TXERR : 0..1;
    TXLARB : 0..1;
    TXABAT : 0..1;
    TXEN : 0..1;
    RESERVED0 : 0..15;
    DONLY : 0..1;
    UINC : 0..1;
    FRESET : 0..1;
    RESERVED1 : 0..1;
    FSIZE : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOCON7bits: TC2FIFOCON7bits absolute ($BF88C510);
  C2FIFOCON7CLR : longWord absolute ($BF88C514);
  C2FIFOCON7SET : longWord absolute ($BF88C518);
  C2FIFOCON7INV : longWord absolute ($BF88C51C);
  C2FIFOINT7 : longWord absolute ($BF88C520);
type
  TC2FIFOINT7bits = bitpacked record
  case integer of
  0 : (
    RXNEMPTYIF : 0..1;
    RXHALFIF : 0..1;
    RXFULLIF : 0..1;
    RXOVFLIF : 0..1;
    RESERVED0 : 0..15;
    TXEMPTYIF : 0..1;
    TXHALFIF : 0..1;
    TXNFULLIF : 0..1;
    RESERVED1 : 0..31;
    RXNEMPTYIE : 0..1;
    RXHALFIE : 0..1;
    RXFULLIE : 0..1;
    RXOVFLIE : 0..1;
    RESERVED2 : 0..15;
    TXEMPTYIE : 0..1;
    TXHALFIE : 0..1;
    TXNFULLIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOINT7bits: TC2FIFOINT7bits absolute ($BF88C520);
  C2FIFOINT7CLR : longWord absolute ($BF88C524);
  C2FIFOINT7SET : longWord absolute ($BF88C528);
  C2FIFOINT7INV : longWord absolute ($BF88C52C);
  C2FIFOUA7 : longWord absolute ($BF88C530);
  C2FIFOUA7CLR : longWord absolute ($BF88C534);
  C2FIFOUA7SET : longWord absolute ($BF88C538);
  C2FIFOUA7INV : longWord absolute ($BF88C53C);
  C2FIFOCI7 : longWord absolute ($BF88C540);
type
  TC2FIFOCI7bits = bitpacked record
  case integer of
  0 : (
    CFIFOCI : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOCI7bits: TC2FIFOCI7bits absolute ($BF88C540);
  C2FIFOCI7CLR : longWord absolute ($BF88C544);
  C2FIFOCI7SET : longWord absolute ($BF88C548);
  C2FIFOCI7INV : longWord absolute ($BF88C54C);
  C2FIFOCON8 : longWord absolute ($BF88C550);
type
  TC2FIFOCON8bits = bitpacked record
  case integer of
  0 : (
    TXPRI : 0..3;
    RTREN : 0..1;
    TXREQ : 0..1;
    TXERR : 0..1;
    TXLARB : 0..1;
    TXABAT : 0..1;
    TXEN : 0..1;
    RESERVED0 : 0..15;
    DONLY : 0..1;
    UINC : 0..1;
    FRESET : 0..1;
    RESERVED1 : 0..1;
    FSIZE : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOCON8bits: TC2FIFOCON8bits absolute ($BF88C550);
  C2FIFOCON8CLR : longWord absolute ($BF88C554);
  C2FIFOCON8SET : longWord absolute ($BF88C558);
  C2FIFOCON8INV : longWord absolute ($BF88C55C);
  C2FIFOINT8 : longWord absolute ($BF88C560);
type
  TC2FIFOINT8bits = bitpacked record
  case integer of
  0 : (
    RXNEMPTYIF : 0..1;
    RXHALFIF : 0..1;
    RXFULLIF : 0..1;
    RXOVFLIF : 0..1;
    RESERVED0 : 0..15;
    TXEMPTYIF : 0..1;
    TXHALFIF : 0..1;
    TXNFULLIF : 0..1;
    RESERVED1 : 0..31;
    RXNEMPTYIE : 0..1;
    RXHALFIE : 0..1;
    RXFULLIE : 0..1;
    RXOVFLIE : 0..1;
    RESERVED2 : 0..15;
    TXEMPTYIE : 0..1;
    TXHALFIE : 0..1;
    TXNFULLIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOINT8bits: TC2FIFOINT8bits absolute ($BF88C560);
  C2FIFOINT8CLR : longWord absolute ($BF88C564);
  C2FIFOINT8SET : longWord absolute ($BF88C568);
  C2FIFOINT8INV : longWord absolute ($BF88C56C);
  C2FIFOUA8 : longWord absolute ($BF88C570);
  C2FIFOUA8CLR : longWord absolute ($BF88C574);
  C2FIFOUA8SET : longWord absolute ($BF88C578);
  C2FIFOUA8INV : longWord absolute ($BF88C57C);
  C2FIFOCI8 : longWord absolute ($BF88C580);
type
  TC2FIFOCI8bits = bitpacked record
  case integer of
  0 : (
    CFIFOCI : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOCI8bits: TC2FIFOCI8bits absolute ($BF88C580);
  C2FIFOCI8CLR : longWord absolute ($BF88C584);
  C2FIFOCI8SET : longWord absolute ($BF88C588);
  C2FIFOCI8INV : longWord absolute ($BF88C58C);
  C2FIFOCON9 : longWord absolute ($BF88C590);
type
  TC2FIFOCON9bits = bitpacked record
  case integer of
  0 : (
    TXPRI : 0..3;
    RTREN : 0..1;
    TXREQ : 0..1;
    TXERR : 0..1;
    TXLARB : 0..1;
    TXABAT : 0..1;
    TXEN : 0..1;
    RESERVED0 : 0..15;
    DONLY : 0..1;
    UINC : 0..1;
    FRESET : 0..1;
    RESERVED1 : 0..1;
    FSIZE : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOCON9bits: TC2FIFOCON9bits absolute ($BF88C590);
  C2FIFOCON9CLR : longWord absolute ($BF88C594);
  C2FIFOCON9SET : longWord absolute ($BF88C598);
  C2FIFOCON9INV : longWord absolute ($BF88C59C);
  C2FIFOINT9 : longWord absolute ($BF88C5A0);
type
  TC2FIFOINT9bits = bitpacked record
  case integer of
  0 : (
    RXNEMPTYIF : 0..1;
    RXHALFIF : 0..1;
    RXFULLIF : 0..1;
    RXOVFLIF : 0..1;
    RESERVED0 : 0..15;
    TXEMPTYIF : 0..1;
    TXHALFIF : 0..1;
    TXNFULLIF : 0..1;
    RESERVED1 : 0..31;
    RXNEMPTYIE : 0..1;
    RXHALFIE : 0..1;
    RXFULLIE : 0..1;
    RXOVFLIE : 0..1;
    RESERVED2 : 0..15;
    TXEMPTYIE : 0..1;
    TXHALFIE : 0..1;
    TXNFULLIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOINT9bits: TC2FIFOINT9bits absolute ($BF88C5A0);
  C2FIFOINT9CLR : longWord absolute ($BF88C5A4);
  C2FIFOINT9SET : longWord absolute ($BF88C5A8);
  C2FIFOINT9INV : longWord absolute ($BF88C5AC);
  C2FIFOUA9 : longWord absolute ($BF88C5B0);
  C2FIFOUA9CLR : longWord absolute ($BF88C5B4);
  C2FIFOUA9SET : longWord absolute ($BF88C5B8);
  C2FIFOUA9INV : longWord absolute ($BF88C5BC);
  C2FIFOCI9 : longWord absolute ($BF88C5C0);
type
  TC2FIFOCI9bits = bitpacked record
  case integer of
  0 : (
    CFIFOCI : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOCI9bits: TC2FIFOCI9bits absolute ($BF88C5C0);
  C2FIFOCI9CLR : longWord absolute ($BF88C5C4);
  C2FIFOCI9SET : longWord absolute ($BF88C5C8);
  C2FIFOCI9INV : longWord absolute ($BF88C5CC);
  C2FIFOCON10 : longWord absolute ($BF88C5D0);
type
  TC2FIFOCON10bits = bitpacked record
  case integer of
  0 : (
    TXPRI : 0..3;
    RTREN : 0..1;
    TXREQ : 0..1;
    TXERR : 0..1;
    TXLARB : 0..1;
    TXABAT : 0..1;
    TXEN : 0..1;
    RESERVED0 : 0..15;
    DONLY : 0..1;
    UINC : 0..1;
    FRESET : 0..1;
    RESERVED1 : 0..1;
    FSIZE : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOCON10bits: TC2FIFOCON10bits absolute ($BF88C5D0);
  C2FIFOCON10CLR : longWord absolute ($BF88C5D4);
  C2FIFOCON10SET : longWord absolute ($BF88C5D8);
  C2FIFOCON10INV : longWord absolute ($BF88C5DC);
  C2FIFOINT10 : longWord absolute ($BF88C5E0);
type
  TC2FIFOINT10bits = bitpacked record
  case integer of
  0 : (
    RXNEMPTYIF : 0..1;
    RXHALFIF : 0..1;
    RXFULLIF : 0..1;
    RXOVFLIF : 0..1;
    RESERVED0 : 0..15;
    TXEMPTYIF : 0..1;
    TXHALFIF : 0..1;
    TXNFULLIF : 0..1;
    RESERVED1 : 0..31;
    RXNEMPTYIE : 0..1;
    RXHALFIE : 0..1;
    RXFULLIE : 0..1;
    RXOVFLIE : 0..1;
    RESERVED2 : 0..15;
    TXEMPTYIE : 0..1;
    TXHALFIE : 0..1;
    TXNFULLIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOINT10bits: TC2FIFOINT10bits absolute ($BF88C5E0);
  C2FIFOINT10CLR : longWord absolute ($BF88C5E4);
  C2FIFOINT10SET : longWord absolute ($BF88C5E8);
  C2FIFOINT10INV : longWord absolute ($BF88C5EC);
  C2FIFOUA10 : longWord absolute ($BF88C5F0);
  C2FIFOUA10CLR : longWord absolute ($BF88C5F4);
  C2FIFOUA10SET : longWord absolute ($BF88C5F8);
  C2FIFOUA10INV : longWord absolute ($BF88C5FC);
  C2FIFOCI10 : longWord absolute ($BF88C600);
type
  TC2FIFOCI10bits = bitpacked record
  case integer of
  0 : (
    CFIFOCI : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOCI10bits: TC2FIFOCI10bits absolute ($BF88C600);
  C2FIFOCI10CLR : longWord absolute ($BF88C604);
  C2FIFOCI10SET : longWord absolute ($BF88C608);
  C2FIFOCI10INV : longWord absolute ($BF88C60C);
  C2FIFOCON11 : longWord absolute ($BF88C610);
type
  TC2FIFOCON11bits = bitpacked record
  case integer of
  0 : (
    TXPRI : 0..3;
    RTREN : 0..1;
    TXREQ : 0..1;
    TXERR : 0..1;
    TXLARB : 0..1;
    TXABAT : 0..1;
    TXEN : 0..1;
    RESERVED0 : 0..15;
    DONLY : 0..1;
    UINC : 0..1;
    FRESET : 0..1;
    RESERVED1 : 0..1;
    FSIZE : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOCON11bits: TC2FIFOCON11bits absolute ($BF88C610);
  C2FIFOCON11CLR : longWord absolute ($BF88C614);
  C2FIFOCON11SET : longWord absolute ($BF88C618);
  C2FIFOCON11INV : longWord absolute ($BF88C61C);
  C2FIFOINT11 : longWord absolute ($BF88C620);
type
  TC2FIFOINT11bits = bitpacked record
  case integer of
  0 : (
    RXNEMPTYIF : 0..1;
    RXHALFIF : 0..1;
    RXFULLIF : 0..1;
    RXOVFLIF : 0..1;
    RESERVED0 : 0..15;
    TXEMPTYIF : 0..1;
    TXHALFIF : 0..1;
    TXNFULLIF : 0..1;
    RESERVED1 : 0..31;
    RXNEMPTYIE : 0..1;
    RXHALFIE : 0..1;
    RXFULLIE : 0..1;
    RXOVFLIE : 0..1;
    RESERVED2 : 0..15;
    TXEMPTYIE : 0..1;
    TXHALFIE : 0..1;
    TXNFULLIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOINT11bits: TC2FIFOINT11bits absolute ($BF88C620);
  C2FIFOINT11CLR : longWord absolute ($BF88C624);
  C2FIFOINT11SET : longWord absolute ($BF88C628);
  C2FIFOINT11INV : longWord absolute ($BF88C62C);
  C2FIFOUA11 : longWord absolute ($BF88C630);
  C2FIFOUA11CLR : longWord absolute ($BF88C634);
  C2FIFOUA11SET : longWord absolute ($BF88C638);
  C2FIFOUA11INV : longWord absolute ($BF88C63C);
  C2FIFOCI11 : longWord absolute ($BF88C640);
type
  TC2FIFOCI11bits = bitpacked record
  case integer of
  0 : (
    CFIFOCI : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOCI11bits: TC2FIFOCI11bits absolute ($BF88C640);
  C2FIFOCI11CLR : longWord absolute ($BF88C644);
  C2FIFOCI11SET : longWord absolute ($BF88C648);
  C2FIFOCI11INV : longWord absolute ($BF88C64C);
  C2FIFOCON12 : longWord absolute ($BF88C650);
type
  TC2FIFOCON12bits = bitpacked record
  case integer of
  0 : (
    TXPRI : 0..3;
    RTREN : 0..1;
    TXREQ : 0..1;
    TXERR : 0..1;
    TXLARB : 0..1;
    TXABAT : 0..1;
    TXEN : 0..1;
    RESERVED0 : 0..15;
    DONLY : 0..1;
    UINC : 0..1;
    FRESET : 0..1;
    RESERVED1 : 0..1;
    FSIZE : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOCON12bits: TC2FIFOCON12bits absolute ($BF88C650);
  C2FIFOCON12CLR : longWord absolute ($BF88C654);
  C2FIFOCON12SET : longWord absolute ($BF88C658);
  C2FIFOCON12INV : longWord absolute ($BF88C65C);
  C2FIFOINT12 : longWord absolute ($BF88C660);
type
  TC2FIFOINT12bits = bitpacked record
  case integer of
  0 : (
    RXNEMPTYIF : 0..1;
    RXHALFIF : 0..1;
    RXFULLIF : 0..1;
    RXOVFLIF : 0..1;
    RESERVED0 : 0..15;
    TXEMPTYIF : 0..1;
    TXHALFIF : 0..1;
    TXNFULLIF : 0..1;
    RESERVED1 : 0..31;
    RXNEMPTYIE : 0..1;
    RXHALFIE : 0..1;
    RXFULLIE : 0..1;
    RXOVFLIE : 0..1;
    RESERVED2 : 0..15;
    TXEMPTYIE : 0..1;
    TXHALFIE : 0..1;
    TXNFULLIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOINT12bits: TC2FIFOINT12bits absolute ($BF88C660);
  C2FIFOINT12CLR : longWord absolute ($BF88C664);
  C2FIFOINT12SET : longWord absolute ($BF88C668);
  C2FIFOINT12INV : longWord absolute ($BF88C66C);
  C2FIFOUA12 : longWord absolute ($BF88C670);
  C2FIFOUA12CLR : longWord absolute ($BF88C674);
  C2FIFOUA12SET : longWord absolute ($BF88C678);
  C2FIFOUA12INV : longWord absolute ($BF88C67C);
  C2FIFOCI12 : longWord absolute ($BF88C680);
type
  TC2FIFOCI12bits = bitpacked record
  case integer of
  0 : (
    CFIFOCI : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOCI12bits: TC2FIFOCI12bits absolute ($BF88C680);
  C2FIFOCI12CLR : longWord absolute ($BF88C684);
  C2FIFOCI12SET : longWord absolute ($BF88C688);
  C2FIFOCI12INV : longWord absolute ($BF88C68C);
  C2FIFOCON13 : longWord absolute ($BF88C690);
type
  TC2FIFOCON13bits = bitpacked record
  case integer of
  0 : (
    TXPRI : 0..3;
    RTREN : 0..1;
    TXREQ : 0..1;
    TXERR : 0..1;
    TXLARB : 0..1;
    TXABAT : 0..1;
    TXEN : 0..1;
    RESERVED0 : 0..15;
    DONLY : 0..1;
    UINC : 0..1;
    FRESET : 0..1;
    RESERVED1 : 0..1;
    FSIZE : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOCON13bits: TC2FIFOCON13bits absolute ($BF88C690);
  C2FIFOCON13CLR : longWord absolute ($BF88C694);
  C2FIFOCON13SET : longWord absolute ($BF88C698);
  C2FIFOCON13INV : longWord absolute ($BF88C69C);
  C2FIFOINT13 : longWord absolute ($BF88C6A0);
type
  TC2FIFOINT13bits = bitpacked record
  case integer of
  0 : (
    RXNEMPTYIF : 0..1;
    RXHALFIF : 0..1;
    RXFULLIF : 0..1;
    RXOVFLIF : 0..1;
    RESERVED0 : 0..15;
    TXEMPTYIF : 0..1;
    TXHALFIF : 0..1;
    TXNFULLIF : 0..1;
    RESERVED1 : 0..31;
    RXNEMPTYIE : 0..1;
    RXHALFIE : 0..1;
    RXFULLIE : 0..1;
    RXOVFLIE : 0..1;
    RESERVED2 : 0..15;
    TXEMPTYIE : 0..1;
    TXHALFIE : 0..1;
    TXNFULLIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOINT13bits: TC2FIFOINT13bits absolute ($BF88C6A0);
  C2FIFOINT13CLR : longWord absolute ($BF88C6A4);
  C2FIFOINT13SET : longWord absolute ($BF88C6A8);
  C2FIFOINT13INV : longWord absolute ($BF88C6AC);
  C2FIFOUA13 : longWord absolute ($BF88C6B0);
  C2FIFOUA13CLR : longWord absolute ($BF88C6B4);
  C2FIFOUA13SET : longWord absolute ($BF88C6B8);
  C2FIFOUA13INV : longWord absolute ($BF88C6BC);
  C2FIFOCI13 : longWord absolute ($BF88C6C0);
type
  TC2FIFOCI13bits = bitpacked record
  case integer of
  0 : (
    CFIFOCI : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOCI13bits: TC2FIFOCI13bits absolute ($BF88C6C0);
  C2FIFOCI13CLR : longWord absolute ($BF88C6C4);
  C2FIFOCI13SET : longWord absolute ($BF88C6C8);
  C2FIFOCI13INV : longWord absolute ($BF88C6CC);
  C2FIFOCON14 : longWord absolute ($BF88C6D0);
type
  TC2FIFOCON14bits = bitpacked record
  case integer of
  0 : (
    TXPRI : 0..3;
    RTREN : 0..1;
    TXREQ : 0..1;
    TXERR : 0..1;
    TXLARB : 0..1;
    TXABAT : 0..1;
    TXEN : 0..1;
    RESERVED0 : 0..15;
    DONLY : 0..1;
    UINC : 0..1;
    FRESET : 0..1;
    RESERVED1 : 0..1;
    FSIZE : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOCON14bits: TC2FIFOCON14bits absolute ($BF88C6D0);
  C2FIFOCON14CLR : longWord absolute ($BF88C6D4);
  C2FIFOCON14SET : longWord absolute ($BF88C6D8);
  C2FIFOCON14INV : longWord absolute ($BF88C6DC);
  C2FIFOINT14 : longWord absolute ($BF88C6E0);
type
  TC2FIFOINT14bits = bitpacked record
  case integer of
  0 : (
    RXNEMPTYIF : 0..1;
    RXHALFIF : 0..1;
    RXFULLIF : 0..1;
    RXOVFLIF : 0..1;
    RESERVED0 : 0..15;
    TXEMPTYIF : 0..1;
    TXHALFIF : 0..1;
    TXNFULLIF : 0..1;
    RESERVED1 : 0..31;
    RXNEMPTYIE : 0..1;
    RXHALFIE : 0..1;
    RXFULLIE : 0..1;
    RXOVFLIE : 0..1;
    RESERVED2 : 0..15;
    TXEMPTYIE : 0..1;
    TXHALFIE : 0..1;
    TXNFULLIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOINT14bits: TC2FIFOINT14bits absolute ($BF88C6E0);
  C2FIFOINT14CLR : longWord absolute ($BF88C6E4);
  C2FIFOINT14SET : longWord absolute ($BF88C6E8);
  C2FIFOINT14INV : longWord absolute ($BF88C6EC);
  C2FIFOUA14 : longWord absolute ($BF88C6F0);
  C2FIFOUA14CLR : longWord absolute ($BF88C6F4);
  C2FIFOUA14SET : longWord absolute ($BF88C6F8);
  C2FIFOUA14INV : longWord absolute ($BF88C6FC);
  C2FIFOCI14 : longWord absolute ($BF88C700);
type
  TC2FIFOCI14bits = bitpacked record
  case integer of
  0 : (
    CFIFOCI : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOCI14bits: TC2FIFOCI14bits absolute ($BF88C700);
  C2FIFOCI14CLR : longWord absolute ($BF88C704);
  C2FIFOCI14SET : longWord absolute ($BF88C708);
  C2FIFOCI14INV : longWord absolute ($BF88C70C);
  C2FIFOCON15 : longWord absolute ($BF88C710);
type
  TC2FIFOCON15bits = bitpacked record
  case integer of
  0 : (
    TXPRI : 0..3;
    RTREN : 0..1;
    TXREQ : 0..1;
    TXERR : 0..1;
    TXLARB : 0..1;
    TXABAT : 0..1;
    TXEN : 0..1;
    RESERVED0 : 0..15;
    DONLY : 0..1;
    UINC : 0..1;
    FRESET : 0..1;
    RESERVED1 : 0..1;
    FSIZE : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOCON15bits: TC2FIFOCON15bits absolute ($BF88C710);
  C2FIFOCON15CLR : longWord absolute ($BF88C714);
  C2FIFOCON15SET : longWord absolute ($BF88C718);
  C2FIFOCON15INV : longWord absolute ($BF88C71C);
  C2FIFOINT15 : longWord absolute ($BF88C720);
type
  TC2FIFOINT15bits = bitpacked record
  case integer of
  0 : (
    RXNEMPTYIF : 0..1;
    RXHALFIF : 0..1;
    RXFULLIF : 0..1;
    RXOVFLIF : 0..1;
    RESERVED0 : 0..15;
    TXEMPTYIF : 0..1;
    TXHALFIF : 0..1;
    TXNFULLIF : 0..1;
    RESERVED1 : 0..31;
    RXNEMPTYIE : 0..1;
    RXHALFIE : 0..1;
    RXFULLIE : 0..1;
    RXOVFLIE : 0..1;
    RESERVED2 : 0..15;
    TXEMPTYIE : 0..1;
    TXHALFIE : 0..1;
    TXNFULLIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOINT15bits: TC2FIFOINT15bits absolute ($BF88C720);
  C2FIFOINT15CLR : longWord absolute ($BF88C724);
  C2FIFOINT15SET : longWord absolute ($BF88C728);
  C2FIFOINT15INV : longWord absolute ($BF88C72C);
  C2FIFOUA15 : longWord absolute ($BF88C730);
  C2FIFOUA15CLR : longWord absolute ($BF88C734);
  C2FIFOUA15SET : longWord absolute ($BF88C738);
  C2FIFOUA15INV : longWord absolute ($BF88C73C);
  C2FIFOCI15 : longWord absolute ($BF88C740);
type
  TC2FIFOCI15bits = bitpacked record
  case integer of
  0 : (
    CFIFOCI : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOCI15bits: TC2FIFOCI15bits absolute ($BF88C740);
  C2FIFOCI15CLR : longWord absolute ($BF88C744);
  C2FIFOCI15SET : longWord absolute ($BF88C748);
  C2FIFOCI15INV : longWord absolute ($BF88C74C);
  C2FIFOCON16 : longWord absolute ($BF88C750);
type
  TC2FIFOCON16bits = bitpacked record
  case integer of
  0 : (
    TXPRI : 0..3;
    RTREN : 0..1;
    TXREQ : 0..1;
    TXERR : 0..1;
    TXLARB : 0..1;
    TXABAT : 0..1;
    TXEN : 0..1;
    RESERVED0 : 0..15;
    DONLY : 0..1;
    UINC : 0..1;
    FRESET : 0..1;
    RESERVED1 : 0..1;
    FSIZE : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOCON16bits: TC2FIFOCON16bits absolute ($BF88C750);
  C2FIFOCON16CLR : longWord absolute ($BF88C754);
  C2FIFOCON16SET : longWord absolute ($BF88C758);
  C2FIFOCON16INV : longWord absolute ($BF88C75C);
  C2FIFOINT16 : longWord absolute ($BF88C760);
type
  TC2FIFOINT16bits = bitpacked record
  case integer of
  0 : (
    RXNEMPTYIF : 0..1;
    RXHALFIF : 0..1;
    RXFULLIF : 0..1;
    RXOVFLIF : 0..1;
    RESERVED0 : 0..15;
    TXEMPTYIF : 0..1;
    TXHALFIF : 0..1;
    TXNFULLIF : 0..1;
    RESERVED1 : 0..31;
    RXNEMPTYIE : 0..1;
    RXHALFIE : 0..1;
    RXFULLIE : 0..1;
    RXOVFLIE : 0..1;
    RESERVED2 : 0..15;
    TXEMPTYIE : 0..1;
    TXHALFIE : 0..1;
    TXNFULLIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOINT16bits: TC2FIFOINT16bits absolute ($BF88C760);
  C2FIFOINT16CLR : longWord absolute ($BF88C764);
  C2FIFOINT16SET : longWord absolute ($BF88C768);
  C2FIFOINT16INV : longWord absolute ($BF88C76C);
  C2FIFOUA16 : longWord absolute ($BF88C770);
  C2FIFOUA16CLR : longWord absolute ($BF88C774);
  C2FIFOUA16SET : longWord absolute ($BF88C778);
  C2FIFOUA16INV : longWord absolute ($BF88C77C);
  C2FIFOCI16 : longWord absolute ($BF88C780);
type
  TC2FIFOCI16bits = bitpacked record
  case integer of
  0 : (
    CFIFOCI : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOCI16bits: TC2FIFOCI16bits absolute ($BF88C780);
  C2FIFOCI16CLR : longWord absolute ($BF88C784);
  C2FIFOCI16SET : longWord absolute ($BF88C788);
  C2FIFOCI16INV : longWord absolute ($BF88C78C);
  C2FIFOCON17 : longWord absolute ($BF88C790);
type
  TC2FIFOCON17bits = bitpacked record
  case integer of
  0 : (
    TXPRI : 0..3;
    RTREN : 0..1;
    TXREQ : 0..1;
    TXERR : 0..1;
    TXLARB : 0..1;
    TXABAT : 0..1;
    TXEN : 0..1;
    RESERVED0 : 0..15;
    DONLY : 0..1;
    UINC : 0..1;
    FRESET : 0..1;
    RESERVED1 : 0..1;
    FSIZE : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOCON17bits: TC2FIFOCON17bits absolute ($BF88C790);
  C2FIFOCON17CLR : longWord absolute ($BF88C794);
  C2FIFOCON17SET : longWord absolute ($BF88C798);
  C2FIFOCON17INV : longWord absolute ($BF88C79C);
  C2FIFOINT17 : longWord absolute ($BF88C7A0);
type
  TC2FIFOINT17bits = bitpacked record
  case integer of
  0 : (
    RXNEMPTYIF : 0..1;
    RXHALFIF : 0..1;
    RXFULLIF : 0..1;
    RXOVFLIF : 0..1;
    RESERVED0 : 0..15;
    TXEMPTYIF : 0..1;
    TXHALFIF : 0..1;
    TXNFULLIF : 0..1;
    RESERVED1 : 0..31;
    RXNEMPTYIE : 0..1;
    RXHALFIE : 0..1;
    RXFULLIE : 0..1;
    RXOVFLIE : 0..1;
    RESERVED2 : 0..15;
    TXEMPTYIE : 0..1;
    TXHALFIE : 0..1;
    TXNFULLIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOINT17bits: TC2FIFOINT17bits absolute ($BF88C7A0);
  C2FIFOINT17CLR : longWord absolute ($BF88C7A4);
  C2FIFOINT17SET : longWord absolute ($BF88C7A8);
  C2FIFOINT17INV : longWord absolute ($BF88C7AC);
  C2FIFOUA17 : longWord absolute ($BF88C7B0);
  C2FIFOUA17CLR : longWord absolute ($BF88C7B4);
  C2FIFOUA17SET : longWord absolute ($BF88C7B8);
  C2FIFOUA17INV : longWord absolute ($BF88C7BC);
  C2FIFOCI17 : longWord absolute ($BF88C7C0);
type
  TC2FIFOCI17bits = bitpacked record
  case integer of
  0 : (
    CFIFOCI : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOCI17bits: TC2FIFOCI17bits absolute ($BF88C7C0);
  C2FIFOCI17CLR : longWord absolute ($BF88C7C4);
  C2FIFOCI17SET : longWord absolute ($BF88C7C8);
  C2FIFOCI17INV : longWord absolute ($BF88C7CC);
  C2FIFOCON18 : longWord absolute ($BF88C7D0);
type
  TC2FIFOCON18bits = bitpacked record
  case integer of
  0 : (
    TXPRI : 0..3;
    RTREN : 0..1;
    TXREQ : 0..1;
    TXERR : 0..1;
    TXLARB : 0..1;
    TXABAT : 0..1;
    TXEN : 0..1;
    RESERVED0 : 0..15;
    DONLY : 0..1;
    UINC : 0..1;
    FRESET : 0..1;
    RESERVED1 : 0..1;
    FSIZE : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOCON18bits: TC2FIFOCON18bits absolute ($BF88C7D0);
  C2FIFOCON18CLR : longWord absolute ($BF88C7D4);
  C2FIFOCON18SET : longWord absolute ($BF88C7D8);
  C2FIFOCON18INV : longWord absolute ($BF88C7DC);
  C2FIFOINT18 : longWord absolute ($BF88C7E0);
type
  TC2FIFOINT18bits = bitpacked record
  case integer of
  0 : (
    RXNEMPTYIF : 0..1;
    RXHALFIF : 0..1;
    RXFULLIF : 0..1;
    RXOVFLIF : 0..1;
    RESERVED0 : 0..15;
    TXEMPTYIF : 0..1;
    TXHALFIF : 0..1;
    TXNFULLIF : 0..1;
    RESERVED1 : 0..31;
    RXNEMPTYIE : 0..1;
    RXHALFIE : 0..1;
    RXFULLIE : 0..1;
    RXOVFLIE : 0..1;
    RESERVED2 : 0..15;
    TXEMPTYIE : 0..1;
    TXHALFIE : 0..1;
    TXNFULLIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOINT18bits: TC2FIFOINT18bits absolute ($BF88C7E0);
  C2FIFOINT18CLR : longWord absolute ($BF88C7E4);
  C2FIFOINT18SET : longWord absolute ($BF88C7E8);
  C2FIFOINT18INV : longWord absolute ($BF88C7EC);
  C2FIFOUA18 : longWord absolute ($BF88C7F0);
  C2FIFOUA18CLR : longWord absolute ($BF88C7F4);
  C2FIFOUA18SET : longWord absolute ($BF88C7F8);
  C2FIFOUA18INV : longWord absolute ($BF88C7FC);
  C2FIFOCI18 : longWord absolute ($BF88C800);
type
  TC2FIFOCI18bits = bitpacked record
  case integer of
  0 : (
    CFIFOCI : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOCI18bits: TC2FIFOCI18bits absolute ($BF88C800);
  C2FIFOCI18CLR : longWord absolute ($BF88C804);
  C2FIFOCI18SET : longWord absolute ($BF88C808);
  C2FIFOCI18INV : longWord absolute ($BF88C80C);
  C2FIFOCON19 : longWord absolute ($BF88C810);
type
  TC2FIFOCON19bits = bitpacked record
  case integer of
  0 : (
    TXPRI : 0..3;
    RTREN : 0..1;
    TXREQ : 0..1;
    TXERR : 0..1;
    TXLARB : 0..1;
    TXABAT : 0..1;
    TXEN : 0..1;
    RESERVED0 : 0..15;
    DONLY : 0..1;
    UINC : 0..1;
    FRESET : 0..1;
    RESERVED1 : 0..1;
    FSIZE : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOCON19bits: TC2FIFOCON19bits absolute ($BF88C810);
  C2FIFOCON19CLR : longWord absolute ($BF88C814);
  C2FIFOCON19SET : longWord absolute ($BF88C818);
  C2FIFOCON19INV : longWord absolute ($BF88C81C);
  C2FIFOINT19 : longWord absolute ($BF88C820);
type
  TC2FIFOINT19bits = bitpacked record
  case integer of
  0 : (
    RXNEMPTYIF : 0..1;
    RXHALFIF : 0..1;
    RXFULLIF : 0..1;
    RXOVFLIF : 0..1;
    RESERVED0 : 0..15;
    TXEMPTYIF : 0..1;
    TXHALFIF : 0..1;
    TXNFULLIF : 0..1;
    RESERVED1 : 0..31;
    RXNEMPTYIE : 0..1;
    RXHALFIE : 0..1;
    RXFULLIE : 0..1;
    RXOVFLIE : 0..1;
    RESERVED2 : 0..15;
    TXEMPTYIE : 0..1;
    TXHALFIE : 0..1;
    TXNFULLIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOINT19bits: TC2FIFOINT19bits absolute ($BF88C820);
  C2FIFOINT19CLR : longWord absolute ($BF88C824);
  C2FIFOINT19SET : longWord absolute ($BF88C828);
  C2FIFOINT19INV : longWord absolute ($BF88C82C);
  C2FIFOUA19 : longWord absolute ($BF88C830);
  C2FIFOUA19CLR : longWord absolute ($BF88C834);
  C2FIFOUA19SET : longWord absolute ($BF88C838);
  C2FIFOUA19INV : longWord absolute ($BF88C83C);
  C2FIFOCI19 : longWord absolute ($BF88C840);
type
  TC2FIFOCI19bits = bitpacked record
  case integer of
  0 : (
    CFIFOCI : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOCI19bits: TC2FIFOCI19bits absolute ($BF88C840);
  C2FIFOCI19CLR : longWord absolute ($BF88C844);
  C2FIFOCI19SET : longWord absolute ($BF88C848);
  C2FIFOCI19INV : longWord absolute ($BF88C84C);
  C2FIFOCON20 : longWord absolute ($BF88C850);
type
  TC2FIFOCON20bits = bitpacked record
  case integer of
  0 : (
    TXPRI : 0..3;
    RTREN : 0..1;
    TXREQ : 0..1;
    TXERR : 0..1;
    TXLARB : 0..1;
    TXABAT : 0..1;
    TXEN : 0..1;
    RESERVED0 : 0..15;
    DONLY : 0..1;
    UINC : 0..1;
    FRESET : 0..1;
    RESERVED1 : 0..1;
    FSIZE : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOCON20bits: TC2FIFOCON20bits absolute ($BF88C850);
  C2FIFOCON20CLR : longWord absolute ($BF88C854);
  C2FIFOCON20SET : longWord absolute ($BF88C858);
  C2FIFOCON20INV : longWord absolute ($BF88C85C);
  C2FIFOINT20 : longWord absolute ($BF88C860);
type
  TC2FIFOINT20bits = bitpacked record
  case integer of
  0 : (
    RXNEMPTYIF : 0..1;
    RXHALFIF : 0..1;
    RXFULLIF : 0..1;
    RXOVFLIF : 0..1;
    RESERVED0 : 0..15;
    TXEMPTYIF : 0..1;
    TXHALFIF : 0..1;
    TXNFULLIF : 0..1;
    RESERVED1 : 0..31;
    RXNEMPTYIE : 0..1;
    RXHALFIE : 0..1;
    RXFULLIE : 0..1;
    RXOVFLIE : 0..1;
    RESERVED2 : 0..15;
    TXEMPTYIE : 0..1;
    TXHALFIE : 0..1;
    TXNFULLIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOINT20bits: TC2FIFOINT20bits absolute ($BF88C860);
  C2FIFOINT20CLR : longWord absolute ($BF88C864);
  C2FIFOINT20SET : longWord absolute ($BF88C868);
  C2FIFOINT20INV : longWord absolute ($BF88C86C);
  C2FIFOUA20 : longWord absolute ($BF88C870);
  C2FIFOUA20CLR : longWord absolute ($BF88C874);
  C2FIFOUA20SET : longWord absolute ($BF88C878);
  C2FIFOUA20INV : longWord absolute ($BF88C87C);
  C2FIFOCI20 : longWord absolute ($BF88C880);
type
  TC2FIFOCI20bits = bitpacked record
  case integer of
  0 : (
    CFIFOCI : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOCI20bits: TC2FIFOCI20bits absolute ($BF88C880);
  C2FIFOCI20CLR : longWord absolute ($BF88C884);
  C2FIFOCI20SET : longWord absolute ($BF88C888);
  C2FIFOCI20INV : longWord absolute ($BF88C88C);
  C2FIFOCON21 : longWord absolute ($BF88C890);
type
  TC2FIFOCON21bits = bitpacked record
  case integer of
  0 : (
    TXPRI : 0..3;
    RTREN : 0..1;
    TXREQ : 0..1;
    TXERR : 0..1;
    TXLARB : 0..1;
    TXABAT : 0..1;
    TXEN : 0..1;
    RESERVED0 : 0..15;
    DONLY : 0..1;
    UINC : 0..1;
    FRESET : 0..1;
    RESERVED1 : 0..1;
    FSIZE : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOCON21bits: TC2FIFOCON21bits absolute ($BF88C890);
  C2FIFOCON21CLR : longWord absolute ($BF88C894);
  C2FIFOCON21SET : longWord absolute ($BF88C898);
  C2FIFOCON21INV : longWord absolute ($BF88C89C);
  C2FIFOINT21 : longWord absolute ($BF88C8A0);
type
  TC2FIFOINT21bits = bitpacked record
  case integer of
  0 : (
    RXNEMPTYIF : 0..1;
    RXHALFIF : 0..1;
    RXFULLIF : 0..1;
    RXOVFLIF : 0..1;
    RESERVED0 : 0..15;
    TXEMPTYIF : 0..1;
    TXHALFIF : 0..1;
    TXNFULLIF : 0..1;
    RESERVED1 : 0..31;
    RXNEMPTYIE : 0..1;
    RXHALFIE : 0..1;
    RXFULLIE : 0..1;
    RXOVFLIE : 0..1;
    RESERVED2 : 0..15;
    TXEMPTYIE : 0..1;
    TXHALFIE : 0..1;
    TXNFULLIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOINT21bits: TC2FIFOINT21bits absolute ($BF88C8A0);
  C2FIFOINT21CLR : longWord absolute ($BF88C8A4);
  C2FIFOINT21SET : longWord absolute ($BF88C8A8);
  C2FIFOINT21INV : longWord absolute ($BF88C8AC);
  C2FIFOUA21 : longWord absolute ($BF88C8B0);
  C2FIFOUA21CLR : longWord absolute ($BF88C8B4);
  C2FIFOUA21SET : longWord absolute ($BF88C8B8);
  C2FIFOUA21INV : longWord absolute ($BF88C8BC);
  C2FIFOCI21 : longWord absolute ($BF88C8C0);
type
  TC2FIFOCI21bits = bitpacked record
  case integer of
  0 : (
    CFIFOCI : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOCI21bits: TC2FIFOCI21bits absolute ($BF88C8C0);
  C2FIFOCI21CLR : longWord absolute ($BF88C8C4);
  C2FIFOCI21SET : longWord absolute ($BF88C8C8);
  C2FIFOCI21INV : longWord absolute ($BF88C8CC);
  C2FIFOCON22 : longWord absolute ($BF88C8D0);
type
  TC2FIFOCON22bits = bitpacked record
  case integer of
  0 : (
    TXPRI : 0..3;
    RTREN : 0..1;
    TXREQ : 0..1;
    TXERR : 0..1;
    TXLARB : 0..1;
    TXABAT : 0..1;
    TXEN : 0..1;
    RESERVED0 : 0..15;
    DONLY : 0..1;
    UINC : 0..1;
    FRESET : 0..1;
    RESERVED1 : 0..1;
    FSIZE : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOCON22bits: TC2FIFOCON22bits absolute ($BF88C8D0);
  C2FIFOCON22CLR : longWord absolute ($BF88C8D4);
  C2FIFOCON22SET : longWord absolute ($BF88C8D8);
  C2FIFOCON22INV : longWord absolute ($BF88C8DC);
  C2FIFOINT22 : longWord absolute ($BF88C8E0);
type
  TC2FIFOINT22bits = bitpacked record
  case integer of
  0 : (
    RXNEMPTYIF : 0..1;
    RXHALFIF : 0..1;
    RXFULLIF : 0..1;
    RXOVFLIF : 0..1;
    RESERVED0 : 0..15;
    TXEMPTYIF : 0..1;
    TXHALFIF : 0..1;
    TXNFULLIF : 0..1;
    RESERVED1 : 0..31;
    RXNEMPTYIE : 0..1;
    RXHALFIE : 0..1;
    RXFULLIE : 0..1;
    RXOVFLIE : 0..1;
    RESERVED2 : 0..15;
    TXEMPTYIE : 0..1;
    TXHALFIE : 0..1;
    TXNFULLIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOINT22bits: TC2FIFOINT22bits absolute ($BF88C8E0);
  C2FIFOINT22CLR : longWord absolute ($BF88C8E4);
  C2FIFOINT22SET : longWord absolute ($BF88C8E8);
  C2FIFOINT22INV : longWord absolute ($BF88C8EC);
  C2FIFOUA22 : longWord absolute ($BF88C8F0);
  C2FIFOUA22CLR : longWord absolute ($BF88C8F4);
  C2FIFOUA22SET : longWord absolute ($BF88C8F8);
  C2FIFOUA22INV : longWord absolute ($BF88C8FC);
  C2FIFOCI22 : longWord absolute ($BF88C900);
type
  TC2FIFOCI22bits = bitpacked record
  case integer of
  0 : (
    CFIFOCI : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOCI22bits: TC2FIFOCI22bits absolute ($BF88C900);
  C2FIFOCI22CLR : longWord absolute ($BF88C904);
  C2FIFOCI22SET : longWord absolute ($BF88C908);
  C2FIFOCI22INV : longWord absolute ($BF88C90C);
  C2FIFOCON23 : longWord absolute ($BF88C910);
type
  TC2FIFOCON23bits = bitpacked record
  case integer of
  0 : (
    TXPRI : 0..3;
    RTREN : 0..1;
    TXREQ : 0..1;
    TXERR : 0..1;
    TXLARB : 0..1;
    TXABAT : 0..1;
    TXEN : 0..1;
    RESERVED0 : 0..15;
    DONLY : 0..1;
    UINC : 0..1;
    FRESET : 0..1;
    RESERVED1 : 0..1;
    FSIZE : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOCON23bits: TC2FIFOCON23bits absolute ($BF88C910);
  C2FIFOCON23CLR : longWord absolute ($BF88C914);
  C2FIFOCON23SET : longWord absolute ($BF88C918);
  C2FIFOCON23INV : longWord absolute ($BF88C91C);
  C2FIFOINT23 : longWord absolute ($BF88C920);
type
  TC2FIFOINT23bits = bitpacked record
  case integer of
  0 : (
    RXNEMPTYIF : 0..1;
    RXHALFIF : 0..1;
    RXFULLIF : 0..1;
    RXOVFLIF : 0..1;
    RESERVED0 : 0..15;
    TXEMPTYIF : 0..1;
    TXHALFIF : 0..1;
    TXNFULLIF : 0..1;
    RESERVED1 : 0..31;
    RXNEMPTYIE : 0..1;
    RXHALFIE : 0..1;
    RXFULLIE : 0..1;
    RXOVFLIE : 0..1;
    RESERVED2 : 0..15;
    TXEMPTYIE : 0..1;
    TXHALFIE : 0..1;
    TXNFULLIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOINT23bits: TC2FIFOINT23bits absolute ($BF88C920);
  C2FIFOINT23CLR : longWord absolute ($BF88C924);
  C2FIFOINT23SET : longWord absolute ($BF88C928);
  C2FIFOINT23INV : longWord absolute ($BF88C92C);
  C2FIFOUA23 : longWord absolute ($BF88C930);
  C2FIFOUA23CLR : longWord absolute ($BF88C934);
  C2FIFOUA23SET : longWord absolute ($BF88C938);
  C2FIFOUA23INV : longWord absolute ($BF88C93C);
  C2FIFOCI23 : longWord absolute ($BF88C940);
type
  TC2FIFOCI23bits = bitpacked record
  case integer of
  0 : (
    CFIFOCI : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOCI23bits: TC2FIFOCI23bits absolute ($BF88C940);
  C2FIFOCI23CLR : longWord absolute ($BF88C944);
  C2FIFOCI23SET : longWord absolute ($BF88C948);
  C2FIFOCI23INV : longWord absolute ($BF88C94C);
  C2FIFOCON24 : longWord absolute ($BF88C950);
type
  TC2FIFOCON24bits = bitpacked record
  case integer of
  0 : (
    TXPRI : 0..3;
    RTREN : 0..1;
    TXREQ : 0..1;
    TXERR : 0..1;
    TXLARB : 0..1;
    TXABAT : 0..1;
    TXEN : 0..1;
    RESERVED0 : 0..15;
    DONLY : 0..1;
    UINC : 0..1;
    FRESET : 0..1;
    RESERVED1 : 0..1;
    FSIZE : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOCON24bits: TC2FIFOCON24bits absolute ($BF88C950);
  C2FIFOCON24CLR : longWord absolute ($BF88C954);
  C2FIFOCON24SET : longWord absolute ($BF88C958);
  C2FIFOCON24INV : longWord absolute ($BF88C95C);
  C2FIFOINT24 : longWord absolute ($BF88C960);
type
  TC2FIFOINT24bits = bitpacked record
  case integer of
  0 : (
    RXNEMPTYIF : 0..1;
    RXHALFIF : 0..1;
    RXFULLIF : 0..1;
    RXOVFLIF : 0..1;
    RESERVED0 : 0..15;
    TXEMPTYIF : 0..1;
    TXHALFIF : 0..1;
    TXNFULLIF : 0..1;
    RESERVED1 : 0..31;
    RXNEMPTYIE : 0..1;
    RXHALFIE : 0..1;
    RXFULLIE : 0..1;
    RXOVFLIE : 0..1;
    RESERVED2 : 0..15;
    TXEMPTYIE : 0..1;
    TXHALFIE : 0..1;
    TXNFULLIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOINT24bits: TC2FIFOINT24bits absolute ($BF88C960);
  C2FIFOINT24CLR : longWord absolute ($BF88C964);
  C2FIFOINT24SET : longWord absolute ($BF88C968);
  C2FIFOINT24INV : longWord absolute ($BF88C96C);
  C2FIFOUA24 : longWord absolute ($BF88C970);
  C2FIFOUA24CLR : longWord absolute ($BF88C974);
  C2FIFOUA24SET : longWord absolute ($BF88C978);
  C2FIFOUA24INV : longWord absolute ($BF88C97C);
  C2FIFOCI24 : longWord absolute ($BF88C980);
type
  TC2FIFOCI24bits = bitpacked record
  case integer of
  0 : (
    CFIFOCI : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOCI24bits: TC2FIFOCI24bits absolute ($BF88C980);
  C2FIFOCI24CLR : longWord absolute ($BF88C984);
  C2FIFOCI24SET : longWord absolute ($BF88C988);
  C2FIFOCI24INV : longWord absolute ($BF88C98C);
  C2FIFOCON25 : longWord absolute ($BF88C990);
type
  TC2FIFOCON25bits = bitpacked record
  case integer of
  0 : (
    TXPRI : 0..3;
    RTREN : 0..1;
    TXREQ : 0..1;
    TXERR : 0..1;
    TXLARB : 0..1;
    TXABAT : 0..1;
    TXEN : 0..1;
    RESERVED0 : 0..15;
    DONLY : 0..1;
    UINC : 0..1;
    FRESET : 0..1;
    RESERVED1 : 0..1;
    FSIZE : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOCON25bits: TC2FIFOCON25bits absolute ($BF88C990);
  C2FIFOCON25CLR : longWord absolute ($BF88C994);
  C2FIFOCON25SET : longWord absolute ($BF88C998);
  C2FIFOCON25INV : longWord absolute ($BF88C99C);
  C2FIFOINT25 : longWord absolute ($BF88C9A0);
type
  TC2FIFOINT25bits = bitpacked record
  case integer of
  0 : (
    RXNEMPTYIF : 0..1;
    RXHALFIF : 0..1;
    RXFULLIF : 0..1;
    RXOVFLIF : 0..1;
    RESERVED0 : 0..15;
    TXEMPTYIF : 0..1;
    TXHALFIF : 0..1;
    TXNFULLIF : 0..1;
    RESERVED1 : 0..31;
    RXNEMPTYIE : 0..1;
    RXHALFIE : 0..1;
    RXFULLIE : 0..1;
    RXOVFLIE : 0..1;
    RESERVED2 : 0..15;
    TXEMPTYIE : 0..1;
    TXHALFIE : 0..1;
    TXNFULLIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOINT25bits: TC2FIFOINT25bits absolute ($BF88C9A0);
  C2FIFOINT25CLR : longWord absolute ($BF88C9A4);
  C2FIFOINT25SET : longWord absolute ($BF88C9A8);
  C2FIFOINT25INV : longWord absolute ($BF88C9AC);
  C2FIFOUA25 : longWord absolute ($BF88C9B0);
  C2FIFOUA25CLR : longWord absolute ($BF88C9B4);
  C2FIFOUA25SET : longWord absolute ($BF88C9B8);
  C2FIFOUA25INV : longWord absolute ($BF88C9BC);
  C2FIFOCI25 : longWord absolute ($BF88C9C0);
type
  TC2FIFOCI25bits = bitpacked record
  case integer of
  0 : (
    CFIFOCI : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOCI25bits: TC2FIFOCI25bits absolute ($BF88C9C0);
  C2FIFOCI25CLR : longWord absolute ($BF88C9C4);
  C2FIFOCI25SET : longWord absolute ($BF88C9C8);
  C2FIFOCI25INV : longWord absolute ($BF88C9CC);
  C2FIFOCON26 : longWord absolute ($BF88C9D0);
type
  TC2FIFOCON26bits = bitpacked record
  case integer of
  0 : (
    TXPRI : 0..3;
    RTREN : 0..1;
    TXREQ : 0..1;
    TXERR : 0..1;
    TXLARB : 0..1;
    TXABAT : 0..1;
    TXEN : 0..1;
    RESERVED0 : 0..15;
    DONLY : 0..1;
    UINC : 0..1;
    FRESET : 0..1;
    RESERVED1 : 0..1;
    FSIZE : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOCON26bits: TC2FIFOCON26bits absolute ($BF88C9D0);
  C2FIFOCON26CLR : longWord absolute ($BF88C9D4);
  C2FIFOCON26SET : longWord absolute ($BF88C9D8);
  C2FIFOCON26INV : longWord absolute ($BF88C9DC);
  C2FIFOINT26 : longWord absolute ($BF88C9E0);
type
  TC2FIFOINT26bits = bitpacked record
  case integer of
  0 : (
    RXNEMPTYIF : 0..1;
    RXHALFIF : 0..1;
    RXFULLIF : 0..1;
    RXOVFLIF : 0..1;
    RESERVED0 : 0..15;
    TXEMPTYIF : 0..1;
    TXHALFIF : 0..1;
    TXNFULLIF : 0..1;
    RESERVED1 : 0..31;
    RXNEMPTYIE : 0..1;
    RXHALFIE : 0..1;
    RXFULLIE : 0..1;
    RXOVFLIE : 0..1;
    RESERVED2 : 0..15;
    TXEMPTYIE : 0..1;
    TXHALFIE : 0..1;
    TXNFULLIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOINT26bits: TC2FIFOINT26bits absolute ($BF88C9E0);
  C2FIFOINT26CLR : longWord absolute ($BF88C9E4);
  C2FIFOINT26SET : longWord absolute ($BF88C9E8);
  C2FIFOINT26INV : longWord absolute ($BF88C9EC);
  C2FIFOUA26 : longWord absolute ($BF88C9F0);
  C2FIFOUA26CLR : longWord absolute ($BF88C9F4);
  C2FIFOUA26SET : longWord absolute ($BF88C9F8);
  C2FIFOUA26INV : longWord absolute ($BF88C9FC);
  C2FIFOCI26 : longWord absolute ($BF88CA00);
type
  TC2FIFOCI26bits = bitpacked record
  case integer of
  0 : (
    CFIFOCI : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOCI26bits: TC2FIFOCI26bits absolute ($BF88CA00);
  C2FIFOCI26CLR : longWord absolute ($BF88CA04);
  C2FIFOCI26SET : longWord absolute ($BF88CA08);
  C2FIFOCI26INV : longWord absolute ($BF88CA0C);
  C2FIFOCON27 : longWord absolute ($BF88CA10);
type
  TC2FIFOCON27bits = bitpacked record
  case integer of
  0 : (
    TXPRI : 0..3;
    RTREN : 0..1;
    TXREQ : 0..1;
    TXERR : 0..1;
    TXLARB : 0..1;
    TXABAT : 0..1;
    TXEN : 0..1;
    RESERVED0 : 0..15;
    DONLY : 0..1;
    UINC : 0..1;
    FRESET : 0..1;
    RESERVED1 : 0..1;
    FSIZE : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOCON27bits: TC2FIFOCON27bits absolute ($BF88CA10);
  C2FIFOCON27CLR : longWord absolute ($BF88CA14);
  C2FIFOCON27SET : longWord absolute ($BF88CA18);
  C2FIFOCON27INV : longWord absolute ($BF88CA1C);
  C2FIFOINT27 : longWord absolute ($BF88CA20);
type
  TC2FIFOINT27bits = bitpacked record
  case integer of
  0 : (
    RXNEMPTYIF : 0..1;
    RXHALFIF : 0..1;
    RXFULLIF : 0..1;
    RXOVFLIF : 0..1;
    RESERVED0 : 0..15;
    TXEMPTYIF : 0..1;
    TXHALFIF : 0..1;
    TXNFULLIF : 0..1;
    RESERVED1 : 0..31;
    RXNEMPTYIE : 0..1;
    RXHALFIE : 0..1;
    RXFULLIE : 0..1;
    RXOVFLIE : 0..1;
    RESERVED2 : 0..15;
    TXEMPTYIE : 0..1;
    TXHALFIE : 0..1;
    TXNFULLIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOINT27bits: TC2FIFOINT27bits absolute ($BF88CA20);
  C2FIFOINT27CLR : longWord absolute ($BF88CA24);
  C2FIFOINT27SET : longWord absolute ($BF88CA28);
  C2FIFOINT27INV : longWord absolute ($BF88CA2C);
  C2FIFOUA27 : longWord absolute ($BF88CA30);
  C2FIFOUA27CLR : longWord absolute ($BF88CA34);
  C2FIFOUA27SET : longWord absolute ($BF88CA38);
  C2FIFOUA27INV : longWord absolute ($BF88CA3C);
  C2FIFOCI27 : longWord absolute ($BF88CA40);
type
  TC2FIFOCI27bits = bitpacked record
  case integer of
  0 : (
    CFIFOCI : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOCI27bits: TC2FIFOCI27bits absolute ($BF88CA40);
  C2FIFOCI27CLR : longWord absolute ($BF88CA44);
  C2FIFOCI27SET : longWord absolute ($BF88CA48);
  C2FIFOCI27INV : longWord absolute ($BF88CA4C);
  C2FIFOCON28 : longWord absolute ($BF88CA50);
type
  TC2FIFOCON28bits = bitpacked record
  case integer of
  0 : (
    TXPRI : 0..3;
    RTREN : 0..1;
    TXREQ : 0..1;
    TXERR : 0..1;
    TXLARB : 0..1;
    TXABAT : 0..1;
    TXEN : 0..1;
    RESERVED0 : 0..15;
    DONLY : 0..1;
    UINC : 0..1;
    FRESET : 0..1;
    RESERVED1 : 0..1;
    FSIZE : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOCON28bits: TC2FIFOCON28bits absolute ($BF88CA50);
  C2FIFOCON28CLR : longWord absolute ($BF88CA54);
  C2FIFOCON28SET : longWord absolute ($BF88CA58);
  C2FIFOCON28INV : longWord absolute ($BF88CA5C);
  C2FIFOINT28 : longWord absolute ($BF88CA60);
type
  TC2FIFOINT28bits = bitpacked record
  case integer of
  0 : (
    RXNEMPTYIF : 0..1;
    RXHALFIF : 0..1;
    RXFULLIF : 0..1;
    RXOVFLIF : 0..1;
    RESERVED0 : 0..15;
    TXEMPTYIF : 0..1;
    TXHALFIF : 0..1;
    TXNFULLIF : 0..1;
    RESERVED1 : 0..31;
    RXNEMPTYIE : 0..1;
    RXHALFIE : 0..1;
    RXFULLIE : 0..1;
    RXOVFLIE : 0..1;
    RESERVED2 : 0..15;
    TXEMPTYIE : 0..1;
    TXHALFIE : 0..1;
    TXNFULLIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOINT28bits: TC2FIFOINT28bits absolute ($BF88CA60);
  C2FIFOINT28CLR : longWord absolute ($BF88CA64);
  C2FIFOINT28SET : longWord absolute ($BF88CA68);
  C2FIFOINT28INV : longWord absolute ($BF88CA6C);
  C2FIFOUA28 : longWord absolute ($BF88CA70);
  C2FIFOUA28CLR : longWord absolute ($BF88CA74);
  C2FIFOUA28SET : longWord absolute ($BF88CA78);
  C2FIFOUA28INV : longWord absolute ($BF88CA7C);
  C2FIFOCI28 : longWord absolute ($BF88CA80);
type
  TC2FIFOCI28bits = bitpacked record
  case integer of
  0 : (
    CFIFOCI : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOCI28bits: TC2FIFOCI28bits absolute ($BF88CA80);
  C2FIFOCI28CLR : longWord absolute ($BF88CA84);
  C2FIFOCI28SET : longWord absolute ($BF88CA88);
  C2FIFOCI28INV : longWord absolute ($BF88CA8C);
  C2FIFOCON29 : longWord absolute ($BF88CA90);
type
  TC2FIFOCON29bits = bitpacked record
  case integer of
  0 : (
    TXPRI : 0..3;
    RTREN : 0..1;
    TXREQ : 0..1;
    TXERR : 0..1;
    TXLARB : 0..1;
    TXABAT : 0..1;
    TXEN : 0..1;
    RESERVED0 : 0..15;
    DONLY : 0..1;
    UINC : 0..1;
    FRESET : 0..1;
    RESERVED1 : 0..1;
    FSIZE : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOCON29bits: TC2FIFOCON29bits absolute ($BF88CA90);
  C2FIFOCON29CLR : longWord absolute ($BF88CA94);
  C2FIFOCON29SET : longWord absolute ($BF88CA98);
  C2FIFOCON29INV : longWord absolute ($BF88CA9C);
  C2FIFOINT29 : longWord absolute ($BF88CAA0);
type
  TC2FIFOINT29bits = bitpacked record
  case integer of
  0 : (
    RXNEMPTYIF : 0..1;
    RXHALFIF : 0..1;
    RXFULLIF : 0..1;
    RXOVFLIF : 0..1;
    RESERVED0 : 0..15;
    TXEMPTYIF : 0..1;
    TXHALFIF : 0..1;
    TXNFULLIF : 0..1;
    RESERVED1 : 0..31;
    RXNEMPTYIE : 0..1;
    RXHALFIE : 0..1;
    RXFULLIE : 0..1;
    RXOVFLIE : 0..1;
    RESERVED2 : 0..15;
    TXEMPTYIE : 0..1;
    TXHALFIE : 0..1;
    TXNFULLIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOINT29bits: TC2FIFOINT29bits absolute ($BF88CAA0);
  C2FIFOINT29CLR : longWord absolute ($BF88CAA4);
  C2FIFOINT29SET : longWord absolute ($BF88CAA8);
  C2FIFOINT29INV : longWord absolute ($BF88CAAC);
  C2FIFOUA29 : longWord absolute ($BF88CAB0);
  C2FIFOUA29CLR : longWord absolute ($BF88CAB4);
  C2FIFOUA29SET : longWord absolute ($BF88CAB8);
  C2FIFOUA29INV : longWord absolute ($BF88CABC);
  C2FIFOCI29 : longWord absolute ($BF88CAC0);
type
  TC2FIFOCI29bits = bitpacked record
  case integer of
  0 : (
    CFIFOCI : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOCI29bits: TC2FIFOCI29bits absolute ($BF88CAC0);
  C2FIFOCI29CLR : longWord absolute ($BF88CAC4);
  C2FIFOCI29SET : longWord absolute ($BF88CAC8);
  C2FIFOCI29INV : longWord absolute ($BF88CACC);
  C2FIFOCON30 : longWord absolute ($BF88CAD0);
type
  TC2FIFOCON30bits = bitpacked record
  case integer of
  0 : (
    TXPRI : 0..3;
    RTREN : 0..1;
    TXREQ : 0..1;
    TXERR : 0..1;
    TXLARB : 0..1;
    TXABAT : 0..1;
    TXEN : 0..1;
    RESERVED0 : 0..15;
    DONLY : 0..1;
    UINC : 0..1;
    FRESET : 0..1;
    RESERVED1 : 0..1;
    FSIZE : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOCON30bits: TC2FIFOCON30bits absolute ($BF88CAD0);
  C2FIFOCON30CLR : longWord absolute ($BF88CAD4);
  C2FIFOCON30SET : longWord absolute ($BF88CAD8);
  C2FIFOCON30INV : longWord absolute ($BF88CADC);
  C2FIFOINT30 : longWord absolute ($BF88CAE0);
type
  TC2FIFOINT30bits = bitpacked record
  case integer of
  0 : (
    RXNEMPTYIF : 0..1;
    RXHALFIF : 0..1;
    RXFULLIF : 0..1;
    RXOVFLIF : 0..1;
    RESERVED0 : 0..15;
    TXEMPTYIF : 0..1;
    TXHALFIF : 0..1;
    TXNFULLIF : 0..1;
    RESERVED1 : 0..31;
    RXNEMPTYIE : 0..1;
    RXHALFIE : 0..1;
    RXFULLIE : 0..1;
    RXOVFLIE : 0..1;
    RESERVED2 : 0..15;
    TXEMPTYIE : 0..1;
    TXHALFIE : 0..1;
    TXNFULLIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOINT30bits: TC2FIFOINT30bits absolute ($BF88CAE0);
  C2FIFOINT30CLR : longWord absolute ($BF88CAE4);
  C2FIFOINT30SET : longWord absolute ($BF88CAE8);
  C2FIFOINT30INV : longWord absolute ($BF88CAEC);
  C2FIFOUA30 : longWord absolute ($BF88CAF0);
  C2FIFOUA30CLR : longWord absolute ($BF88CAF4);
  C2FIFOUA30SET : longWord absolute ($BF88CAF8);
  C2FIFOUA30INV : longWord absolute ($BF88CAFC);
  C2FIFOCI30 : longWord absolute ($BF88CB00);
type
  TC2FIFOCI30bits = bitpacked record
  case integer of
  0 : (
    CFIFOCI : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOCI30bits: TC2FIFOCI30bits absolute ($BF88CB00);
  C2FIFOCI30CLR : longWord absolute ($BF88CB04);
  C2FIFOCI30SET : longWord absolute ($BF88CB08);
  C2FIFOCI30INV : longWord absolute ($BF88CB0C);
  C2FIFOCON31 : longWord absolute ($BF88CB10);
type
  TC2FIFOCON31bits = bitpacked record
  case integer of
  0 : (
    TXPRI : 0..3;
    RTREN : 0..1;
    TXREQ : 0..1;
    TXERR : 0..1;
    TXLARB : 0..1;
    TXABAT : 0..1;
    TXEN : 0..1;
    RESERVED0 : 0..15;
    DONLY : 0..1;
    UINC : 0..1;
    FRESET : 0..1;
    RESERVED1 : 0..1;
    FSIZE : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOCON31bits: TC2FIFOCON31bits absolute ($BF88CB10);
  C2FIFOCON31CLR : longWord absolute ($BF88CB14);
  C2FIFOCON31SET : longWord absolute ($BF88CB18);
  C2FIFOCON31INV : longWord absolute ($BF88CB1C);
  C2FIFOINT31 : longWord absolute ($BF88CB20);
type
  TC2FIFOINT31bits = bitpacked record
  case integer of
  0 : (
    RXNEMPTYIF : 0..1;
    RXHALFIF : 0..1;
    RXFULLIF : 0..1;
    RXOVFLIF : 0..1;
    RESERVED0 : 0..15;
    TXEMPTYIF : 0..1;
    TXHALFIF : 0..1;
    TXNFULLIF : 0..1;
    RESERVED1 : 0..31;
    RXNEMPTYIE : 0..1;
    RXHALFIE : 0..1;
    RXFULLIE : 0..1;
    RXOVFLIE : 0..1;
    RESERVED2 : 0..15;
    TXEMPTYIE : 0..1;
    TXHALFIE : 0..1;
    TXNFULLIE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOINT31bits: TC2FIFOINT31bits absolute ($BF88CB20);
  C2FIFOINT31CLR : longWord absolute ($BF88CB24);
  C2FIFOINT31SET : longWord absolute ($BF88CB28);
  C2FIFOINT31INV : longWord absolute ($BF88CB2C);
  C2FIFOUA31 : longWord absolute ($BF88CB30);
  C2FIFOUA31CLR : longWord absolute ($BF88CB34);
  C2FIFOUA31SET : longWord absolute ($BF88CB38);
  C2FIFOUA31INV : longWord absolute ($BF88CB3C);
  C2FIFOCI31 : longWord absolute ($BF88CB40);
type
  TC2FIFOCI31bits = bitpacked record
  case integer of
  0 : (
    CFIFOCI : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  C2FIFOCI31bits: TC2FIFOCI31bits absolute ($BF88CB40);
  C2FIFOCI31CLR : longWord absolute ($BF88CB44);
  C2FIFOCI31SET : longWord absolute ($BF88CB48);
  C2FIFOCI31INV : longWord absolute ($BF88CB4C);
  DEVCFG3 : longWord absolute ($BFC02FF0);
type
  TDEVCFG3bits = bitpacked record
  case integer of
  0 : (
    USERID : 0..65535;
    FSRSSEL : 0..7;
    RESERVED0 : 0..31;
    FMIIEN : 0..1;
    FETHIO : 0..1;
    FCANIO : 0..1;
    RESERVED1 : 0..7;
    FUSBIDIO : 0..1;
    FVBUSONIO : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  DEVCFG3bits: TDEVCFG3bits absolute ($BFC02FF0);
  DEVCFG2 : longWord absolute ($BFC02FF4);
type
  TDEVCFG2bits = bitpacked record
  case integer of
  0 : (
    FPLLIDIV : 0..7;
    RESERVED0 : 0..1;
    FPLLMUL : 0..7;
    RESERVED1 : 0..1;
    UPLLIDIV : 0..7;
    RESERVED2 : 0..15;
    UPLLEN : 0..1;
    FPLLODIV : 0..7;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  DEVCFG2bits: TDEVCFG2bits absolute ($BFC02FF4);
  DEVCFG1 : longWord absolute ($BFC02FF8);
type
  TDEVCFG1bits = bitpacked record
  case integer of
  0 : (
    FNOSC : 0..7;
    RESERVED0 : 0..3;
    FSOSCEN : 0..1;
    RESERVED1 : 0..1;
    IESO : 0..1;
    POSCMOD : 0..3;
    OSCIOFNC : 0..1;
    RESERVED2 : 0..1;
    FPBDIV : 0..3;
    FCKSM : 0..3;
    WDTPS : 0..31;
    RESERVED3 : 0..3;
    FWDTEN : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  DEVCFG1bits: TDEVCFG1bits absolute ($BFC02FF8);
  DEVCFG0 : longWord absolute ($BFC02FFC);
type
  TDEVCFG0bits = bitpacked record
  case integer of
  0 : (
    DEBUG : 0..3;
    RESERVED0 : 0..1;
    ICESEL : 0..1;
    RESERVED1 : 0..255;
    PWP : 0..255;
    RESERVED2 : 0..15;
    BWP : 0..1;
    RESERVED3 : 0..7;
    CP : 0..1;
    RESERVED4 : 0..3;
    RESERVED5 : 0..1;
  );
  1 : (
    FDEBUG : 0..3;
  );
  2 : (
    w : 0..4294967295;
  );
  end;
var
  DEVCFG0bits: TDEVCFG0bits absolute ($BFC02FFC);
(* Vector Numbers *)
const
CORE_TIMER_VECTOR = 0;
CORE_SOFTWARE_0_VECTOR = 1;
CORE_SOFTWARE_1_VECTOR = 2;
EXTERNAL_0_VECTOR = 3;
TIMER_1_VECTOR = 4;
INPUT_CAPTURE_1_VECTOR = 5;
OUTPUT_COMPARE_1_VECTOR = 6;
EXTERNAL_1_VECTOR = 7;
TIMER_2_VECTOR = 8;
INPUT_CAPTURE_2_VECTOR = 9;
OUTPUT_COMPARE_2_VECTOR = 10;
EXTERNAL_2_VECTOR = 11;
TIMER_3_VECTOR = 12;
INPUT_CAPTURE_3_VECTOR = 13;
OUTPUT_COMPARE_3_VECTOR = 14;
EXTERNAL_3_VECTOR = 15;
TIMER_4_VECTOR = 16;
INPUT_CAPTURE_4_VECTOR = 17;
OUTPUT_COMPARE_4_VECTOR = 18;
EXTERNAL_4_VECTOR = 19;
TIMER_5_VECTOR = 20;
INPUT_CAPTURE_5_VECTOR = 21;
OUTPUT_COMPARE_5_VECTOR = 22;
SPI_1_VECTOR = 23;
I2C_3_VECTOR = 24;
I2C_1A_VECTOR = 24;
SPI_3_VECTOR = 24;
SPI_1A_VECTOR = 24;
UART_1_VECTOR = 24;
UART_1A_VECTOR = 24;
I2C_1_VECTOR = 25;
CHANGE_NOTICE_VECTOR = 26;
ADC_VECTOR = 27;
PMP_VECTOR = 28;
COMPARATOR_1_VECTOR = 29;
COMPARATOR_2_VECTOR = 30;
I2C_4_VECTOR = 31;
I2C_2A_VECTOR = 31;
SPI_2_VECTOR = 31;
SPI_2A_VECTOR = 31;
UART_3_VECTOR = 31;
UART_2A_VECTOR = 31;
I2C_5_VECTOR = 32;
I2C_3A_VECTOR = 32;
SPI_4_VECTOR = 32;
SPI_3A_VECTOR = 32;
UART_2_VECTOR = 32;
UART_3A_VECTOR = 32;
I2C_2_VECTOR = 33;
FAIL_SAFE_MONITOR_VECTOR = 34;
RTCC_VECTOR = 35;
DMA_0_VECTOR = 36;
DMA_1_VECTOR = 37;
DMA_2_VECTOR = 38;
DMA_3_VECTOR = 39;
DMA_4_VECTOR = 40;
DMA_5_VECTOR = 41;
DMA_6_VECTOR = 42;
DMA_7_VECTOR = 43;
FCE_VECTOR = 44;
USB_1_VECTOR = 45;
CAN_1_VECTOR = 46;
CAN_2_VECTOR = 47;
ETH_VECTOR = 48;
UART_1B_VECTOR = 49;
UART_4_VECTOR = 49;
UART_6_VECTOR = 50;
UART_2B_VECTOR = 50;
UART_5_VECTOR = 51;
UART_3B_VECTOR = 51;
(* IRQ Numbers *)
CORE_TIMER_IRQ = 0;
CORE_SOFTWARE_0_IRQ = 1;
CORE_SOFTWARE_1_IRQ = 2;
EXTERNAL_0_IRQ = 3;
TIMER_1_IRQ = 4;
INPUT_CAPTURE_1_IRQ = 5;
OUTPUT_COMPARE_1_IRQ = 6;
EXTERNAL_1_IRQ = 7;
TIMER_2_IRQ = 8;
INPUT_CAPTURE_2_IRQ = 9;
OUTPUT_COMPARE_2_IRQ = 10;
EXTERNAL_2_IRQ = 11;
TIMER_3_IRQ = 12;
INPUT_CAPTURE_3_IRQ = 13;
OUTPUT_COMPARE_3_IRQ = 14;
EXTERNAL_3_IRQ = 15;
TIMER_4_IRQ = 16;
INPUT_CAPTURE_4_IRQ = 17;
OUTPUT_COMPARE_4_IRQ = 18;
EXTERNAL_4_IRQ = 19;
TIMER_5_IRQ = 20;
INPUT_CAPTURE_5_IRQ = 21;
OUTPUT_COMPARE_5_IRQ = 22;
SPI1_ERR_IRQ = 23;
SPI1_RX_IRQ = 24;
SPI1_TX_IRQ = 25;
I2C1A_ERR_IRQ = 26;
I2C3_BUS_IRQ = 26;
SPI1A_ERR_IRQ = 26;
SPI3_ERR_IRQ = 26;
UART1A_ERR_IRQ = 26;
UART1_ERR_IRQ = 26;
I2C1A_RX_IRQ = 27;
I2C3_SLAVE_IRQ = 27;
SPI1A_RX_IRQ = 27;
SPI3_RX_IRQ = 27;
UART1A_RX_IRQ = 27;
UART1_RX_IRQ = 27;
I2C1A_TX_IRQ = 28;
I2C3_MASTER_IRQ = 28;
SPI1A_TX_IRQ = 28;
SPI3_TX_IRQ = 28;
UART1A_TX_IRQ = 28;
UART1_TX_IRQ = 28;
I2C1_BUS_IRQ = 29;
I2C1_SLAVE_IRQ = 30;
I2C1_MASTER_IRQ = 31;
CHANGE_NOTICE_IRQ = 32;
ADC_IRQ = 33;
PMP_IRQ = 34;
COMPARATOR_1_IRQ = 35;
COMPARATOR_2_IRQ = 36;
I2C2A_ERR_IRQ = 37;
I2C4_BUS_IRQ = 37;
SPI2_ERR_IRQ = 37;
SPI2A_ERR_IRQ = 37;
UART2A_ERR_IRQ = 37;
UART3_ERR_IRQ = 37;
I2C2A_RX_IRQ = 38;
I2C4_SLAVE_IRQ = 38;
SPI2_RX_IRQ = 38;
SPI2A_RX_IRQ = 38;
UART2A_RX_IRQ = 38;
UART3_RX_IRQ = 38;
I2C2A_TX_IRQ = 39;
I2C4_MASTER_IRQ = 39;
SPI2A_TX_IRQ = 39;
SPI2_TX_IRQ = 39;
UART2A_TX_IRQ = 39;
UART3_TX_IRQ = 39;
I2C3A_ERR_IRQ = 40;
I2C5_BUS_IRQ = 40;
SPI3A_ERR_IRQ = 40;
SPI4_ERR_IRQ = 40;
UART2_ERR_IRQ = 40;
UART3A_ERR_IRQ = 40;
I2C3A_RX_IRQ = 41;
I2C5_SLAVE_IRQ = 41;
SPI3A_RX_IRQ = 41;
SPI4_RX_IRQ = 41;
UART2_RX_IRQ = 41;
UART3A_RX_IRQ = 41;
I2C3A_TX_IRQ = 42;
I2C5_MASTER_IRQ = 42;
SPI3A_TX_IRQ = 42;
SPI4_TX_IRQ = 42;
UART2_TX_IRQ = 42;
UART3A_TX_IRQ = 42;
I2C2_BUS_IRQ = 43;
I2C2_SLAVE_IRQ = 44;
I2C2_MASTER_IRQ = 45;
FAIL_SAFE_MONITOR_IRQ = 46;
RTCC_IRQ = 47;
DMA0_IRQ = 48;
DMA1_IRQ = 49;
DMA2_IRQ = 50;
DMA3_IRQ = 51;
DMA4_IRQ = 52;
DMA5_IRQ = 53;
DMA6_IRQ = 54;
DMA7_IRQ = 55;
FLASH_CONTROL_IRQ = 56;
USB_IRQ = 57;
CAN1_IRQ = 58;
CAN2_IRQ = 59;
ETHERNET_IRQ = 60;
INPUT_CAPTURE_ERROR_1_IRQ = 61;
INPUT_CAPTURE_ERROR_2_IRQ = 62;
INPUT_CAPTURE_ERROR_3_IRQ = 63;
INPUT_CAPTURE_ERROR_4_IRQ = 64;
INPUT_CAPTURE_ERROR_5_IRQ = 65;
PMP_ERROR_IRQ = 66;
UART1B_ERR_IRQ = 67;
UART4_ERR_IRQ = 67;
UART1B_RX_IRQ = 68;
UART4_RX_IRQ = 68;
UART1B_TX_IRQ = 69;
UART4_TX_IRQ = 69;
UART2B_ERR_IRQ = 70;
UART6_ERR_IRQ = 70;
UART2B_RX_IRQ = 71;
UART6_RX_IRQ = 71;
UART2B_TX_IRQ = 72;
UART6_TX_IRQ = 72;
UART3B_ERR_IRQ = 73;
UART5_ERR_IRQ = 73;
UART3B_RX_IRQ = 74;
UART5_RX_IRQ = 74;
UART3B_TX_IRQ = 75;
UART5_TX_IRQ = 75;
(* Device Peripherals *)
(* Base Addresses for Peripherals *)
ADC10_BASE_ADDRESS = $BF809000;
_APPI_BASE_ADDRESS = $BF880190;
_APPO_BASE_ADDRESS = $BF880180;
BMX_BASE_ADDRESS = $BF882000;
CAN1_BASE_ADDRESS = $BF88B000;
CAN2_BASE_ADDRESS = $BF88C000;
CFG_BASE_ADDRESS = $BF80F200;
CMP_BASE_ADDRESS = $BF80A000;
CVR_BASE_ADDRESS = $BF809800;
_DDPSTAT_BASE_ADDRESS = $BF880140;
DMAC_BASE_ADDRESS = $BF883000;
DMAC0_BASE_ADDRESS = $BF883060;
DMAC1_BASE_ADDRESS = $BF883120;
DMAC2_BASE_ADDRESS = $BF8831E0;
DMAC3_BASE_ADDRESS = $BF8832A0;
DMAC4_BASE_ADDRESS = $BF883360;
DMAC5_BASE_ADDRESS = $BF883420;
DMAC6_BASE_ADDRESS = $BF8834E0;
DMAC7_BASE_ADDRESS = $BF8835A0;
ETH_BASE_ADDRESS = $BF889000;
I2C1_BASE_ADDRESS = $BF805300;
I2C1A_BASE_ADDRESS = $BF805000;
I2C2_BASE_ADDRESS = $BF805400;
I2C2A_BASE_ADDRESS = $BF805100;
I2C3_BASE_ADDRESS = $BF805000;
I2C3A_BASE_ADDRESS = $BF805200;
I2C4_BASE_ADDRESS = $BF805100;
I2C5_BASE_ADDRESS = $BF805200;
ICAP1_BASE_ADDRESS = $BF802000;
ICAP2_BASE_ADDRESS = $BF802200;
ICAP3_BASE_ADDRESS = $BF802400;
ICAP4_BASE_ADDRESS = $BF802600;
ICAP5_BASE_ADDRESS = $BF802800;
INT_BASE_ADDRESS = $BF881000;
NVM_BASE_ADDRESS = $BF80F400;
OCMP1_BASE_ADDRESS = $BF803000;
OCMP2_BASE_ADDRESS = $BF803200;
OCMP3_BASE_ADDRESS = $BF803400;
OCMP4_BASE_ADDRESS = $BF803600;
OCMP5_BASE_ADDRESS = $BF803800;
OSC_BASE_ADDRESS = $BF80F000;
PCACHE_BASE_ADDRESS = $BF884000;
PMP_BASE_ADDRESS = $BF807000;
PORTA_BASE_ADDRESS = $BF886000;
PORTB_BASE_ADDRESS = $BF886040;
PORTC_BASE_ADDRESS = $BF886080;
PORTD_BASE_ADDRESS = $BF8860C0;
PORTE_BASE_ADDRESS = $BF886100;
PORTF_BASE_ADDRESS = $BF886140;
PORTG_BASE_ADDRESS = $BF886180;
RCON_BASE_ADDRESS = $BF80F600;
RTCC_BASE_ADDRESS = $BF800200;
SPI1_BASE_ADDRESS = $BF805E00;
SPI1A_BASE_ADDRESS = $BF805800;
SPI2_BASE_ADDRESS = $BF805A00;
SPI2A_BASE_ADDRESS = $BF805A00;
SPI3_BASE_ADDRESS = $BF805800;
SPI3A_BASE_ADDRESS = $BF805C00;
SPI4_BASE_ADDRESS = $BF805C00;
_STRO_BASE_ADDRESS = $BF880170;
TMR1_BASE_ADDRESS = $BF800600;
TMR2_BASE_ADDRESS = $BF800800;
TMR23_BASE_ADDRESS = $BF800800;
TMR3_BASE_ADDRESS = $BF800A00;
TMR4_BASE_ADDRESS = $BF800C00;
TMR45_BASE_ADDRESS = $BF800C00;
TMR5_BASE_ADDRESS = $BF800E00;
UART1_BASE_ADDRESS = $BF806000;
UART1A_BASE_ADDRESS = $BF806000;
UART1B_BASE_ADDRESS = $BF806200;
UART2_BASE_ADDRESS = $BF806800;
UART2A_BASE_ADDRESS = $BF806400;
UART2B_BASE_ADDRESS = $BF806600;
UART3_BASE_ADDRESS = $BF806400;
UART3A_BASE_ADDRESS = $BF806800;
UART3B_BASE_ADDRESS = $BF806A00;
UART4_BASE_ADDRESS = $BF806200;
UART5_BASE_ADDRESS = $BF806A00;
UART6_BASE_ADDRESS = $BF806600;
USB_BASE_ADDRESS = $BF885040;
WDT_BASE_ADDRESS = $BF800000;
(* include generic header file for backwards compatibility with old C32 v1.xx code *)
implementation
  procedure CORE_TIMER_interrupt external name 'CORE_TIMER_interrupt';
  procedure CORE_SOFTWARE_0_interrupt external name 'CORE_SOFTWARE_0_interrupt';
  procedure CORE_SOFTWARE_1_interrupt external name 'CORE_SOFTWARE_1_interrupt';
  procedure EXTERNAL_0_interrupt external name 'EXTERNAL_0_interrupt';
  procedure TIMER_1_interrupt external name 'TIMER_1_interrupt';
  procedure INPUT_CAPTURE_1_interrupt external name 'INPUT_CAPTURE_1_interrupt';
  procedure OUTPUT_COMPARE_1_interrupt external name 'OUTPUT_COMPARE_1_interrupt';
  procedure EXTERNAL_1_interrupt external name 'EXTERNAL_1_interrupt';
  procedure TIMER_2_interrupt external name 'TIMER_2_interrupt';
  procedure INPUT_CAPTURE_2_interrupt external name 'INPUT_CAPTURE_2_interrupt';
  procedure OUTPUT_COMPARE_2_interrupt external name 'OUTPUT_COMPARE_2_interrupt';
  procedure EXTERNAL_2_interrupt external name 'EXTERNAL_2_interrupt';
  procedure TIMER_3_interrupt external name 'TIMER_3_interrupt';
  procedure INPUT_CAPTURE_3_interrupt external name 'INPUT_CAPTURE_3_interrupt';
  procedure OUTPUT_COMPARE_3_interrupt external name 'OUTPUT_COMPARE_3_interrupt';
  procedure EXTERNAL_3_interrupt external name 'EXTERNAL_3_interrupt';
  procedure TIMER_4_interrupt external name 'TIMER_4_interrupt';
  procedure INPUT_CAPTURE_4_interrupt external name 'INPUT_CAPTURE_4_interrupt';
  procedure OUTPUT_COMPARE_4_interrupt external name 'OUTPUT_COMPARE_4_interrupt';
  procedure EXTERNAL_4_interrupt external name 'EXTERNAL_4_interrupt';
  procedure TIMER_5_interrupt external name 'TIMER_5_interrupt';
  procedure INPUT_CAPTURE_5_interrupt external name 'INPUT_CAPTURE_5_interrupt';
  procedure OUTPUT_COMPARE_5_interrupt external name 'OUTPUT_COMPARE_5_interrupt';
  procedure SPI_1_interrupt external name 'SPI_1_interrupt';
  procedure I2C_3_interrupt external name 'I2C_3_interrupt';
  procedure I2C_1A_interrupt external name 'I2C_1A_interrupt';
  procedure SPI_3_interrupt external name 'SPI_3_interrupt';
  procedure SPI_1A_interrupt external name 'SPI_1A_interrupt';
  procedure UART_1_interrupt external name 'UART_1_interrupt';
  procedure UART_1A_interrupt external name 'UART_1A_interrupt';
  procedure I2C_1_interrupt external name 'I2C_1_interrupt';
  procedure CHANGE_NOTICE_interrupt external name 'CHANGE_NOTICE_interrupt';
  procedure ADC_interrupt external name 'ADC_interrupt';
  procedure PMP_interrupt external name 'PMP_interrupt';
  procedure COMPARATOR_1_interrupt external name 'COMPARATOR_1_interrupt';
  procedure COMPARATOR_2_interrupt external name 'COMPARATOR_2_interrupt';
  procedure I2C_4_interrupt external name 'I2C_4_interrupt';
  procedure I2C_2A_interrupt external name 'I2C_2A_interrupt';
  procedure SPI_2_interrupt external name 'SPI_2_interrupt';
  procedure SPI_2A_interrupt external name 'SPI_2A_interrupt';
  procedure UART_3_interrupt external name 'UART_3_interrupt';
  procedure UART_2A_interrupt external name 'UART_2A_interrupt';
  procedure I2C_5_interrupt external name 'I2C_5_interrupt';
  procedure I2C_3A_interrupt external name 'I2C_3A_interrupt';
  procedure SPI_4_interrupt external name 'SPI_4_interrupt';
  procedure SPI_3A_interrupt external name 'SPI_3A_interrupt';
  procedure UART_2_interrupt external name 'UART_2_interrupt';
  procedure UART_3A_interrupt external name 'UART_3A_interrupt';
  procedure I2C_2_interrupt external name 'I2C_2_interrupt';
  procedure FAIL_SAFE_MONITOR_interrupt external name 'FAIL_SAFE_MONITOR_interrupt';
  procedure RTCC_interrupt external name 'RTCC_interrupt';
  procedure DMA_0_interrupt external name 'DMA_0_interrupt';
  procedure DMA_1_interrupt external name 'DMA_1_interrupt';
  procedure DMA_2_interrupt external name 'DMA_2_interrupt';
  procedure DMA_3_interrupt external name 'DMA_3_interrupt';
  procedure DMA_4_interrupt external name 'DMA_4_interrupt';
  procedure DMA_5_interrupt external name 'DMA_5_interrupt';
  procedure DMA_6_interrupt external name 'DMA_6_interrupt';
  procedure DMA_7_interrupt external name 'DMA_7_interrupt';
  procedure FCE_interrupt external name 'FCE_interrupt';
  procedure USB_1_interrupt external name 'USB_1_interrupt';
  procedure CAN_1_interrupt external name 'CAN_1_interrupt';
  procedure CAN_2_interrupt external name 'CAN_2_interrupt';
  procedure ETH_interrupt external name 'ETH_interrupt';
  procedure UART_1B_interrupt external name 'UART_1B_interrupt';
  procedure UART_4_interrupt external name 'UART_4_interrupt';
  procedure UART_6_interrupt external name 'UART_6_interrupt';
  procedure UART_2B_interrupt external name 'UART_2B_interrupt';
  procedure UART_5_interrupt external name 'UART_5_interrupt';
  procedure UART_3B_interrupt external name 'UART_3B_interrupt';

  procedure Vectors; assembler;
  nostackframe;
  label interrupt_vectors;
  asm
    .section ".init.interrupt_vectors,\"ax\",@progbits"
    interrupt_vectors:
    j CORE_TIMER_interrupt
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    j CORE_SOFTWARE_0_interrupt
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    j CORE_SOFTWARE_1_interrupt
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    j EXTERNAL_0_interrupt
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    j TIMER_1_interrupt
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    j INPUT_CAPTURE_1_interrupt
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    j OUTPUT_COMPARE_1_interrupt
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    j EXTERNAL_1_interrupt
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    j TIMER_2_interrupt
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    j INPUT_CAPTURE_2_interrupt
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    j OUTPUT_COMPARE_2_interrupt
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    j EXTERNAL_2_interrupt
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    j TIMER_3_interrupt
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    j INPUT_CAPTURE_3_interrupt
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    j OUTPUT_COMPARE_3_interrupt
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    j EXTERNAL_3_interrupt
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    j TIMER_4_interrupt
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    j INPUT_CAPTURE_4_interrupt
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    j OUTPUT_COMPARE_4_interrupt
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    j EXTERNAL_4_interrupt
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    j TIMER_5_interrupt
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    j INPUT_CAPTURE_5_interrupt
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    j OUTPUT_COMPARE_5_interrupt
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    j SPI_1_interrupt
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    j I2C_3_interrupt
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop

    .weak CORE_TIMER_interrupt
    .weak CORE_SOFTWARE_0_interrupt
    .weak CORE_SOFTWARE_1_interrupt
    .weak EXTERNAL_0_interrupt
    .weak TIMER_1_interrupt
    .weak INPUT_CAPTURE_1_interrupt
    .weak OUTPUT_COMPARE_1_interrupt
    .weak EXTERNAL_1_interrupt
    .weak TIMER_2_interrupt
    .weak INPUT_CAPTURE_2_interrupt
    .weak OUTPUT_COMPARE_2_interrupt
    .weak EXTERNAL_2_interrupt
    .weak TIMER_3_interrupt
    .weak INPUT_CAPTURE_3_interrupt
    .weak OUTPUT_COMPARE_3_interrupt
    .weak EXTERNAL_3_interrupt
    .weak TIMER_4_interrupt
    .weak INPUT_CAPTURE_4_interrupt
    .weak OUTPUT_COMPARE_4_interrupt
    .weak EXTERNAL_4_interrupt
    .weak TIMER_5_interrupt
    .weak INPUT_CAPTURE_5_interrupt
    .weak OUTPUT_COMPARE_5_interrupt
    .weak SPI_1_interrupt
    .weak I2C_3_interrupt
    .weak I2C_1A_interrupt
    .weak SPI_3_interrupt
    .weak SPI_1A_interrupt
    .weak UART_1_interrupt
    .weak UART_1A_interrupt
    .weak I2C_1_interrupt
    .weak CHANGE_NOTICE_interrupt
    .weak ADC_interrupt
    .weak PMP_interrupt
    .weak COMPARATOR_1_interrupt
    .weak COMPARATOR_2_interrupt
    .weak I2C_4_interrupt
    .weak I2C_2A_interrupt
    .weak SPI_2_interrupt
    .weak SPI_2A_interrupt
    .weak UART_3_interrupt
    .weak UART_2A_interrupt
    .weak I2C_5_interrupt
    .weak I2C_3A_interrupt
    .weak SPI_4_interrupt
    .weak SPI_3A_interrupt
    .weak UART_2_interrupt
    .weak UART_3A_interrupt
    .weak I2C_2_interrupt
    .weak FAIL_SAFE_MONITOR_interrupt
    .weak RTCC_interrupt
    .weak DMA_0_interrupt
    .weak DMA_1_interrupt
    .weak DMA_2_interrupt
    .weak DMA_3_interrupt
    .weak DMA_4_interrupt
    .weak DMA_5_interrupt
    .weak DMA_6_interrupt
    .weak DMA_7_interrupt
    .weak FCE_interrupt
    .weak USB_1_interrupt
    .weak CAN_1_interrupt
    .weak CAN_2_interrupt
    .weak ETH_interrupt
    .weak UART_1B_interrupt
    .weak UART_4_interrupt
    .weak UART_6_interrupt
    .weak UART_2B_interrupt
    .weak UART_5_interrupt
    .weak UART_3B_interrupt

    .set CORE_TIMER_interrupt , DefaultInterrupt
    .set CORE_SOFTWARE_0_interrupt , DefaultInterrupt
    .set CORE_SOFTWARE_1_interrupt , DefaultInterrupt
    .set EXTERNAL_0_interrupt , DefaultInterrupt
    .set TIMER_1_interrupt , DefaultInterrupt
    .set INPUT_CAPTURE_1_interrupt , DefaultInterrupt
    .set OUTPUT_COMPARE_1_interrupt , DefaultInterrupt
    .set EXTERNAL_1_interrupt , DefaultInterrupt
    .set TIMER_2_interrupt , DefaultInterrupt
    .set INPUT_CAPTURE_2_interrupt , DefaultInterrupt
    .set OUTPUT_COMPARE_2_interrupt , DefaultInterrupt
    .set EXTERNAL_2_interrupt , DefaultInterrupt
    .set TIMER_3_interrupt , DefaultInterrupt
    .set INPUT_CAPTURE_3_interrupt , DefaultInterrupt
    .set OUTPUT_COMPARE_3_interrupt , DefaultInterrupt
    .set EXTERNAL_3_interrupt , DefaultInterrupt
    .set TIMER_4_interrupt , DefaultInterrupt
    .set INPUT_CAPTURE_4_interrupt , DefaultInterrupt
    .set OUTPUT_COMPARE_4_interrupt , DefaultInterrupt
    .set EXTERNAL_4_interrupt , DefaultInterrupt
    .set TIMER_5_interrupt , DefaultInterrupt
    .set INPUT_CAPTURE_5_interrupt , DefaultInterrupt
    .set OUTPUT_COMPARE_5_interrupt , DefaultInterrupt
    .set SPI_1_interrupt , DefaultInterrupt
    .set I2C_3_interrupt , DefaultInterrupt
    .set I2C_1A_interrupt , DefaultInterrupt
    .set SPI_3_interrupt , DefaultInterrupt
    .set SPI_1A_interrupt , DefaultInterrupt
    .set UART_1_interrupt , DefaultInterrupt
    .set UART_1A_interrupt , DefaultInterrupt
    .set I2C_1_interrupt , DefaultInterrupt
    .set CHANGE_NOTICE_interrupt , DefaultInterrupt
    .set ADC_interrupt , DefaultInterrupt
    .set PMP_interrupt , DefaultInterrupt
    .set COMPARATOR_1_interrupt , DefaultInterrupt
    .set COMPARATOR_2_interrupt , DefaultInterrupt
    .set I2C_4_interrupt , DefaultInterrupt
    .set I2C_2A_interrupt , DefaultInterrupt
    .set SPI_2_interrupt , DefaultInterrupt
    .set SPI_2A_interrupt , DefaultInterrupt
    .set UART_3_interrupt , DefaultInterrupt
    .set UART_2A_interrupt , DefaultInterrupt
    .set I2C_5_interrupt , DefaultInterrupt
    .set I2C_3A_interrupt , DefaultInterrupt
    .set SPI_4_interrupt , DefaultInterrupt
    .set SPI_3A_interrupt , DefaultInterrupt
    .set UART_2_interrupt , DefaultInterrupt
    .set UART_3A_interrupt , DefaultInterrupt
    .set I2C_2_interrupt , DefaultInterrupt
    .set FAIL_SAFE_MONITOR_interrupt , DefaultInterrupt
    .set RTCC_interrupt , DefaultInterrupt
    .set DMA_0_interrupt , DefaultInterrupt
    .set DMA_1_interrupt , DefaultInterrupt
    .set DMA_2_interrupt , DefaultInterrupt
    .set DMA_3_interrupt , DefaultInterrupt
    .set DMA_4_interrupt , DefaultInterrupt
    .set DMA_5_interrupt , DefaultInterrupt
    .set DMA_6_interrupt , DefaultInterrupt
    .set DMA_7_interrupt , DefaultInterrupt
    .set FCE_interrupt , DefaultInterrupt
    .set USB_1_interrupt , DefaultInterrupt
    .set CAN_1_interrupt , DefaultInterrupt
    .set CAN_2_interrupt , DefaultInterrupt
    .set ETH_interrupt , DefaultInterrupt
    .set UART_1B_interrupt , DefaultInterrupt
    .set UART_4_interrupt , DefaultInterrupt
    .set UART_6_interrupt , DefaultInterrupt
    .set UART_2B_interrupt , DefaultInterrupt
    .set UART_5_interrupt , DefaultInterrupt
    .set UART_3B_interrupt , DefaultInterrupt
    .text
  end;
end.
