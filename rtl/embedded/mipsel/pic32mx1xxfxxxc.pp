unit
{$goto on}
  pic32mx1xxfxxxc;
{$L startup.o}
interface
{$PACKRECORDS 2}
var
  DefaultInterrupt: record end; external name '_DefaultInterrupt';
(*-------------------------------------------------------------------------
 * PIC32MX110F016C processor header
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
    WDTWINEN : 0..1;
    SWDTPS : 0..31;
    RESERVED0 : 0..255;
    ON : 0..1;
  );
  1 : (
    RESERVED1 : 0..3;
    SWDTPS0 : 0..1;
    SWDTPS1 : 0..1;
    SWDTPS2 : 0..1;
    SWDTPS3 : 0..1;
    SWDTPS4 : 0..1;
  );
  2 : (
    RESERVED2 : 0..3;
    WDTPSTA : 0..31;
  );
  3 : (
    RESERVED3 : 0..3;
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
  I2C1CON : longWord absolute ($BF805000);
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
  I2C1CONbits: TI2C1CONbits absolute ($BF805000);
  I2C1ACONCLR : longWord absolute ($BF805004);
  I2C1CONCLR : longWord absolute ($BF805004);
  I2C1ACONSET : longWord absolute ($BF805008);
  I2C1CONSET : longWord absolute ($BF805008);
  I2C1ACONINV : longWord absolute ($BF80500C);
  I2C1CONINV : longWord absolute ($BF80500C);
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
  I2C1STAT : longWord absolute ($BF805010);
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
  I2C1STATbits: TI2C1STATbits absolute ($BF805010);
  I2C1ASTATCLR : longWord absolute ($BF805014);
  I2C1STATCLR : longWord absolute ($BF805014);
  I2C1ASTATSET : longWord absolute ($BF805018);
  I2C1STATSET : longWord absolute ($BF805018);
  I2C1ASTATINV : longWord absolute ($BF80501C);
  I2C1STATINV : longWord absolute ($BF80501C);
  I2C1AADD : longWord absolute ($BF805020);
  I2C1ADD : longWord absolute ($BF805020);
  I2C1AADDCLR : longWord absolute ($BF805024);
  I2C1ADDCLR : longWord absolute ($BF805024);
  I2C1AADDSET : longWord absolute ($BF805028);
  I2C1ADDSET : longWord absolute ($BF805028);
  I2C1AADDINV : longWord absolute ($BF80502C);
  I2C1ADDINV : longWord absolute ($BF80502C);
  I2C1AMSK : longWord absolute ($BF805030);
  I2C1MSK : longWord absolute ($BF805030);
  I2C1AMSKCLR : longWord absolute ($BF805034);
  I2C1MSKCLR : longWord absolute ($BF805034);
  I2C1AMSKSET : longWord absolute ($BF805038);
  I2C1MSKSET : longWord absolute ($BF805038);
  I2C1AMSKINV : longWord absolute ($BF80503C);
  I2C1MSKINV : longWord absolute ($BF80503C);
  I2C1ABRG : longWord absolute ($BF805040);
  I2C1BRG : longWord absolute ($BF805040);
  I2C1ABRGCLR : longWord absolute ($BF805044);
  I2C1BRGCLR : longWord absolute ($BF805044);
  I2C1ABRGSET : longWord absolute ($BF805048);
  I2C1BRGSET : longWord absolute ($BF805048);
  I2C1ABRGINV : longWord absolute ($BF80504C);
  I2C1BRGINV : longWord absolute ($BF80504C);
  I2C1ATRN : longWord absolute ($BF805050);
  I2C1TRN : longWord absolute ($BF805050);
  I2C1ATRNCLR : longWord absolute ($BF805054);
  I2C1TRNCLR : longWord absolute ($BF805054);
  I2C1ATRNSET : longWord absolute ($BF805058);
  I2C1TRNSET : longWord absolute ($BF805058);
  I2C1ATRNINV : longWord absolute ($BF80505C);
  I2C1TRNINV : longWord absolute ($BF80505C);
  I2C1ARCV : longWord absolute ($BF805060);
  I2C1RCV : longWord absolute ($BF805060);
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
  I2C2CON : longWord absolute ($BF805100);
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
  I2C2CONbits: TI2C2CONbits absolute ($BF805100);
  I2C2ACONCLR : longWord absolute ($BF805104);
  I2C2CONCLR : longWord absolute ($BF805104);
  I2C2ACONSET : longWord absolute ($BF805108);
  I2C2CONSET : longWord absolute ($BF805108);
  I2C2ACONINV : longWord absolute ($BF80510C);
  I2C2CONINV : longWord absolute ($BF80510C);
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
  I2C2STAT : longWord absolute ($BF805110);
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
  I2C2STATbits: TI2C2STATbits absolute ($BF805110);
  I2C2ASTATCLR : longWord absolute ($BF805114);
  I2C2STATCLR : longWord absolute ($BF805114);
  I2C2ASTATSET : longWord absolute ($BF805118);
  I2C2STATSET : longWord absolute ($BF805118);
  I2C2ASTATINV : longWord absolute ($BF80511C);
  I2C2STATINV : longWord absolute ($BF80511C);
  I2C2AADD : longWord absolute ($BF805120);
  I2C2ADD : longWord absolute ($BF805120);
  I2C2AADDCLR : longWord absolute ($BF805124);
  I2C2ADDCLR : longWord absolute ($BF805124);
  I2C2AADDSET : longWord absolute ($BF805128);
  I2C2ADDSET : longWord absolute ($BF805128);
  I2C2AADDINV : longWord absolute ($BF80512C);
  I2C2ADDINV : longWord absolute ($BF80512C);
  I2C2AMSK : longWord absolute ($BF805130);
  I2C2MSK : longWord absolute ($BF805130);
  I2C2AMSKCLR : longWord absolute ($BF805134);
  I2C2MSKCLR : longWord absolute ($BF805134);
  I2C2AMSKSET : longWord absolute ($BF805138);
  I2C2MSKSET : longWord absolute ($BF805138);
  I2C2AMSKINV : longWord absolute ($BF80513C);
  I2C2MSKINV : longWord absolute ($BF80513C);
  I2C2ABRG : longWord absolute ($BF805140);
  I2C2BRG : longWord absolute ($BF805140);
  I2C2ABRGCLR : longWord absolute ($BF805144);
  I2C2BRGCLR : longWord absolute ($BF805144);
  I2C2ABRGSET : longWord absolute ($BF805148);
  I2C2BRGSET : longWord absolute ($BF805148);
  I2C2ABRGINV : longWord absolute ($BF80514C);
  I2C2BRGINV : longWord absolute ($BF80514C);
  I2C2ATRN : longWord absolute ($BF805150);
  I2C2TRN : longWord absolute ($BF805150);
  I2C2ATRNCLR : longWord absolute ($BF805154);
  I2C2TRNCLR : longWord absolute ($BF805154);
  I2C2ATRNSET : longWord absolute ($BF805158);
  I2C2TRNSET : longWord absolute ($BF805158);
  I2C2ATRNINV : longWord absolute ($BF80515C);
  I2C2TRNINV : longWord absolute ($BF80515C);
  I2C2ARCV : longWord absolute ($BF805160);
  I2C2RCV : longWord absolute ($BF805160);
  SPI1CON : longWord absolute ($BF805800);
type
  TSPI1CONbits = bitpacked record
  case integer of
  0 : (
    SRXISEL : 0..3;
    STXISEL : 0..3;
    DISSDI : 0..1;
    MSTEN : 0..1;
    CKP : 0..1;
    SSEN : 0..1;
    CKE : 0..1;
    SMP : 0..1;
    MODE16 : 0..1;
    MODE32 : 0..1;
    DISSDO : 0..1;
    SIDL : 0..1;
    RESERVED0 : 0..1;
    ON : 0..1;
    ENHBUF : 0..1;
    SPIFE : 0..1;
    RESERVED1 : 0..31;
    MCLKSEL : 0..1;
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
  SPI1CONbits: TSPI1CONbits absolute ($BF805800);
  SPI1CONCLR : longWord absolute ($BF805804);
  SPI1CONSET : longWord absolute ($BF805808);
  SPI1CONINV : longWord absolute ($BF80580C);
  SPI1STAT : longWord absolute ($BF805810);
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
    FRMERR : 0..1;
    RESERVED3 : 0..7;
    TXBUFELM : 0..31;
    RESERVED4 : 0..7;
    RXBUFELM : 0..31;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  SPI1STATbits: TSPI1STATbits absolute ($BF805810);
  SPI1STATCLR : longWord absolute ($BF805814);
  SPI1STATSET : longWord absolute ($BF805818);
  SPI1STATINV : longWord absolute ($BF80581C);
  SPI1BUF : longWord absolute ($BF805820);
  SPI1BRG : longWord absolute ($BF805830);
  SPI1BRGCLR : longWord absolute ($BF805834);
  SPI1BRGSET : longWord absolute ($BF805838);
  SPI1BRGINV : longWord absolute ($BF80583C);
  SPI1CON2 : longWord absolute ($BF805840);
type
  TSPI1CON2bits = bitpacked record
  case integer of
  0 : (
    AUDMOD : 0..3;
    RESERVED0 : 0..1;
    AUDMONO : 0..1;
    RESERVED1 : 0..7;
    AUDEN : 0..1;
    IGNTUR : 0..1;
    IGNROV : 0..1;
    SPITUREN : 0..1;
    SPIROVEN : 0..1;
    FRMERREN : 0..1;
    RESERVED2 : 0..3;
    SPISGNEXT : 0..1;
  );
  1 : (
    AUDMOD0 : 0..1;
    AUDMOD1 : 0..1;
  );
  2 : (
    w : 0..4294967295;
  );
  end;
var
  SPI1CON2bits: TSPI1CON2bits absolute ($BF805840);
  SPI1CON2CLR : longWord absolute ($BF805844);
  SPI1CON2SET : longWord absolute ($BF805848);
  SPI1CON2INV : longWord absolute ($BF80584C);
  SPI2CON : longWord absolute ($BF805A00);
type
  TSPI2CONbits = bitpacked record
  case integer of
  0 : (
    SRXISEL : 0..3;
    STXISEL : 0..3;
    DISSDI : 0..1;
    MSTEN : 0..1;
    CKP : 0..1;
    SSEN : 0..1;
    CKE : 0..1;
    SMP : 0..1;
    MODE16 : 0..1;
    MODE32 : 0..1;
    DISSDO : 0..1;
    SIDL : 0..1;
    RESERVED0 : 0..1;
    ON : 0..1;
    ENHBUF : 0..1;
    SPIFE : 0..1;
    RESERVED1 : 0..31;
    MCLKSEL : 0..1;
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
  SPI2CONCLR : longWord absolute ($BF805A04);
  SPI2CONSET : longWord absolute ($BF805A08);
  SPI2CONINV : longWord absolute ($BF805A0C);
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
    FRMERR : 0..1;
    RESERVED3 : 0..7;
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
  SPI2STATCLR : longWord absolute ($BF805A14);
  SPI2STATSET : longWord absolute ($BF805A18);
  SPI2STATINV : longWord absolute ($BF805A1C);
  SPI2BUF : longWord absolute ($BF805A20);
  SPI2BRG : longWord absolute ($BF805A30);
  SPI2BRGCLR : longWord absolute ($BF805A34);
  SPI2BRGSET : longWord absolute ($BF805A38);
  SPI2BRGINV : longWord absolute ($BF805A3C);
  SPI2CON2 : longWord absolute ($BF805A40);
type
  TSPI2CON2bits = bitpacked record
  case integer of
  0 : (
    AUDMOD : 0..3;
    RESERVED0 : 0..1;
    AUDMONO : 0..1;
    RESERVED1 : 0..7;
    AUDEN : 0..1;
    IGNTUR : 0..1;
    IGNROV : 0..1;
    SPITUREN : 0..1;
    SPIROVEN : 0..1;
    FRMERREN : 0..1;
    RESERVED2 : 0..3;
    SPISGNEXT : 0..1;
  );
  1 : (
    AUDMOD0 : 0..1;
    AUDMOD1 : 0..1;
  );
  2 : (
    w : 0..4294967295;
  );
  end;
var
  SPI2CON2bits: TSPI2CON2bits absolute ($BF805A40);
  SPI2CON2CLR : longWord absolute ($BF805A44);
  SPI2CON2SET : longWord absolute ($BF805A48);
  SPI2CON2INV : longWord absolute ($BF805A4C);
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
  U2MODE : longWord absolute ($BF806200);
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
  U2MODEbits: TU2MODEbits absolute ($BF806200);
  U3AMODE : longWord absolute ($BF806200);
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
  U3AMODEbits: TU3AMODEbits absolute ($BF806200);
  U2MODECLR : longWord absolute ($BF806204);
  U3AMODECLR : longWord absolute ($BF806204);
  U2MODESET : longWord absolute ($BF806208);
  U3AMODESET : longWord absolute ($BF806208);
  U2MODEINV : longWord absolute ($BF80620C);
  U3AMODEINV : longWord absolute ($BF80620C);
  U2STA : longWord absolute ($BF806210);
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
  U2STAbits: TU2STAbits absolute ($BF806210);
  U3ASTA : longWord absolute ($BF806210);
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
  U3ASTAbits: TU3ASTAbits absolute ($BF806210);
  U2STACLR : longWord absolute ($BF806214);
  U3ASTACLR : longWord absolute ($BF806214);
  U2STASET : longWord absolute ($BF806218);
  U3ASTASET : longWord absolute ($BF806218);
  U2STAINV : longWord absolute ($BF80621C);
  U3ASTAINV : longWord absolute ($BF80621C);
  U2TXREG : longWord absolute ($BF806220);
  U3ATXREG : longWord absolute ($BF806220);
  U2RXREG : longWord absolute ($BF806230);
  U3ARXREG : longWord absolute ($BF806230);
  U2BRG : longWord absolute ($BF806240);
  U3ABRG : longWord absolute ($BF806240);
  U2BRGCLR : longWord absolute ($BF806244);
  U3ABRGCLR : longWord absolute ($BF806244);
  U2BRGSET : longWord absolute ($BF806248);
  U3ABRGSET : longWord absolute ($BF806248);
  U2BRGINV : longWord absolute ($BF80624C);
  U3ABRGINV : longWord absolute ($BF80624C);
  PMCON : longWord absolute ($BF807000);
type
  TPMCONbits = bitpacked record
  case integer of
  0 : (
    RDSP : 0..1;
    WRSP : 0..1;
    RESERVED0 : 0..1;
    CS1P : 0..1;
    RESERVED1 : 0..1;
    ALP : 0..1;
    CSF : 0..3;
    PTRDEN : 0..1;
    PTWREN : 0..1;
    PMPTTL : 0..1;
    ADRMUX : 0..3;
    SIDL : 0..1;
    RESERVED2 : 0..1;
    ON : 0..1;
  );
  1 : (
    RESERVED3 : 0..63;
    CSF0 : 0..1;
    CSF1 : 0..1;
    RESERVED4 : 0..7;
    ADRMUX0 : 0..1;
    ADRMUX1 : 0..1;
  );
  2 : (
    RESERVED5 : 0..8191;
    PSIDL : 0..1;
    RESERVED6 : 0..1;
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
    RESERVED0 : 0..1;
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
    RESERVED1 : 0..1;
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
  CM3CON : longWord absolute ($BF80A020);
type
  TCM3CONbits = bitpacked record
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
  CM3CONbits: TCM3CONbits absolute ($BF80A020);
  CM3CONCLR : longWord absolute ($BF80A024);
  CM3CONSET : longWord absolute ($BF80A028);
  CM3CONINV : longWord absolute ($BF80A02C);
  CMSTAT : longWord absolute ($BF80A060);
type
  TCMSTATbits = bitpacked record
  case integer of
  0 : (
    C1OUT : 0..1;
    C2OUT : 0..1;
    C3OUT : 0..1;
    RESERVED0 : 0..1023;
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
  CTMUCON : longWord absolute ($BF80A200);
type
  TCTMUCONbits = bitpacked record
  case integer of
  0 : (
    IRNG : 0..3;
    ITRIM : 0..63;
    CTTRIG : 0..1;
    IDISSEN : 0..1;
    EDGSEQEN : 0..1;
    EDGEN : 0..1;
    TGEN : 0..1;
    CTMUSIDL : 0..1;
    RESERVED0 : 0..1;
    ON : 0..1;
    RESERVED1 : 0..3;
    EDG2SEL : 0..15;
    EDG2POL : 0..1;
    EDG2MOD : 0..1;
    EDG1STAT : 0..1;
    EDG2STAT : 0..1;
    EDG1SEL : 0..15;
    EDG1POL : 0..1;
    EDG1MOD : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  CTMUCONbits: TCTMUCONbits absolute ($BF80A200);
  CTMUCONCLR : longWord absolute ($BF80A204);
  CTMUCONSET : longWord absolute ($BF80A208);
  CTMUCONINV : longWord absolute ($BF80A20C);
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
    PBDIVRDY : 0..1;
    SOSCRDY : 0..1;
    RESERVED2 : 0..1;
    FRCDIV : 0..7;
    PLLODIV : 0..7;
  );
  1 : (
    RESERVED3 : 0..255;
    NOSC0 : 0..1;
    NOSC1 : 0..1;
    NOSC2 : 0..1;
    RESERVED4 : 0..1;
    COSC0 : 0..1;
    COSC1 : 0..1;
    COSC2 : 0..1;
    RESERVED5 : 0..1;
    PLLMULT0 : 0..1;
    PLLMULT1 : 0..1;
    PLLMULT2 : 0..1;
    PBDIV0 : 0..1;
    PBDIV1 : 0..1;
    RESERVED6 : 0..7;
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
  REFOCON : longWord absolute ($BF80F020);
type
  TREFOCONbits = bitpacked record
  case integer of
  0 : (
    ROSEL : 0..15;
    RESERVED0 : 0..15;
    ACTIVE : 0..1;
    DIVSWEN : 0..1;
    RESERVED1 : 0..1;
    RSLP : 0..1;
    OE : 0..1;
    SIDL : 0..1;
    RESERVED2 : 0..1;
    ON : 0..1;
    RODIV : 0..32767;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  REFOCONbits: TREFOCONbits absolute ($BF80F020);
  REFOCONCLR : longWord absolute ($BF80F024);
  REFOCONSET : longWord absolute ($BF80F028);
  REFOCONINV : longWord absolute ($BF80F02C);
  REFOTRIM : longWord absolute ($BF80F030);
type
  TREFOTRIMbits = bitpacked record
  case integer of
  0 : (
    RESERVED0 : 0..8388607;
    ROTRIM : 0..511;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  REFOTRIMbits: TREFOTRIMbits absolute ($BF80F030);
  REFOTRIMCLR : longWord absolute ($BF80F034);
  REFOTRIMSET : longWord absolute ($BF80F038);
  REFOTRIMINV : longWord absolute ($BF80F03C);
  CFGCON : longWord absolute ($BF80F200);
type
  TCFGCONbits = bitpacked record
    TDOEN : 0..1;
    RESERVED0 : 0..1;
    TROEN : 0..1;
    JTAGEN : 0..1;
    RESERVED1 : 0..255;
    PMDLOCK : 0..1;
    IOLOCK : 0..1;
  end;
var
  CFGCONbits: TCFGCONbits absolute ($BF80F200);
  DDPCON : longWord absolute ($BF80F200);
type
  TDDPCONbits = bitpacked record
    TDOEN : 0..1;
    RESERVED0 : 0..1;
    TROEN : 0..1;
    JTAGEN : 0..1;
    RESERVED1 : 0..255;
    PMDLOCK : 0..1;
    IOLOCK : 0..1;
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
  PMD1 : longWord absolute ($BF80F240);
type
  TPMD1bits = bitpacked record
    AD1MD : 0..1;
    RESERVED0 : 0..127;
    CTMUMD : 0..1;
    RESERVED1 : 0..7;
    CVRMD : 0..1;
  end;
var
  PMD1bits: TPMD1bits absolute ($BF80F240);
  PMD1CLR : longWord absolute ($BF80F244);
  PMD1SET : longWord absolute ($BF80F248);
  PMD1INV : longWord absolute ($BF80F24C);
  PMD2 : longWord absolute ($BF80F250);
type
  TPMD2bits = bitpacked record
    CMP1MD : 0..1;
    CMP2MD : 0..1;
    CMP3MD : 0..1;
  end;
var
  PMD2bits: TPMD2bits absolute ($BF80F250);
  PMD2CLR : longWord absolute ($BF80F254);
  PMD2SET : longWord absolute ($BF80F258);
  PMD2INV : longWord absolute ($BF80F25C);
  PMD3 : longWord absolute ($BF80F260);
type
  TPMD3bits = bitpacked record
    IC1MD : 0..1;
    IC2MD : 0..1;
    IC3MD : 0..1;
    IC4MD : 0..1;
    IC5MD : 0..1;
    RESERVED0 : 0..2047;
    OC1MD : 0..1;
    OC2MD : 0..1;
    OC3MD : 0..1;
    OC4MD : 0..1;
    OC5MD : 0..1;
  end;
var
  PMD3bits: TPMD3bits absolute ($BF80F260);
  PMD3CLR : longWord absolute ($BF80F264);
  PMD3SET : longWord absolute ($BF80F268);
  PMD3INV : longWord absolute ($BF80F26C);
  PMD4 : longWord absolute ($BF80F270);
type
  TPMD4bits = bitpacked record
    T1MD : 0..1;
    T2MD : 0..1;
    T3MD : 0..1;
    T4MD : 0..1;
    T5MD : 0..1;
  end;
var
  PMD4bits: TPMD4bits absolute ($BF80F270);
  PMD4CLR : longWord absolute ($BF80F274);
  PMD4SET : longWord absolute ($BF80F278);
  PMD4INV : longWord absolute ($BF80F27C);
  PMD5 : longWord absolute ($BF80F280);
type
  TPMD5bits = bitpacked record
    U1MD : 0..1;
    U2MD : 0..1;
    RESERVED0 : 0..63;
    SPI1MD : 0..1;
    SPI2MD : 0..1;
    RESERVED1 : 0..63;
    I2C1MD : 0..1;
    I2C2MD : 0..1;
  end;
var
  PMD5bits: TPMD5bits absolute ($BF80F280);
  PMD5CLR : longWord absolute ($BF80F284);
  PMD5SET : longWord absolute ($BF80F288);
  PMD5INV : longWord absolute ($BF80F28C);
  PMD6 : longWord absolute ($BF80F290);
type
  TPMD6bits = bitpacked record
    RTCCMD : 0..1;
    REFOMD : 0..1;
    RESERVED0 : 0..16383;
    PMPMD : 0..1;
  end;
var
  PMD6bits: TPMD6bits absolute ($BF80F290);
  PMD6CLR : longWord absolute ($BF80F294);
  PMD6SET : longWord absolute ($BF80F298);
  PMD6INV : longWord absolute ($BF80F29C);
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
  INT1R : longWord absolute ($BF80FA04);
type
  TINT1Rbits = bitpacked record
    INT1R : 0..15;
  end;
var
  INT1Rbits: TINT1Rbits absolute ($BF80FA04);
  INT2R : longWord absolute ($BF80FA08);
type
  TINT2Rbits = bitpacked record
    INT2R : 0..15;
  end;
var
  INT2Rbits: TINT2Rbits absolute ($BF80FA08);
  INT3R : longWord absolute ($BF80FA0C);
type
  TINT3Rbits = bitpacked record
    INT3R : 0..15;
  end;
var
  INT3Rbits: TINT3Rbits absolute ($BF80FA0C);
  INT4R : longWord absolute ($BF80FA10);
type
  TINT4Rbits = bitpacked record
    INT4R : 0..15;
  end;
var
  INT4Rbits: TINT4Rbits absolute ($BF80FA10);
  T2CKR : longWord absolute ($BF80FA18);
type
  TT2CKRbits = bitpacked record
    T2CKR : 0..15;
  end;
var
  T2CKRbits: TT2CKRbits absolute ($BF80FA18);
  T3CKR : longWord absolute ($BF80FA1C);
type
  TT3CKRbits = bitpacked record
    T3CKR : 0..15;
  end;
var
  T3CKRbits: TT3CKRbits absolute ($BF80FA1C);
  T4CKR : longWord absolute ($BF80FA20);
type
  TT4CKRbits = bitpacked record
    T4CKR : 0..15;
  end;
var
  T4CKRbits: TT4CKRbits absolute ($BF80FA20);
  T5CKR : longWord absolute ($BF80FA24);
type
  TT5CKRbits = bitpacked record
    T5CKR : 0..15;
  end;
var
  T5CKRbits: TT5CKRbits absolute ($BF80FA24);
  IC1R : longWord absolute ($BF80FA28);
type
  TIC1Rbits = bitpacked record
    IC1R : 0..15;
  end;
var
  IC1Rbits: TIC1Rbits absolute ($BF80FA28);
  IC2R : longWord absolute ($BF80FA2C);
type
  TIC2Rbits = bitpacked record
    IC2R : 0..15;
  end;
var
  IC2Rbits: TIC2Rbits absolute ($BF80FA2C);
  IC3R : longWord absolute ($BF80FA30);
type
  TIC3Rbits = bitpacked record
    IC3R : 0..15;
  end;
var
  IC3Rbits: TIC3Rbits absolute ($BF80FA30);
  IC4R : longWord absolute ($BF80FA34);
type
  TIC4Rbits = bitpacked record
    IC4R : 0..15;
  end;
var
  IC4Rbits: TIC4Rbits absolute ($BF80FA34);
  IC5R : longWord absolute ($BF80FA38);
type
  TIC5Rbits = bitpacked record
    IC5R : 0..15;
  end;
var
  IC5Rbits: TIC5Rbits absolute ($BF80FA38);
  OCFAR : longWord absolute ($BF80FA48);
type
  TOCFARbits = bitpacked record
    OCFAR : 0..15;
  end;
var
  OCFARbits: TOCFARbits absolute ($BF80FA48);
  OCFBR : longWord absolute ($BF80FA4C);
type
  TOCFBRbits = bitpacked record
    OCFBR : 0..15;
  end;
var
  OCFBRbits: TOCFBRbits absolute ($BF80FA4C);
  U1RXR : longWord absolute ($BF80FA50);
type
  TU1RXRbits = bitpacked record
    U1RXR : 0..15;
  end;
var
  U1RXRbits: TU1RXRbits absolute ($BF80FA50);
  U1CTSR : longWord absolute ($BF80FA54);
type
  TU1CTSRbits = bitpacked record
    U1CTSR : 0..15;
  end;
var
  U1CTSRbits: TU1CTSRbits absolute ($BF80FA54);
  U2RXR : longWord absolute ($BF80FA58);
type
  TU2RXRbits = bitpacked record
    U2RXR : 0..15;
  end;
var
  U2RXRbits: TU2RXRbits absolute ($BF80FA58);
  U2CTSR : longWord absolute ($BF80FA5C);
type
  TU2CTSRbits = bitpacked record
    U2CTSR : 0..15;
  end;
var
  U2CTSRbits: TU2CTSRbits absolute ($BF80FA5C);
  SDI1R : longWord absolute ($BF80FA84);
type
  TSDI1Rbits = bitpacked record
    SDI1R : 0..15;
  end;
var
  SDI1Rbits: TSDI1Rbits absolute ($BF80FA84);
  SS1R : longWord absolute ($BF80FA88);
type
  TSS1Rbits = bitpacked record
    SS1R : 0..15;
  end;
var
  SS1Rbits: TSS1Rbits absolute ($BF80FA88);
  SDI2R : longWord absolute ($BF80FA90);
type
  TSDI2Rbits = bitpacked record
    SDI2R : 0..15;
  end;
var
  SDI2Rbits: TSDI2Rbits absolute ($BF80FA90);
  SS2R : longWord absolute ($BF80FA94);
type
  TSS2Rbits = bitpacked record
    SS2R : 0..15;
  end;
var
  SS2Rbits: TSS2Rbits absolute ($BF80FA94);
  REFCLKIR : longWord absolute ($BF80FAB8);
type
  TREFCLKIRbits = bitpacked record
    REFCLKIR : 0..15;
  end;
var
  REFCLKIRbits: TREFCLKIRbits absolute ($BF80FAB8);
  RPA0R : longWord absolute ($BF80FB00);
type
  TRPA0Rbits = bitpacked record
    RPA0R : 0..15;
  end;
var
  RPA0Rbits: TRPA0Rbits absolute ($BF80FB00);
  RPA1R : longWord absolute ($BF80FB04);
type
  TRPA1Rbits = bitpacked record
    RPA1R : 0..15;
  end;
var
  RPA1Rbits: TRPA1Rbits absolute ($BF80FB04);
  RPA2R : longWord absolute ($BF80FB08);
type
  TRPA2Rbits = bitpacked record
    RPA2R : 0..15;
  end;
var
  RPA2Rbits: TRPA2Rbits absolute ($BF80FB08);
  RPA3R : longWord absolute ($BF80FB0C);
type
  TRPA3Rbits = bitpacked record
    RPA3R : 0..15;
  end;
var
  RPA3Rbits: TRPA3Rbits absolute ($BF80FB0C);
  RPA4R : longWord absolute ($BF80FB10);
type
  TRPA4Rbits = bitpacked record
    RPA4R : 0..15;
  end;
var
  RPA4Rbits: TRPA4Rbits absolute ($BF80FB10);
  RPA8R : longWord absolute ($BF80FB20);
type
  TRPA8Rbits = bitpacked record
    RPA8R : 0..15;
  end;
var
  RPA8Rbits: TRPA8Rbits absolute ($BF80FB20);
  RPA9R : longWord absolute ($BF80FB24);
type
  TRPA9Rbits = bitpacked record
    RPA9R : 0..15;
  end;
var
  RPA9Rbits: TRPA9Rbits absolute ($BF80FB24);
  RPB0R : longWord absolute ($BF80FB2C);
type
  TRPB0Rbits = bitpacked record
    RPB0R : 0..15;
  end;
var
  RPB0Rbits: TRPB0Rbits absolute ($BF80FB2C);
  RPB1R : longWord absolute ($BF80FB30);
type
  TRPB1Rbits = bitpacked record
    RPB1R : 0..15;
  end;
var
  RPB1Rbits: TRPB1Rbits absolute ($BF80FB30);
  RPB2R : longWord absolute ($BF80FB34);
type
  TRPB2Rbits = bitpacked record
    RPB2R : 0..15;
  end;
var
  RPB2Rbits: TRPB2Rbits absolute ($BF80FB34);
  RPB3R : longWord absolute ($BF80FB38);
type
  TRPB3Rbits = bitpacked record
    RPB3R : 0..15;
  end;
var
  RPB3Rbits: TRPB3Rbits absolute ($BF80FB38);
  RPB4R : longWord absolute ($BF80FB3C);
type
  TRPB4Rbits = bitpacked record
    RPB4R : 0..15;
  end;
var
  RPB4Rbits: TRPB4Rbits absolute ($BF80FB3C);
  RPB5R : longWord absolute ($BF80FB40);
type
  TRPB5Rbits = bitpacked record
    RPB5R : 0..15;
  end;
var
  RPB5Rbits: TRPB5Rbits absolute ($BF80FB40);
  RPB6R : longWord absolute ($BF80FB44);
type
  TRPB6Rbits = bitpacked record
    RPB6R : 0..15;
  end;
var
  RPB6Rbits: TRPB6Rbits absolute ($BF80FB44);
  RPB7R : longWord absolute ($BF80FB48);
type
  TRPB7Rbits = bitpacked record
    RPB7R : 0..15;
  end;
var
  RPB7Rbits: TRPB7Rbits absolute ($BF80FB48);
  RPB8R : longWord absolute ($BF80FB4C);
type
  TRPB8Rbits = bitpacked record
    RPB8R : 0..15;
  end;
var
  RPB8Rbits: TRPB8Rbits absolute ($BF80FB4C);
  RPB9R : longWord absolute ($BF80FB50);
type
  TRPB9Rbits = bitpacked record
    RPB9R : 0..15;
  end;
var
  RPB9Rbits: TRPB9Rbits absolute ($BF80FB50);
  RPB10R : longWord absolute ($BF80FB54);
type
  TRPB10Rbits = bitpacked record
    RPB10R : 0..15;
  end;
var
  RPB10Rbits: TRPB10Rbits absolute ($BF80FB54);
  RPB11R : longWord absolute ($BF80FB58);
type
  TRPB11Rbits = bitpacked record
    RPB11R : 0..15;
  end;
var
  RPB11Rbits: TRPB11Rbits absolute ($BF80FB58);
  RPB12R : longWord absolute ($BF80FB5C);
type
  TRPB12Rbits = bitpacked record
    RPB12R : 0..15;
  end;
var
  RPB12Rbits: TRPB12Rbits absolute ($BF80FB5C);
  RPB13R : longWord absolute ($BF80FB60);
type
  TRPB13Rbits = bitpacked record
    RPB13R : 0..15;
  end;
var
  RPB13Rbits: TRPB13Rbits absolute ($BF80FB60);
  RPB14R : longWord absolute ($BF80FB64);
type
  TRPB14Rbits = bitpacked record
    RPB14R : 0..15;
  end;
var
  RPB14Rbits: TRPB14Rbits absolute ($BF80FB64);
  RPB15R : longWord absolute ($BF80FB68);
type
  TRPB15Rbits = bitpacked record
    RPB15R : 0..15;
  end;
var
  RPB15Rbits: TRPB15Rbits absolute ($BF80FB68);
  RPC0R : longWord absolute ($BF80FB6C);
type
  TRPC0Rbits = bitpacked record
    RPC0R : 0..15;
  end;
var
  RPC0Rbits: TRPC0Rbits absolute ($BF80FB6C);
  RPC1R : longWord absolute ($BF80FB70);
type
  TRPC1Rbits = bitpacked record
    RPC1R : 0..15;
  end;
var
  RPC1Rbits: TRPC1Rbits absolute ($BF80FB70);
  RPC2R : longWord absolute ($BF80FB74);
type
  TRPC2Rbits = bitpacked record
    RPC2R : 0..15;
  end;
var
  RPC2Rbits: TRPC2Rbits absolute ($BF80FB74);
  RPC3R : longWord absolute ($BF80FB78);
type
  TRPC3Rbits = bitpacked record
    RPC3R : 0..15;
  end;
var
  RPC3Rbits: TRPC3Rbits absolute ($BF80FB78);
  RPC4R : longWord absolute ($BF80FB7C);
type
  TRPC4Rbits = bitpacked record
    RPC4R : 0..15;
  end;
var
  RPC4Rbits: TRPC4Rbits absolute ($BF80FB7C);
  RPC5R : longWord absolute ($BF80FB80);
type
  TRPC5Rbits = bitpacked record
    RPC5R : 0..15;
  end;
var
  RPC5Rbits: TRPC5Rbits absolute ($BF80FB80);
  RPC6R : longWord absolute ($BF80FB84);
type
  TRPC6Rbits = bitpacked record
    RPC6R : 0..15;
  end;
var
  RPC6Rbits: TRPC6Rbits absolute ($BF80FB84);
  RPC7R : longWord absolute ($BF80FB88);
type
  TRPC7Rbits = bitpacked record
    RPC7R : 0..15;
  end;
var
  RPC7Rbits: TRPC7Rbits absolute ($BF80FB88);
  RPC8R : longWord absolute ($BF80FB8C);
type
  TRPC8Rbits = bitpacked record
    RPC8R : 0..15;
  end;
var
  RPC8Rbits: TRPC8Rbits absolute ($BF80FB8C);
  RPC9R : longWord absolute ($BF80FB90);
type
  TRPC9Rbits = bitpacked record
    RPC9R : 0..15;
  end;
var
  RPC9Rbits: TRPC9Rbits absolute ($BF80FB90);
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
    RESERVED2 : 0..7;
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
    IC1EIF : 0..1;
    IC1IF : 0..1;
    OC1IF : 0..1;
    INT1IF : 0..1;
    T2IF : 0..1;
    IC2EIF : 0..1;
    IC2IF : 0..1;
    OC2IF : 0..1;
    INT2IF : 0..1;
    T3IF : 0..1;
    IC3EIF : 0..1;
    IC3IF : 0..1;
    OC3IF : 0..1;
    INT3IF : 0..1;
    T4IF : 0..1;
    IC4EIF : 0..1;
    IC4IF : 0..1;
    OC4IF : 0..1;
    INT4IF : 0..1;
    T5IF : 0..1;
    IC5EIF : 0..1;
    IC5IF : 0..1;
    OC5IF : 0..1;
    AD1IF : 0..1;
    FSCMIF : 0..1;
    RTCCIF : 0..1;
    FCEIF : 0..1;
  );
  1 : (
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
    CMP1IF : 0..1;
    CMP2IF : 0..1;
    CMP3IF : 0..1;
    RESERVED0 : 0..1;
    SPI1EIF : 0..1;
    SPI1RXIF : 0..1;
    SPI1TXIF : 0..1;
    U1EIF : 0..1;
    U1RXIF : 0..1;
    U1TXIF : 0..1;
    I2C1BIF : 0..1;
    I2C1SIF : 0..1;
    I2C1MIF : 0..1;
    CNAIF : 0..1;
    CNBIF : 0..1;
    CNCIF : 0..1;
    PMPIF : 0..1;
    PMPEIF : 0..1;
    SPI2EIF : 0..1;
    SPI2RXIF : 0..1;
    SPI2TXIF : 0..1;
    U2EIF : 0..1;
    U2RXIF : 0..1;
    U2TXIF : 0..1;
    I2C2BIF : 0..1;
    I2C2SIF : 0..1;
    I2C2MIF : 0..1;
    CTMUIF : 0..1;
    DMA0IF : 0..1;
    DMA1IF : 0..1;
    DMA2IF : 0..1;
    DMA3IF : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  IFS1bits: TIFS1bits absolute ($BF881040);
  IFS1CLR : longWord absolute ($BF881044);
  IFS1SET : longWord absolute ($BF881048);
  IFS1INV : longWord absolute ($BF88104C);
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
    IC1EIE : 0..1;
    IC1IE : 0..1;
    OC1IE : 0..1;
    INT1IE : 0..1;
    T2IE : 0..1;
    IC2EIE : 0..1;
    IC2IE : 0..1;
    OC2IE : 0..1;
    INT2IE : 0..1;
    T3IE : 0..1;
    IC3EIE : 0..1;
    IC3IE : 0..1;
    OC3IE : 0..1;
    INT3IE : 0..1;
    T4IE : 0..1;
    IC4EIE : 0..1;
    IC4IE : 0..1;
    OC4IE : 0..1;
    INT4IE : 0..1;
    T5IE : 0..1;
    IC5EIE : 0..1;
    IC5IE : 0..1;
    OC5IE : 0..1;
    AD1IE : 0..1;
    FSCMIE : 0..1;
    RTCCIE : 0..1;
    FCEIE : 0..1;
  );
  1 : (
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
    CMP1IE : 0..1;
    CMP2IE : 0..1;
    CMP3IE : 0..1;
    RESERVED0 : 0..1;
    SPI1EIE : 0..1;
    SPI1RXIE : 0..1;
    SPI1TXIE : 0..1;
    U1EIE : 0..1;
    U1RXIE : 0..1;
    U1TXIE : 0..1;
    I2C1BIE : 0..1;
    I2C1SIE : 0..1;
    I2C1MIE : 0..1;
    CNAIE : 0..1;
    CNBIE : 0..1;
    CNCIE : 0..1;
    PMPIE : 0..1;
    PMPEIE : 0..1;
    SPI2EIE : 0..1;
    SPI2RXIE : 0..1;
    SPI2TXIE : 0..1;
    U2EIE : 0..1;
    U2RXIE : 0..1;
    U2TXIE : 0..1;
    I2C2BIE : 0..1;
    I2C2SIE : 0..1;
    I2C2MIE : 0..1;
    CTMUIE : 0..1;
    DMA0IE : 0..1;
    DMA1IE : 0..1;
    DMA2IE : 0..1;
    DMA3IE : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  IEC1bits: TIEC1bits absolute ($BF881070);
  IEC1CLR : longWord absolute ($BF881074);
  IEC1SET : longWord absolute ($BF881078);
  IEC1INV : longWord absolute ($BF88107C);
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
    AD1IS : 0..3;
    AD1IP : 0..7;
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
    FSCMIS : 0..3;
    FSCMIP : 0..7;
    RESERVED0 : 0..7;
    RTCCIS : 0..3;
    RTCCIP : 0..7;
    RESERVED1 : 0..7;
    FCEIS : 0..3;
    FCEIP : 0..7;
    RESERVED2 : 0..7;
    CMP1IS : 0..3;
    CMP1IP : 0..7;
  );
  1 : (
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
    CMP2IS : 0..3;
    CMP2IP : 0..7;
    RESERVED0 : 0..7;
    CMP3IS : 0..3;
    CMP3IP : 0..7;
    RESERVED1 : 0..2047;
    SPI1IS : 0..3;
    SPI1IP : 0..7;
  );
  1 : (
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
    U1IS : 0..3;
    U1IP : 0..7;
    RESERVED0 : 0..7;
    I2C1IS : 0..3;
    I2C1IP : 0..7;
    RESERVED1 : 0..7;
    CNIS : 0..3;
    CNIP : 0..7;
    RESERVED2 : 0..7;
    PMPIS : 0..3;
    PMPIP : 0..7;
  );
  1 : (
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
    SPI2IS : 0..3;
    SPI2IP : 0..7;
    RESERVED0 : 0..7;
    U2IS : 0..3;
    U2IP : 0..7;
    RESERVED1 : 0..7;
    I2C2IS : 0..3;
    I2C2IP : 0..7;
    RESERVED2 : 0..7;
    CTMUIS : 0..3;
    CTMUIP : 0..7;
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
  IPC10bits: TIPC10bits absolute ($BF881130);
  IPC10CLR : longWord absolute ($BF881134);
  IPC10SET : longWord absolute ($BF881138);
  IPC10INV : longWord absolute ($BF88113C);
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
  ANSELA : longWord absolute ($BF886000);
type
  TANSELAbits = bitpacked record
  case integer of
  0 : (
    ANSA0 : 0..1;
    ANSA1 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  ANSELAbits: TANSELAbits absolute ($BF886000);
  ANSELACLR : longWord absolute ($BF886004);
  ANSELASET : longWord absolute ($BF886008);
  ANSELAINV : longWord absolute ($BF88600C);
  TRISA : longWord absolute ($BF886010);
type
  TTRISAbits = bitpacked record
  case integer of
  0 : (
    TRISA0 : 0..1;
    TRISA1 : 0..1;
    TRISA2 : 0..1;
    TRISA3 : 0..1;
    TRISA4 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  TRISAbits: TTRISAbits absolute ($BF886010);
  TRISACLR : longWord absolute ($BF886014);
  TRISASET : longWord absolute ($BF886018);
  TRISAINV : longWord absolute ($BF88601C);
  PORTA : longWord absolute ($BF886020);
type
  TPORTAbits = bitpacked record
  case integer of
  0 : (
    RA0 : 0..1;
    RA1 : 0..1;
    RA2 : 0..1;
    RA3 : 0..1;
    RA4 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  PORTAbits: TPORTAbits absolute ($BF886020);
  PORTACLR : longWord absolute ($BF886024);
  PORTASET : longWord absolute ($BF886028);
  PORTAINV : longWord absolute ($BF88602C);
  LATA : longWord absolute ($BF886030);
type
  TLATAbits = bitpacked record
  case integer of
  0 : (
    LATA0 : 0..1;
    LATA1 : 0..1;
    LATA2 : 0..1;
    LATA3 : 0..1;
    LATA4 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  LATAbits: TLATAbits absolute ($BF886030);
  LATACLR : longWord absolute ($BF886034);
  LATASET : longWord absolute ($BF886038);
  LATAINV : longWord absolute ($BF88603C);
  ODCA : longWord absolute ($BF886040);
type
  TODCAbits = bitpacked record
    w : 0..4294967295;
  end;
var
  ODCAbits: TODCAbits absolute ($BF886040);
  ODCACLR : longWord absolute ($BF886044);
  ODCASET : longWord absolute ($BF886048);
  ODCAINV : longWord absolute ($BF88604C);
  CNPUA : longWord absolute ($BF886050);
type
  TCNPUAbits = bitpacked record
  case integer of
  0 : (
    CNPUA0 : 0..1;
    CNPUA1 : 0..1;
    CNPUA2 : 0..1;
    CNPUA3 : 0..1;
    CNPUA4 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  CNPUAbits: TCNPUAbits absolute ($BF886050);
  CNPUACLR : longWord absolute ($BF886054);
  CNPUASET : longWord absolute ($BF886058);
  CNPUAINV : longWord absolute ($BF88605C);
  CNPDA : longWord absolute ($BF886060);
type
  TCNPDAbits = bitpacked record
  case integer of
  0 : (
    CNPDA0 : 0..1;
    CNPDA1 : 0..1;
    CNPDA2 : 0..1;
    CNPDA3 : 0..1;
    CNPDA4 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  CNPDAbits: TCNPDAbits absolute ($BF886060);
  CNPDACLR : longWord absolute ($BF886064);
  CNPDASET : longWord absolute ($BF886068);
  CNPDAINV : longWord absolute ($BF88606C);
  CNCONA : longWord absolute ($BF886070);
type
  TCNCONAbits = bitpacked record
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
  CNCONAbits: TCNCONAbits absolute ($BF886070);
  CNCONACLR : longWord absolute ($BF886074);
  CNCONASET : longWord absolute ($BF886078);
  CNCONAINV : longWord absolute ($BF88607C);
  CNENA : longWord absolute ($BF886080);
type
  TCNENAbits = bitpacked record
  case integer of
  0 : (
    CNIEA0 : 0..1;
    CNIEA1 : 0..1;
    CNIEA2 : 0..1;
    CNIEA3 : 0..1;
    CNIEA4 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  CNENAbits: TCNENAbits absolute ($BF886080);
  CNENACLR : longWord absolute ($BF886084);
  CNENASET : longWord absolute ($BF886088);
  CNENAINV : longWord absolute ($BF88608C);
  CNSTATA : longWord absolute ($BF886090);
type
  TCNSTATAbits = bitpacked record
  case integer of
  0 : (
    CNSTATA0 : 0..1;
    CNSTATA1 : 0..1;
    CNSTATA2 : 0..1;
    CNSTATA3 : 0..1;
    CNSTATA4 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  CNSTATAbits: TCNSTATAbits absolute ($BF886090);
  CNSTATACLR : longWord absolute ($BF886094);
  CNSTATASET : longWord absolute ($BF886098);
  CNSTATAINV : longWord absolute ($BF88609C);
  ANSELB : longWord absolute ($BF886100);
type
  TANSELBbits = bitpacked record
  case integer of
  0 : (
    ANSB0 : 0..1;
    ANSB1 : 0..1;
    ANSB2 : 0..1;
    ANSB3 : 0..1;
    RESERVED0 : 0..255;
    ANSB12 : 0..1;
    ANSB13 : 0..1;
    ANSB14 : 0..1;
    ANSB15 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  ANSELBbits: TANSELBbits absolute ($BF886100);
  ANSELBCLR : longWord absolute ($BF886104);
  ANSELBSET : longWord absolute ($BF886108);
  ANSELBINV : longWord absolute ($BF88610C);
  TRISB : longWord absolute ($BF886110);
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
  TRISBbits: TTRISBbits absolute ($BF886110);
  TRISBCLR : longWord absolute ($BF886114);
  TRISBSET : longWord absolute ($BF886118);
  TRISBINV : longWord absolute ($BF88611C);
  PORTB : longWord absolute ($BF886120);
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
  PORTBbits: TPORTBbits absolute ($BF886120);
  PORTBCLR : longWord absolute ($BF886124);
  PORTBSET : longWord absolute ($BF886128);
  PORTBINV : longWord absolute ($BF88612C);
  LATB : longWord absolute ($BF886130);
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
  LATBbits: TLATBbits absolute ($BF886130);
  LATBCLR : longWord absolute ($BF886134);
  LATBSET : longWord absolute ($BF886138);
  LATBINV : longWord absolute ($BF88613C);
  ODCB : longWord absolute ($BF886140);
type
  TODCBbits = bitpacked record
  case integer of
  0 : (
    RESERVED0 : 0..15;
    ODCB4 : 0..1;
    ODCB5 : 0..1;
    ODCB6 : 0..1;
    ODCB7 : 0..1;
    ODCB8 : 0..1;
    ODCB9 : 0..1;
    ODCB10 : 0..1;
    ODCB11 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  ODCBbits: TODCBbits absolute ($BF886140);
  ODCBCLR : longWord absolute ($BF886144);
  ODCBSET : longWord absolute ($BF886148);
  ODCBINV : longWord absolute ($BF88614C);
  CNPUB : longWord absolute ($BF886150);
type
  TCNPUBbits = bitpacked record
  case integer of
  0 : (
    CNPUB0 : 0..1;
    CNPUB1 : 0..1;
    CNPUB2 : 0..1;
    CNPUB3 : 0..1;
    CNPUB4 : 0..1;
    CNPUB5 : 0..1;
    CNPUB6 : 0..1;
    CNPUB7 : 0..1;
    CNPUB8 : 0..1;
    CNPUB9 : 0..1;
    CNPUB10 : 0..1;
    CNPUB11 : 0..1;
    CNPUB12 : 0..1;
    CNPUB13 : 0..1;
    CNPUB14 : 0..1;
    CNPUB15 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  CNPUBbits: TCNPUBbits absolute ($BF886150);
  CNPUBCLR : longWord absolute ($BF886154);
  CNPUBSET : longWord absolute ($BF886158);
  CNPUBINV : longWord absolute ($BF88615C);
  CNPDB : longWord absolute ($BF886160);
type
  TCNPDBbits = bitpacked record
  case integer of
  0 : (
    CNPDB0 : 0..1;
    CNPDB1 : 0..1;
    CNPDB2 : 0..1;
    CNPDB3 : 0..1;
    CNPDB4 : 0..1;
    CNPDB5 : 0..1;
    CNPDB6 : 0..1;
    CNPDB7 : 0..1;
    CNPDB8 : 0..1;
    CNPDB9 : 0..1;
    CNPDB10 : 0..1;
    CNPDB11 : 0..1;
    CNPDB12 : 0..1;
    CNPDB13 : 0..1;
    CNPDB14 : 0..1;
    CNPDB15 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  CNPDBbits: TCNPDBbits absolute ($BF886160);
  CNPDBCLR : longWord absolute ($BF886164);
  CNPDBSET : longWord absolute ($BF886168);
  CNPDBINV : longWord absolute ($BF88616C);
  CNCONB : longWord absolute ($BF886170);
type
  TCNCONBbits = bitpacked record
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
  CNCONBbits: TCNCONBbits absolute ($BF886170);
  CNCONBCLR : longWord absolute ($BF886174);
  CNCONBSET : longWord absolute ($BF886178);
  CNCONBINV : longWord absolute ($BF88617C);
  CNENB : longWord absolute ($BF886180);
type
  TCNENBbits = bitpacked record
  case integer of
  0 : (
    CNIEB0 : 0..1;
    CNIEB1 : 0..1;
    CNIEB2 : 0..1;
    CNIEB3 : 0..1;
    CNIEB4 : 0..1;
    CNIEB5 : 0..1;
    CNIEB6 : 0..1;
    CNIEB7 : 0..1;
    CNIEB8 : 0..1;
    CNIEB9 : 0..1;
    CNIEB10 : 0..1;
    CNIEB11 : 0..1;
    CNIEB12 : 0..1;
    CNIEB13 : 0..1;
    CNIEB14 : 0..1;
    CNIEB15 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  CNENBbits: TCNENBbits absolute ($BF886180);
  CNENBCLR : longWord absolute ($BF886184);
  CNENBSET : longWord absolute ($BF886188);
  CNENBINV : longWord absolute ($BF88618C);
  CNSTATB : longWord absolute ($BF886190);
type
  TCNSTATBbits = bitpacked record
  case integer of
  0 : (
    CNSTATB0 : 0..1;
    CNSTATB1 : 0..1;
    CNSTATB2 : 0..1;
    CNSTATB3 : 0..1;
    CNSTATB4 : 0..1;
    CNSTATB5 : 0..1;
    CNSTATB6 : 0..1;
    CNSTATB7 : 0..1;
    CNSTATB8 : 0..1;
    CNSTATB9 : 0..1;
    CNSTATB10 : 0..1;
    CNSTATB11 : 0..1;
    CNSTATB12 : 0..1;
    CNSTATB13 : 0..1;
    CNSTATB14 : 0..1;
    CNSTATB15 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  CNSTATBbits: TCNSTATBbits absolute ($BF886190);
  CNSTATBCLR : longWord absolute ($BF886194);
  CNSTATBSET : longWord absolute ($BF886198);
  CNSTATBINV : longWord absolute ($BF88619C);
  ANSELC : longWord absolute ($BF886200);
type
  TANSELCbits = bitpacked record
  case integer of
  0 : (
    ANSC0 : 0..1;
    ANSC1 : 0..1;
    RESERVED0 : 0..1;
    ANSC3 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  ANSELCbits: TANSELCbits absolute ($BF886200);
  ANSELCCLR : longWord absolute ($BF886204);
  ANSELCSET : longWord absolute ($BF886208);
  ANSELCINV : longWord absolute ($BF88620C);
  TRISC : longWord absolute ($BF886210);
type
  TTRISCbits = bitpacked record
  case integer of
  0 : (
    TRISC0 : 0..1;
    TRISC1 : 0..1;
    RESERVED0 : 0..1;
    TRISC3 : 0..1;
    RESERVED1 : 0..31;
    TRISC9 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  TRISCbits: TTRISCbits absolute ($BF886210);
  TRISCCLR : longWord absolute ($BF886214);
  TRISCSET : longWord absolute ($BF886218);
  TRISCINV : longWord absolute ($BF88621C);
  PORTC : longWord absolute ($BF886220);
type
  TPORTCbits = bitpacked record
  case integer of
  0 : (
    RC0 : 0..1;
    RC1 : 0..1;
    RESERVED0 : 0..1;
    RC3 : 0..1;
    RESERVED1 : 0..31;
    RC9 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  PORTCbits: TPORTCbits absolute ($BF886220);
  PORTCCLR : longWord absolute ($BF886224);
  PORTCSET : longWord absolute ($BF886228);
  PORTCINV : longWord absolute ($BF88622C);
  LATC : longWord absolute ($BF886230);
type
  TLATCbits = bitpacked record
  case integer of
  0 : (
    LATC0 : 0..1;
    LATC1 : 0..1;
    RESERVED0 : 0..1;
    LATC3 : 0..1;
    RESERVED1 : 0..31;
    LATC9 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  LATCbits: TLATCbits absolute ($BF886230);
  LATCCLR : longWord absolute ($BF886234);
  LATCSET : longWord absolute ($BF886238);
  LATCINV : longWord absolute ($BF88623C);
  ODCC : longWord absolute ($BF886240);
type
  TODCCbits = bitpacked record
  case integer of
  0 : (
    RESERVED0 : 0..511;
    ODCC9 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  ODCCbits: TODCCbits absolute ($BF886240);
  ODCCCLR : longWord absolute ($BF886244);
  ODCCSET : longWord absolute ($BF886248);
  ODCCINV : longWord absolute ($BF88624C);
  CNPUC : longWord absolute ($BF886250);
type
  TCNPUCbits = bitpacked record
  case integer of
  0 : (
    CNPUC0 : 0..1;
    CNPUC1 : 0..1;
    RESERVED0 : 0..1;
    CNPUC3 : 0..1;
    RESERVED1 : 0..31;
    CNPUC9 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  CNPUCbits: TCNPUCbits absolute ($BF886250);
  CNPUCCLR : longWord absolute ($BF886254);
  CNPUCSET : longWord absolute ($BF886258);
  CNPUCINV : longWord absolute ($BF88625C);
  CNPDC : longWord absolute ($BF886260);
type
  TCNPDCbits = bitpacked record
  case integer of
  0 : (
    CNPDC0 : 0..1;
    CNPDC1 : 0..1;
    RESERVED0 : 0..1;
    CNPDC3 : 0..1;
    RESERVED1 : 0..31;
    CNPDC9 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  CNPDCbits: TCNPDCbits absolute ($BF886260);
  CNPDCCLR : longWord absolute ($BF886264);
  CNPDCSET : longWord absolute ($BF886268);
  CNPDCINV : longWord absolute ($BF88626C);
  CNCONC : longWord absolute ($BF886270);
type
  TCNCONCbits = bitpacked record
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
  CNCONCbits: TCNCONCbits absolute ($BF886270);
  CNCONCCLR : longWord absolute ($BF886274);
  CNCONCSET : longWord absolute ($BF886278);
  CNCONCINV : longWord absolute ($BF88627C);
  CNENC : longWord absolute ($BF886280);
type
  TCNENCbits = bitpacked record
  case integer of
  0 : (
    CNIEC0 : 0..1;
    CNIEC1 : 0..1;
    RESERVED0 : 0..1;
    CNIEC3 : 0..1;
    RESERVED1 : 0..31;
    CNIEC9 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  CNENCbits: TCNENCbits absolute ($BF886280);
  CNENCCLR : longWord absolute ($BF886284);
  CNENCSET : longWord absolute ($BF886288);
  CNENCINV : longWord absolute ($BF88628C);
  CNSTATC : longWord absolute ($BF886290);
type
  TCNSTATCbits = bitpacked record
  case integer of
  0 : (
    CNSTATC0 : 0..1;
    CNSTATC1 : 0..1;
    RESERVED0 : 0..1;
    CNSTATC3 : 0..1;
    RESERVED1 : 0..31;
    CNSTATC9 : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  CNSTATCbits: TCNSTATCbits absolute ($BF886290);
  CNSTATCCLR : longWord absolute ($BF886294);
  CNSTATCSET : longWord absolute ($BF886298);
  CNSTATCINV : longWord absolute ($BF88629C);
  DEVCFG3 : longWord absolute ($BFC00BF0);
type
  TDEVCFG3bits = bitpacked record
  case integer of
  0 : (
    USERID : 0..65535;
    RESERVED0 : 0..4095;
    PMDL1WAY : 0..1;
    IOL1WAY : 0..1;
    FUSBIDIO : 0..1;
    FVBUSONIO : 0..1;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  DEVCFG3bits: TDEVCFG3bits absolute ($BFC00BF0);
  DEVCFG2 : longWord absolute ($BFC00BF4);
type
  TDEVCFG2bits = bitpacked record
  case integer of
  0 : (
    FPLLIDIV : 0..7;
    RESERVED0 : 0..1;
    FPLLMUL : 0..7;
    RESERVED1 : 0..511;
    FPLLODIV : 0..7;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  DEVCFG2bits: TDEVCFG2bits absolute ($BFC00BF4);
  DEVCFG1 : longWord absolute ($BFC00BF8);
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
    RESERVED3 : 0..1;
    WINDIS : 0..1;
    FWDTEN : 0..1;
    FWDTWINSZ : 0..3;
  );
  1 : (
    w : 0..4294967295;
  );
  end;
var
  DEVCFG1bits: TDEVCFG1bits absolute ($BFC00BF8);
  DEVCFG0 : longWord absolute ($BFC00BFC);
type
  TDEVCFG0bits = bitpacked record
  case integer of
  0 : (
    DEBUG : 0..3;
    JTAGEN : 0..1;
    ICESEL : 0..3;
    RESERVED0 : 0..31;
    PWP : 0..63;
    RESERVED1 : 0..255;
    BWP : 0..1;
    RESERVED2 : 0..7;
    CP : 0..1;
    RESERVED3 : 0..3;
    RESERVED : 0..1;
  );
  1 : (
    FDEBUG : 0..3;
  );
  2 : (
    w : 0..4294967295;
  );
  end;
var
  DEVCFG0bits: TDEVCFG0bits absolute ($BFC00BFC);
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
ADC_VECTOR = 23;
FAIL_SAFE_MONITOR_VECTOR = 24;
RTCC_VECTOR = 25;
FCE_VECTOR = 26;
COMPARATOR_1_VECTOR = 27;
COMPARATOR_2_VECTOR = 28;
COMPARATOR_3_VECTOR = 29;
SPI_1_VECTOR = 31;
UART_1_VECTOR = 32;
I2C_1_VECTOR = 33;
CHANGE_NOTICE_VECTOR = 34;
PMP_VECTOR = 35;
SPI_2_VECTOR = 36;
UART_2_VECTOR = 37;
I2C_2_VECTOR = 38;
CTMU_VECTOR = 39;
DMA_0_VECTOR = 40;
DMA_1_VECTOR = 41;
DMA_2_VECTOR = 42;
DMA_3_VECTOR = 43;
(* IRQ Numbers *)
CORE_TIMER_IRQ = 0;
CORE_SOFTWARE_0_IRQ = 1;
CORE_SOFTWARE_1_IRQ = 2;
EXTERNAL_0_IRQ = 3;
TIMER_1_IRQ = 4;
INPUT_CAPTURE_ERROR_1_IRQ = 5;
INPUT_CAPTURE_1_IRQ = 6;
OUTPUT_COMPARE_1_IRQ = 7;
EXTERNAL_1_IRQ = 8;
TIMER_2_IRQ = 9;
INPUT_CAPTURE_ERROR_2_IRQ = 10;
INPUT_CAPTURE_2_IRQ = 11;
OUTPUT_COMPARE_2_IRQ = 12;
EXTERNAL_2_IRQ = 13;
TIMER_3_IRQ = 14;
INPUT_CAPTURE_ERROR_3_IRQ = 15;
INPUT_CAPTURE_3_IRQ = 16;
OUTPUT_COMPARE_3_IRQ = 17;
EXTERNAL_3_IRQ = 18;
TIMER_4_IRQ = 19;
INPUT_CAPTURE_ERROR_4_IRQ = 20;
INPUT_CAPTURE_4_IRQ = 21;
OUTPUT_COMPARE_4_IRQ = 22;
EXTERNAL_4_IRQ = 23;
TIMER_5_IRQ = 24;
INPUT_CAPTURE_ERROR_5_IRQ = 25;
INPUT_CAPTURE_5_IRQ = 26;
OUTPUT_COMPARE_5_IRQ = 27;
ADC_IRQ = 28;
FAIL_SAFE_MONITOR_IRQ = 29;
RTCC_IRQ = 30;
FLASH_CONTROL_IRQ = 31;
COMPARATOR_1_IRQ = 32;
COMPARATOR_2_IRQ = 33;
COMPARATOR_3_IRQ = 34;
USB_IRQ = 35;
SPI1_ERR_IRQ = 36;
SPI1_RX_IRQ = 37;
SPI1_TX_IRQ = 38;
UART1_ERR_IRQ = 39;
UART1_RX_IRQ = 40;
UART1_TX_IRQ = 41;
I2C1_BUS_IRQ = 42;
I2C1_SLAVE_IRQ = 43;
I2C1_MASTER_IRQ = 44;
CHANGE_NOTICE_A_IRQ = 45;
CHANGE_NOTICE_B_IRQ = 46;
CHANGE_NOTICE_C_IRQ = 47;
PMP_IRQ = 48;
PMP_ERROR_IRQ = 49;
SPI2_ERR_IRQ = 50;
SPI2_RX_IRQ = 51;
SPI2_TX_IRQ = 52;
UART2_ERR_IRQ = 53;
UART2_RX_IRQ = 54;
UART2_TX_IRQ = 55;
I2C2_BUS_IRQ = 56;
I2C2_SLAVE_IRQ = 57;
I2C2_MASTER_IRQ = 58;
CTMU_IRQ = 59;
DMA0_IRQ = 60;
DMA1_IRQ = 61;
DMA2_IRQ = 62;
DMA3_IRQ = 63;
(* Device Peripherals *)
(* Base Addresses for Peripherals *)
ADC10_BASE_ADDRESS = $BF809000;
BMX_BASE_ADDRESS = $BF882000;
CFG_BASE_ADDRESS = $BF80F200;
CMP_BASE_ADDRESS = $BF80A000;
CVR_BASE_ADDRESS = $BF809800;
DMAC_BASE_ADDRESS = $BF883000;
DMAC0_BASE_ADDRESS = $BF883060;
DMAC1_BASE_ADDRESS = $BF883120;
DMAC2_BASE_ADDRESS = $BF8831E0;
DMAC3_BASE_ADDRESS = $BF8832A0;
I2C1_BASE_ADDRESS = $BF805000;
I2C2_BASE_ADDRESS = $BF805100;
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
PMP_BASE_ADDRESS = $BF807000;
PORTA_BASE_ADDRESS = $BF886000;
PORTB_BASE_ADDRESS = $BF886100;
PORTC_BASE_ADDRESS = $BF886200;
RCON_BASE_ADDRESS = $BF80F600;
RTCC_BASE_ADDRESS = $BF800200;
SPI1_BASE_ADDRESS = $BF805800;
SPI2_BASE_ADDRESS = $BF805A00;
TMR1_BASE_ADDRESS = $BF800600;
TMR2_BASE_ADDRESS = $BF800800;
TMR3_BASE_ADDRESS = $BF800A00;
TMR4_BASE_ADDRESS = $BF800C00;
TMR5_BASE_ADDRESS = $BF800E00;
UART1_BASE_ADDRESS = $BF806000;
UART2_BASE_ADDRESS = $BF806200;
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
  procedure ADC_interrupt external name 'ADC_interrupt';
  procedure FAIL_SAFE_MONITOR_interrupt external name 'FAIL_SAFE_MONITOR_interrupt';
  procedure RTCC_interrupt external name 'RTCC_interrupt';
  procedure FCE_interrupt external name 'FCE_interrupt';
  procedure COMPARATOR_1_interrupt external name 'COMPARATOR_1_interrupt';
  procedure COMPARATOR_2_interrupt external name 'COMPARATOR_2_interrupt';
  procedure COMPARATOR_3_interrupt external name 'COMPARATOR_3_interrupt';
  procedure SPI_1_interrupt external name 'SPI_1_interrupt';
  procedure UART_1_interrupt external name 'UART_1_interrupt';
  procedure I2C_1_interrupt external name 'I2C_1_interrupt';
  procedure CHANGE_NOTICE_interrupt external name 'CHANGE_NOTICE_interrupt';
  procedure PMP_interrupt external name 'PMP_interrupt';
  procedure SPI_2_interrupt external name 'SPI_2_interrupt';
  procedure UART_2_interrupt external name 'UART_2_interrupt';
  procedure I2C_2_interrupt external name 'I2C_2_interrupt';
  procedure CTMU_interrupt external name 'CTMU_interrupt';
  procedure DMA_0_interrupt external name 'DMA_0_interrupt';
  procedure DMA_1_interrupt external name 'DMA_1_interrupt';
  procedure DMA_2_interrupt external name 'DMA_2_interrupt';
  procedure DMA_3_interrupt external name 'DMA_3_interrupt';

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
    j ADC_interrupt
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    j FAIL_SAFE_MONITOR_interrupt
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    j RTCC_interrupt
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    j FCE_interrupt
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    j COMPARATOR_1_interrupt
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    j COMPARATOR_2_interrupt
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    j COMPARATOR_3_interrupt
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
    j SPI_1_interrupt
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    j UART_1_interrupt
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    j I2C_1_interrupt
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    j CHANGE_NOTICE_interrupt
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    j PMP_interrupt
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    j SPI_2_interrupt
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    j UART_2_interrupt
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    j I2C_2_interrupt
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    j CTMU_interrupt
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    j DMA_0_interrupt
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    j DMA_1_interrupt
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    j DMA_2_interrupt
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    j DMA_3_interrupt
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
    .weak ADC_interrupt
    .weak FAIL_SAFE_MONITOR_interrupt
    .weak RTCC_interrupt
    .weak FCE_interrupt
    .weak COMPARATOR_1_interrupt
    .weak COMPARATOR_2_interrupt
    .weak COMPARATOR_3_interrupt
    .weak SPI_1_interrupt
    .weak UART_1_interrupt
    .weak I2C_1_interrupt
    .weak CHANGE_NOTICE_interrupt
    .weak PMP_interrupt
    .weak SPI_2_interrupt
    .weak UART_2_interrupt
    .weak I2C_2_interrupt
    .weak CTMU_interrupt
    .weak DMA_0_interrupt
    .weak DMA_1_interrupt
    .weak DMA_2_interrupt
    .weak DMA_3_interrupt

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
    .set ADC_interrupt , DefaultInterrupt
    .set FAIL_SAFE_MONITOR_interrupt , DefaultInterrupt
    .set RTCC_interrupt , DefaultInterrupt
    .set FCE_interrupt , DefaultInterrupt
    .set COMPARATOR_1_interrupt , DefaultInterrupt
    .set COMPARATOR_2_interrupt , DefaultInterrupt
    .set COMPARATOR_3_interrupt , DefaultInterrupt
    .set SPI_1_interrupt , DefaultInterrupt
    .set UART_1_interrupt , DefaultInterrupt
    .set I2C_1_interrupt , DefaultInterrupt
    .set CHANGE_NOTICE_interrupt , DefaultInterrupt
    .set PMP_interrupt , DefaultInterrupt
    .set SPI_2_interrupt , DefaultInterrupt
    .set UART_2_interrupt , DefaultInterrupt
    .set I2C_2_interrupt , DefaultInterrupt
    .set CTMU_interrupt , DefaultInterrupt
    .set DMA_0_interrupt , DefaultInterrupt
    .set DMA_1_interrupt , DefaultInterrupt
    .set DMA_2_interrupt , DefaultInterrupt
    .set DMA_3_interrupt , DefaultInterrupt
    .text
  end;
end.
