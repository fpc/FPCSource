{$mode objfpc}

unit avxopcodes;

interface

uses Classes;

type

  TTestFileTyp = (tfNasm, tfFPC);

  TAVXTestGenerator = class(TObject)
  private
    FOpCodeList: TStringList;
  protected
    procedure Init;

    function InternalMakeTestFiles(aX64: boolean; aDestPath, aFileExt: String; aOpCodeList, aHeaderList, aFooterList: TStringList): boolean;

  public
    constructor Create;
    destructor Destroy; override;

    function MakeTestFiles(aTyp: TTestFileTyp; aX64: boolean; aDestPath: String): boolean;

    property OpCodeList: TStringList read FOpCodeList write FOpCodeList;
  end;

implementation

uses SysUtils, AsmTestGenerator;

{ TAVXTestGenerator }

constructor TAVXTestGenerator.Create;
begin
  inherited;

  FOpCodeList := TStringList.Create;

  Init;
end;

destructor TAVXTestGenerator.Destroy;
begin
  FreeAndNil(FOpCodeList);

  inherited;
end;

procedure TAVXTestGenerator.Init;
begin
  FOpCodeList.Add('VADDPD,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VADDPD,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VADDPS,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VADDPS,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VADDSD,1,1,XMMREG,XMMREG,MEM64,');
  FOpCodeList.Add('VADDSD,1,1,XMMREG,XMMREG,XMMREG,');
  FOpCodeList.Add('VADDSS,1,1,XMMREG,XMMREG,MEM32,');
  FOpCodeList.Add('VADDSS,1,1,XMMREG,XMMREG,XMMREG,');
  FOpCodeList.Add('VADDSUBPD,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VADDSUBPD,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VADDSUBPS,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VADDSUBPS,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VAESDEC,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VAESDECLAST,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VAESENC,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VAESENCLAST,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VAESIMC,1,1,XMMREG,XMMRM,,');
  FOpCodeList.Add('VAESKEYGENASSIST,1,1,XMMREG,XMMRM,IMM8,');
  FOpCodeList.Add('VANDNPD,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VANDNPD,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VANDNPS,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VANDNPS,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VANDPD,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VANDPD,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VANDPS,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VANDPS,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VBLENDPD,1,1,XMMREG,XMMREG,XMMRM,IMM8');
  FOpCodeList.Add('VBLENDPD,1,1,YMMREG,YMMREG,YMMRM,IMM8');
  FOpCodeList.Add('VBLENDPS,1,1,XMMREG,XMMREG,XMMRM,IMM8');
  FOpCodeList.Add('VBLENDPS,1,1,YMMREG,YMMREG,YMMRM,IMM8');
  FOpCodeList.Add('VBLENDVPD,1,1,XMMREG,XMMREG,XMMRM,XMMREG');
  FOpCodeList.Add('VBLENDVPD,1,1,YMMREG,YMMREG,YMMRM,YMMREG');
  FOpCodeList.Add('VBLENDVPS,1,1,XMMREG,XMMREG,XMMRM,XMMREG');
  FOpCodeList.Add('VBLENDVPS,1,1,YMMREG,YMMREG,YMMRM,YMMREG');
  FOpCodeList.Add('VBROADCASTF128,1,1,YMMREG,MEM128,,');
  FOpCodeList.Add('VBROADCASTSD,1,1,YMMREG,MEM64,,');
  FOpCodeList.Add('VBROADCASTSS,1,1,YMMREG,MEM32,,');
  FOpCodeList.Add('VBROADCASTSS,1,1,XMMREG,MEM32,,');
  FOpCodeList.Add('VCMPPD,1,1,XMMREG,XMMREG,XMMRM,IMM8');
  FOpCodeList.Add('VCMPPD,1,1,YMMREG,YMMREG,YMMRM,IMM8');
  FOpCodeList.Add('VCMPPS,1,1,XMMREG,XMMREG,XMMRM,IMM8');
  FOpCodeList.Add('VCMPPS,1,1,YMMREG,YMMREG,YMMRM,IMM8');
  FOpCodeList.Add('VCMPSD,1,1,XMMREG,XMMREG,MEM64,IMM8');
  FOpCodeList.Add('VCMPSD,1,1,XMMREG,XMMREG,XMMREG,IMM8');
  FOpCodeList.Add('VCMPSS,1,1,XMMREG,XMMREG,MEM64,IMM8');
  FOpCodeList.Add('VCMPSS,1,1,XMMREG,XMMREG,XMMREG,IMM8');
  FOpCodeList.Add('VCOMISD,1,1,XMMREG,MEM64,,');
  FOpCodeList.Add('VCOMISD,1,1,XMMREG,XMMREG,,');
  FOpCodeList.Add('VCOMISS,1,1,XMMREG,MEM32,,');
  FOpCodeList.Add('VCOMISS,1,1,XMMREG,XMMREG,,');
  FOpCodeList.Add('VCVTDQ2PD,1,1,XMMREG,MEM64,,');
  FOpCodeList.Add('VCVTDQ2PD,1,1,XMMREG,XMMREG,,');
  FOpCodeList.Add('VCVTDQ2PD,1,1,YMMREG,XMMRM,,');
  FOpCodeList.Add('VCVTDQ2PS,1,1,XMMREG,XMMRM,,');
  FOpCodeList.Add('VCVTDQ2PS,1,1,YMMREG,YMMRM,,');
  FOpCodeList.Add('VCVTPD2DQ,1,1,XMMREG,XMMRM,,');
  FOpCodeList.Add('VCVTPD2DQ,1,1,XMMREG,YMMRM,,');
  FOpCodeList.Add('VCVTPD2PS,1,1,XMMREG,XMMRM,,');
  FOpCodeList.Add('VCVTPD2PS,1,1,XMMREG,YMMRM,,');
  FOpCodeList.Add('VCVTPS2DQ,1,1,XMMREG,XMMRM,,');
  FOpCodeList.Add('VCVTPS2DQ,1,1,YMMREG,YMMRM,,');
  FOpCodeList.Add('VCVTPS2PD,1,1,XMMREG,MEM64,,');
  FOpCodeList.Add('VCVTPS2PD,1,1,XMMREG,XMMREG,,');
  FOpCodeList.Add('VCVTPS2PD,1,1,YMMREG,XMMRM,,');
  FOpCodeList.Add('VCVTSD2SI,1,1,REG32,MEM64,,');
  FOpCodeList.Add('VCVTSD2SI,1,1,REG32,XMMREG,,');
  FOpCodeList.Add('VCVTSD2SI,0,1,REG64,MEM64,,');
  FOpCodeList.Add('VCVTSD2SI,0,1,REG64,XMMREG,,');
  FOpCodeList.Add('VCVTSD2SS,1,1,XMMREG,XMMREG,MEM64,');
  FOpCodeList.Add('VCVTSD2SS,1,1,XMMREG,XMMREG,XMMREG,');
  FOpCodeList.Add('VCVTSI2SD,1,1,XMMREG,XMMREG,RM32,');
  FOpCodeList.Add('VCVTSI2SD,0,1,XMMREG,XMMREG,RM64,');
  FOpCodeList.Add('VCVTSI2SS,1,1,XMMREG,XMMREG,RM32,');
  FOpCodeList.Add('VCVTSI2SS,0,1,XMMREG,XMMREG,RM64,');
  FOpCodeList.Add('VCVTSS2SD,1,1,XMMREG,XMMREG,MEM32,');
  FOpCodeList.Add('VCVTSS2SD,1,1,XMMREG,XMMREG,XMMREG,');
  FOpCodeList.Add('VCVTSS2SI,1,1,REG32,MEM32,,');
  FOpCodeList.Add('VCVTSS2SI,1,1,REG32,XMMREG,,');
  FOpCodeList.Add('VCVTSS2SI,0,1,REG64,MEM32,,');
  FOpCodeList.Add('VCVTSS2SI,0,1,REG64,XMMREG,,');
  FOpCodeList.Add('VCVTTPD2DQ,1,1,XMMREG,XMMRM,,');
  FOpCodeList.Add('VCVTTPD2DQ,1,1,XMMREG,YMMRM,,');
  FOpCodeList.Add('VCVTTPS2DQ,1,1,XMMREG,XMMRM,,');
  FOpCodeList.Add('VCVTTPS2DQ,1,1,YMMREG,YMMRM,,');
  FOpCodeList.Add('VCVTTSD2SI,1,1,REG32,MEM64,,');
  FOpCodeList.Add('VCVTTSD2SI,1,1,REG32,XMMREG,,');
  FOpCodeList.Add('VCVTTSD2SI,0,1,REG64,MEM64,,');
  FOpCodeList.Add('VCVTTSD2SI,0,1,REG64,XMMREG,,');
  FOpCodeList.Add('VCVTTSS2SI,1,1,REG32,MEM32,,');
  FOpCodeList.Add('VCVTTSS2SI,1,1,REG32,XMMREG,,');
  FOpCodeList.Add('VCVTTSS2SI,0,1,REG64,MEM32,,');
  FOpCodeList.Add('VCVTTSS2SI,0,1,REG64,XMMREG,,');
  FOpCodeList.Add('VDIVPD,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VDIVPD,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VDIVPS,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VDIVPS,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VDIVSD,1,1,XMMREG,XMMREG,MEM64,');
  FOpCodeList.Add('VDIVSD,1,1,XMMREG,XMMREG,XMMREG,');
  FOpCodeList.Add('VDIVSS,1,1,XMMREG,XMMREG,MEM32,');
  FOpCodeList.Add('VDIVSS,1,1,XMMREG,XMMREG,XMMREG,');
  FOpCodeList.Add('VDPPD,1,1,XMMREG,XMMREG,XMMRM,IMM8');
  FOpCodeList.Add('VDPPS,1,1,XMMREG,XMMREG,XMMRM,IMM8');
  FOpCodeList.Add('VDPPS,1,1,YMMREG,YMMREG,YMMRM,IMM8');
  FOpCodeList.Add('VEXTRACTF128,1,1,XMMRM,YMMREG,IMM8,');
  FOpCodeList.Add('VEXTRACTPS,1,1,RM32,XMMREG,IMM8,');
  FOpCodeList.Add('VHADDPD,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VHADDPD,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VHADDPS,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VHADDPS,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VHSUBPD,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VHSUBPD,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VHSUBPS,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VHSUBPS,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VINSERTF128,1,1,YMMREG,YMMREG,XMMRM,IMM8');
  FOpCodeList.Add('VINSERTPS,1,1,XMMREG,XMMREG,MEM32,IMM8');
  FOpCodeList.Add('VINSERTPS,1,1,XMMREG,XMMREG,XMMREG,IMM8');
  FOpCodeList.Add('VLDDQU,1,1,XMMREG,MEM128,,');
  FOpCodeList.Add('VLDDQU,1,1,YMMREG,MEM256,,');
  FOpCodeList.Add('VLDMXCSR,1,1,MEM32,,,');
  FOpCodeList.Add('VMASKMOVDQU,1,1,XMMREG,XMMREG,,');
  FOpCodeList.Add('VMASKMOVPD,1,1,MEM256,YMMREG,YMMREG,');
  FOpCodeList.Add('VMASKMOVPD,1,1,MEM128,XMMREG,XMMREG,');
  FOpCodeList.Add('VMASKMOVPD,1,1,YMMREG,YMMREG,MEM256,');
  FOpCodeList.Add('VMASKMOVPD,1,1,XMMREG,XMMREG,MEM128,');
  FOpCodeList.Add('VMASKMOVPS,1,1,MEM256,YMMREG,YMMREG,');
  FOpCodeList.Add('VMASKMOVPS,1,1,MEM128,XMMREG,XMMREG,');
  FOpCodeList.Add('VMASKMOVPS,1,1,YMMREG,YMMREG,MEM256,');
  FOpCodeList.Add('VMASKMOVPS,1,1,XMMREG,XMMREG,MEM128,');
  FOpCodeList.Add('VMAXPD,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VMAXPD,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VMAXPS,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VMAXPS,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VMAXSD,1,1,XMMREG,XMMREG,MEM64,');
  FOpCodeList.Add('VMAXSD,1,1,XMMREG,XMMREG,XMMREG,');
  FOpCodeList.Add('VMAXSS,1,1,XMMREG,XMMREG,MEM32,');
  FOpCodeList.Add('VMAXSS,1,1,XMMREG,XMMREG,XMMREG,');
  FOpCodeList.Add('VMINPD,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VMINPD,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VMINPS,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VMINPS,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VMINSD,1,1,XMMREG,XMMREG,MEM64,');
  FOpCodeList.Add('VMINSD,1,1,XMMREG,XMMREG,XMMREG,');
  FOpCodeList.Add('VMINSS,1,1,XMMREG,XMMREG,MEM32,');
  FOpCodeList.Add('VMINSS,1,1,XMMREG,XMMREG,XMMREG,');
  FOpCodeList.Add('VMOVAPD,1,1,XMMREG,XMMRM,,');
  FOpCodeList.Add('VMOVAPD,1,1,XMMRM,XMMREG,,');
  FOpCodeList.Add('VMOVAPD,1,1,YMMREG,YMMRM,,');
  FOpCodeList.Add('VMOVAPD,1,1,YMMRM,YMMREG,,');
  FOpCodeList.Add('VMOVAPS,1,1,XMMREG,XMMRM,,');
  FOpCodeList.Add('VMOVAPS,1,1,XMMRM,XMMREG,,');
  FOpCodeList.Add('VMOVAPS,1,1,YMMREG,YMMRM,,');
  FOpCodeList.Add('VMOVAPS,1,1,YMMRM,YMMREG,,');
  FOpCodeList.Add('VMOVD,1,1,XMMREG,RM32,,');
  FOpCodeList.Add('VMOVD,1,1,RM32,XMMREG,,');
  FOpCodeList.Add('VMOVDDUP,1,1,YMMREG,YMMRM,,');
  FOpCodeList.Add('VMOVDDUP,1,1,XMMREG,MEM64,,');
  FOpCodeList.Add('VMOVDDUP,1,1,XMMREG,XMMREG,,');
  FOpCodeList.Add('VMOVDQA,1,1,YMMRM,YMMREG,,');
  FOpCodeList.Add('VMOVDQA,1,1,XMMREG,XMMRM,,');
  FOpCodeList.Add('VMOVDQA,1,1,XMMRM,XMMREG,,');
  FOpCodeList.Add('VMOVDQA,1,1,YMMREG,YMMRM,,');
  FOpCodeList.Add('VMOVDQU,1,1,YMMREG,YMMRM,,');
  FOpCodeList.Add('VMOVDQU,1,1,XMMREG,XMMRM,,');
  FOpCodeList.Add('VMOVDQU,1,1,XMMRM,XMMREG,,');
  FOpCodeList.Add('VMOVDQU,1,1,YMMRM,YMMREG,,');
  FOpCodeList.Add('VMOVHLPS,1,1,XMMREG,XMMREG,XMMREG,');
  FOpCodeList.Add('VMOVHPD,1,1,MEM64,XMMREG,,');
  FOpCodeList.Add('VMOVHPD,1,1,XMMREG,XMMREG,MEM64,');
  FOpCodeList.Add('VMOVHPS,1,1,XMMREG,XMMREG,MEM64,');
  FOpCodeList.Add('VMOVHPS,1,1,MEM64,XMMREG,,');
  FOpCodeList.Add('VMOVLHPS,1,1,XMMREG,XMMREG,XMMREG,');
  FOpCodeList.Add('VMOVLPD,1,1,MEM64,XMMREG,,');
  FOpCodeList.Add('VMOVLPD,1,1,XMMREG,XMMREG,MEM64,');
  FOpCodeList.Add('VMOVLPS,1,1,MEM64,XMMREG,,');
  FOpCodeList.Add('VMOVLPS,1,1,XMMREG,XMMREG,MEM64,');
  FOpCodeList.Add('VMOVMSKPD,1,1,REG32,YMMREG,,');
  FOpCodeList.Add('VMOVMSKPD,1,1,REG64,XMMREG,,');
  FOpCodeList.Add('VMOVMSKPD,1,1,REG32,XMMREG,,');
  FOpCodeList.Add('VMOVMSKPD,1,1,REG64,YMMREG,,');
  FOpCodeList.Add('VMOVMSKPS,1,1,REG32,YMMREG,,');
  FOpCodeList.Add('VMOVMSKPS,1,1,REG64,XMMREG,,');
  FOpCodeList.Add('VMOVMSKPS,1,1,REG32,XMMREG,,');
  FOpCodeList.Add('VMOVMSKPS,1,1,REG64,YMMREG,,');
  FOpCodeList.Add('VMOVNTDQ,1,1,MEM128,XMMREG,,');
  FOpCodeList.Add('VMOVNTDQ,1,1,MEM256,YMMREG,,');
  FOpCodeList.Add('VMOVNTDQA,1,1,XMMREG,MEM128,,');
  FOpCodeList.Add('VMOVNTPD,1,1,MEM256,YMMREG,,');
  FOpCodeList.Add('VMOVNTPD,1,1,MEM128,XMMREG,,');
  FOpCodeList.Add('VMOVNTPS,1,1,MEM128,YMMREG,,');
  FOpCodeList.Add('VMOVNTPS,1,1,MEM128,XMMREG,,');
  FOpCodeList.Add('VMOVQ,0,1,RM64,XMMREG,,');
  FOpCodeList.Add('VMOVQ,0,1,XMMREG,RM64,,');
  FOpCodeList.Add('VMOVSD,1,1,XMMREG,XMMREG,XMMREG,');
  FOpCodeList.Add('VMOVSD,1,1,XMMREG,MEM64,,');
  FOpCodeList.Add('VMOVSD,1,1,XMMREG,XMMREG,XMMREG,');
  FOpCodeList.Add('VMOVSD,1,1,MEM64,XMMREG,,');
  FOpCodeList.Add('VMOVSHDUP,1,1,XMMREG,XMMRM,,');
  FOpCodeList.Add('VMOVSHDUP,1,1,YMMREG,YMMRM,,');
  FOpCodeList.Add('VMOVSLDUP,1,1,XMMREG,XMMRM,,');
  FOpCodeList.Add('VMOVSLDUP,1,1,YMMREG,YMMRM,,');
  FOpCodeList.Add('VMOVSS,1,1,XMMREG,XMMREG,XMMREG,');
  FOpCodeList.Add('VMOVSS,1,1,XMMREG,MEM64,,');
  FOpCodeList.Add('VMOVSS,1,1,XMMREG,XMMREG,XMMREG,');
  FOpCodeList.Add('VMOVSS,1,1,MEM64,XMMREG,,');
  FOpCodeList.Add('VMOVUPD,1,1,XMMREG,XMMRM,,');
  FOpCodeList.Add('VMOVUPD,1,1,XMMRM,XMMREG,,');
  FOpCodeList.Add('VMOVUPD,1,1,YMMREG,YMMRM,,');
  FOpCodeList.Add('VMOVUPD,1,1,YMMRM,YMMREG,,');
  FOpCodeList.Add('VMOVUPS,1,1,XMMREG,XMMRM,,');
  FOpCodeList.Add('VMOVUPS,1,1,XMMRM,XMMREG,,');
  FOpCodeList.Add('VMOVUPS,1,1,YMMREG,YMMRM,,');
  FOpCodeList.Add('VMOVUPS,1,1,YMMRM,YMMREG,,');
  FOpCodeList.Add('VMPSADBW,1,1,XMMREG,XMMREG,XMMRM,IMM8');
  FOpCodeList.Add('VMULPD,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VMULPD,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VMULPS,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VMULPS,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VMULSD,1,1,XMMREG,XMMREG,MEM64,');
  FOpCodeList.Add('VMULSD,1,1,XMMREG,XMMREG,XMMREG,');
  FOpCodeList.Add('VMULSS,1,1,XMMREG,XMMREG,MEM32,');
  FOpCodeList.Add('VMULSS,1,1,XMMREG,XMMREG,XMMREG,');
  FOpCodeList.Add('VORPD,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VORPS,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VORPS,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPABSB,1,1,XMMREG,XMMRM,,');
  FOpCodeList.Add('VPABSB,1,1,YMMREG,YMMRM,,');
  FOpCodeList.Add('VPABSD,1,1,XMMREG,XMMRM,,');
  FOpCodeList.Add('VPABSD,1,1,YMMREG,YMMRM,,');
  FOpCodeList.Add('VPABSW,1,1,XMMREG,XMMRM,,');
  FOpCodeList.Add('VPABSW,1,1,YMMREG,YMMRM,,');
  FOpCodeList.Add('VPACKSSDW,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPACKSSDW,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPACKSSWB,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPACKSSWB,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPACKUSDW,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPACKUSDW,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPACKUSWB,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPACKUSWB,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPADDB,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPADDB,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPADDD,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPADDD,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPADDQ,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPADDQ,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPADDSB,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPADDSB,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPADDSW,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPADDSW,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPADDUSB,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPADDUSB,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPADDUSW,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPADDUSW,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPADDW,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPADDW,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPALIGNR,1,1,XMMREG,XMMREG,XMMRM,IMM8');
  FOpCodeList.Add('VPALIGNR,1,1,YMMREG,YMMREG,YMMRM,IMM8');
  FOpCodeList.Add('VPAND,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPAND,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPANDN,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPANDN,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPAVGB,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPAVGB,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPAVGW,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPAVGW,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPBLENDVB,1,1,XMMREG,XMMREG,XMMRM,XMMREG');
  FOpCodeList.Add('VPBLENDVB,1,1,YMMREG,YMMREG,YMMRM,YMMREG');
  FOpCodeList.Add('VPBLENDW,1,1,XMMREG,XMMREG,XMMRM,IMM8');
  FOpCodeList.Add('VPBLENDW,1,1,YMMREG,YMMREG,YMMRM,IMM8');
  FOpCodeList.Add('VPCLMULQDQ,1,1,XMMREG,XMMREG,XMMRM,IMM8');
  FOpCodeList.Add('VPCMPEQB,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPCMPEQB,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPCMPEQD,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPCMPEQD,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPCMPEQQ,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPCMPEQQ,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPCMPEQW,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPCMPEQW,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPCMPESTRI,1,1,XMMREG,XMMRM,IMM8,');
  FOpCodeList.Add('VPCMPESTRM,1,1,XMMREG,XMMRM,IMM8,');
  FOpCodeList.Add('VPCMPGTB,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPCMPGTB,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPCMPGTD,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPCMPGTD,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPCMPGTQ,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPCMPGTQ,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPCMPGTW,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPCMPGTW,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPCMPISTRI,1,1,XMMREG,XMMRM,IMM8,');
  FOpCodeList.Add('VPCMPISTRM,1,1,XMMREG,XMMRM,IMM8,');
  FOpCodeList.Add('VPERM2F128,1,1,YMMREG,YMMREG,YMMRM,IMM8');
  FOpCodeList.Add('VPERMILPD,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPERMILPD,1,1,XMMREG,XMMRM,IMM8,');
  FOpCodeList.Add('VPERMILPD,1,1,YMMREG,YMMRM,IMM8,');
  FOpCodeList.Add('VPERMILPD,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPERMILPS,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPERMILPS,1,1,XMMREG,XMMRM,IMM8,');
  FOpCodeList.Add('VPERMILPS,1,1,YMMREG,YMMRM,IMM8,');
  FOpCodeList.Add('VPERMILPS,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPEXTRB,1,1,REG32,XMMREG,IMM8,');
  FOpCodeList.Add('VPEXTRB,1,1,REG64,XMMREG,IMM8,');
  FOpCodeList.Add('VPEXTRB,1,1,MEM8,XMMREG,IMM8,');
  FOpCodeList.Add('VPEXTRD,1,1,RM32,XMMREG,IMM8,');
  FOpCodeList.Add('VPEXTRQ,0,1,RM64,XMMREG,IMM8,');
  FOpCodeList.Add('VPEXTRW,1,1,REG32,XMMREG,IMM8,');
  FOpCodeList.Add('VPEXTRW,1,1,REG64,XMMREG,IMM8,');
  FOpCodeList.Add('VPEXTRW,1,1,REG64,XMMREG,IMM8,');
  FOpCodeList.Add('VPEXTRW,1,1,MEM16,XMMREG,IMM8,');
  FOpCodeList.Add('VPEXTRW,1,1,REG32,XMMREG,IMM8,');
  FOpCodeList.Add('VPHADDD,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPHADDD,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPHADDSW,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPHADDSW,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPHADDW,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPHADDW,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPHMINPOSUW,1,1,XMMREG,XMMRM,,');
  FOpCodeList.Add('VPHSUBD,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPHSUBD,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPHSUBSW,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPHSUBSW,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPHSUBW,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPHSUBW,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPINSRB,1,1,XMMREG,XMMREG,REG32,IMM8');
  FOpCodeList.Add('VPINSRB,1,1,XMMREG,XMMREG,MEM8,IMM8');
  FOpCodeList.Add('VPINSRD,1,1,XMMREG,XMMREG,RM32,IMM8');
  FOpCodeList.Add('VPINSRQ,0,1,XMMREG,XMMREG,RM64,IMM8');
  FOpCodeList.Add('VPINSRW,1,1,XMMREG,XMMREG,REG32,IMM8');
  FOpCodeList.Add('VPINSRW,1,1,XMMREG,XMMREG,MEM16,IMM8');
  FOpCodeList.Add('VPMADDUBSW,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPMADDUBSW,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPMADDWD,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPMADDWD,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPMAXSB,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPMAXSB,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPMAXSD,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPMAXSD,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPMAXSW,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPMAXSW,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPMAXUB,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPMAXUB,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPMAXUD,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPMAXUD,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPMAXUW,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPMAXUW,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPMINSB,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPMINSB,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPMINSD,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPMINSD,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPMINSW,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPMINSW,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPMINUB,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPMINUB,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPMINUD,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPMINUD,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPMINUW,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPMINUW,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPMOVMSKB,1,1,REG64,XMMREG,,');
  FOpCodeList.Add('VPMOVMSKB,1,1,REG64,YMMREG,,');
  FOpCodeList.Add('VPMOVMSKB,1,1,REG32,XMMREG,,');
  FOpCodeList.Add('VPMOVMSKB,1,1,REG32,YMMREG,,');
  FOpCodeList.Add('VPMOVSXBD,1,1,XMMREG,MEM32,,');
  FOpCodeList.Add('VPMOVSXBD,1,1,YMMREG,MEM64,,');
  FOpCodeList.Add('VPMOVSXBD,1,1,XMMREG,XMMREG,,');
  FOpCodeList.Add('VPMOVSXBD,1,1,YMMREG,XMMREG,,');
  FOpCodeList.Add('VPMOVSXBQ,1,1,XMMREG,MEM16,,');
  FOpCodeList.Add('VPMOVSXBQ,1,1,YMMREG,MEM32,,');
  FOpCodeList.Add('VPMOVSXBQ,1,1,XMMREG,XMMREG,,');
  FOpCodeList.Add('VPMOVSXBQ,1,1,YMMREG,XMMREG,,');
  FOpCodeList.Add('VPMOVSXBW,1,1,XMMREG,MEM64,,');
  FOpCodeList.Add('VPMOVSXBW,1,1,XMMREG,XMMREG,,');
  FOpCodeList.Add('VPMOVSXBW,1,1,YMMREG,XMMRM,,');
  FOpCodeList.Add('VPMOVSXDQ,1,1,XMMREG,MEM64,,');
  FOpCodeList.Add('VPMOVSXDQ,1,1,XMMREG,XMMREG,,');
  FOpCodeList.Add('VPMOVSXDQ,1,1,YMMREG,XMMRM,,');
  FOpCodeList.Add('VPMOVSXWD,1,1,XMMREG,MEM64,,');
  FOpCodeList.Add('VPMOVSXWD,1,1,XMMREG,XMMREG,,');
  FOpCodeList.Add('VPMOVSXWD,1,1,XMMREG,XMMRM,,');
  FOpCodeList.Add('VPMOVSXWQ,1,1,XMMREG,MEM32,,');
  FOpCodeList.Add('VPMOVSXWQ,1,1,YMMREG,MEM64,,');
  FOpCodeList.Add('VPMOVSXWQ,1,1,XMMREG,XMMREG,,');
  FOpCodeList.Add('VPMOVSXWQ,1,1,YMMREG,XMMREG,,');
  FOpCodeList.Add('VPMOVZXBD,1,1,XMMREG,MEM32,,');
  FOpCodeList.Add('VPMOVZXBD,1,1,YMMREG,MEM64,,');
  FOpCodeList.Add('VPMOVZXBD,1,1,XMMREG,XMMREG,,');
  FOpCodeList.Add('VPMOVZXBD,1,1,YMMREG,XMMREG,,');
  FOpCodeList.Add('VPMOVZXBQ,1,1,XMMREG,MEM16,,');
  FOpCodeList.Add('VPMOVZXBQ,1,1,YMMREG,MEM32,,');
  FOpCodeList.Add('VPMOVZXBQ,1,1,XMMREG,XMMREG,,');
  FOpCodeList.Add('VPMOVZXBQ,1,1,YMMREG,XMMREG,,');
  FOpCodeList.Add('VPMOVZXBW,1,1,XMMREG,MEM64,,');
  FOpCodeList.Add('VPMOVZXBW,1,1,XMMREG,XMMREG,,');
  FOpCodeList.Add('VPMOVZXBW,1,1,YMMREG,XMMRM,,');
  FOpCodeList.Add('VPMOVZXDQ,1,1,XMMREG,MEM64,,');
  FOpCodeList.Add('VPMOVZXDQ,1,1,XMMREG,XMMREG,,');
  FOpCodeList.Add('VPMOVZXDQ,1,1,YMMREG,XMMRM,,');
  FOpCodeList.Add('VPMOVZXWD,1,1,XMMREG,MEM64,,');
  FOpCodeList.Add('VPMOVZXWD,1,1,XMMREG,XMMREG,,');
  FOpCodeList.Add('VPMOVZXWD,1,1,YMMREG,XMMRM,,');
  FOpCodeList.Add('VPMOVZXWQ,1,1,XMMREG,MEM32,,');
  FOpCodeList.Add('VPMOVZXWQ,1,1,YMMREG,MEM64,,');
  FOpCodeList.Add('VPMOVZXWQ,1,1,XMMREG,XMMREG,,');
  FOpCodeList.Add('VPMOVZXWQ,1,1,YMMREG,XMMREG,,');
  FOpCodeList.Add('VPMULDQ,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPMULDQ,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPMULHRSW,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPMULHRSW,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPMULHUW,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPMULHUW,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPMULHW,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPMULHW,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPMULLD,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPMULLD,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPMULLW,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPMULLW,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPMULUDQ,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPMULUDQ,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPOR,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPOR,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPSADBW,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPSADBW,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPSHUFB,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPSHUFB,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPSHUFD,1,1,XMMREG,XMMRM,IMM8,');
  FOpCodeList.Add('VPSHUFD,1,1,YMMREG,YMMRM,IMM8,');
  FOpCodeList.Add('VPSHUFHW,1,1,XMMREG,XMMRM,IMM8,');
  FOpCodeList.Add('VPSHUFHW,1,1,YMMREG,YMMRM,IMM8,');
  FOpCodeList.Add('VPSHUFLW,1,1,XMMREG,XMMRM,IMM8,');
  FOpCodeList.Add('VPSHUFLW,1,1,YMMREG,YMMRM,IMM8,');
  FOpCodeList.Add('VPSIGNB,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPSIGNB,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPSIGND,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPSIGND,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPSIGNW,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPSIGNW,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPSLLD,1,1,XMMREG,XMMREG,IMM8,');
  FOpCodeList.Add('VPSLLD,1,1,YMMREG,YMMREG,IMM8,');
  FOpCodeList.Add('VPSLLD,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPSLLD,1,1,YMMREG,YMMREG,XMMRM,');
  FOpCodeList.Add('VPSLLDQ,1,1,XMMREG,XMMREG,IMM8,');
  FOpCodeList.Add('VPSLLDQ,1,1,YMMREG,YMMREG,IMM8,');
  FOpCodeList.Add('VPSLLQ,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPSLLQ,1,1,YMMREG,YMMREG,XMMRM,');
  FOpCodeList.Add('VPSLLQ,1,1,XMMREG,XMMREG,IMM8,');
  FOpCodeList.Add('VPSLLQ,1,1,YMMREG,YMMREG,IMM8,');
  FOpCodeList.Add('VPSLLW,1,1,XMMREG,XMMREG,IMM8,');
  FOpCodeList.Add('VPSLLW,1,1,YMMREG,YMMREG,IMM8,');
  FOpCodeList.Add('VPSLLW,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPSLLW,1,1,YMMREG,YMMREG,XMMRM,');
  FOpCodeList.Add('VPSRAD,1,1,XMMREG,XMMREG,IMM8,');
  FOpCodeList.Add('VPSRAD,1,1,YMMREG,YMMREG,IMM8,');
  FOpCodeList.Add('VPSRAD,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPSRAD,1,1,YMMREG,YMMREG,XMMRM,');
  FOpCodeList.Add('VPSRAW,1,1,XMMREG,XMMREG,IMM8,');
  FOpCodeList.Add('VPSRAW,1,1,YMMREG,YMMREG,IMM8,');
  FOpCodeList.Add('VPSRAW,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPSRAW,1,1,YMMREG,YMMREG,XMMRM,');
  FOpCodeList.Add('VPSRLD,1,1,XMMREG,XMMREG,IMM8,');
  FOpCodeList.Add('VPSRLD,1,1,YMMREG,YMMREG,IMM8,');
  FOpCodeList.Add('VPSRLD,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPSRLD,1,1,YMMREG,YMMREG,XMMRM,');
  FOpCodeList.Add('VPSRLDQ,1,1,XMMREG,XMMREG,IMM8,');
  FOpCodeList.Add('VPSRLDQ,1,1,YMMREG,YMMREG,IMM8,');
  FOpCodeList.Add('VPSRLQ,1,1,XMMREG,XMMREG,IMM8,');
  FOpCodeList.Add('VPSRLQ,1,1,YMMREG,YMMREG,IMM8,');
  FOpCodeList.Add('VPSRLQ,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPSRLQ,1,1,YMMREG,YMMREG,XMMRM,');
  FOpCodeList.Add('VPSRLW,1,1,XMMREG,XMMREG,IMM8,');
  FOpCodeList.Add('VPSRLW,1,1,YMMREG,YMMREG,IMM8,');
  FOpCodeList.Add('VPSRLW,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPSRLW,1,1,YMMREG,YMMREG,XMMRM,');
  FOpCodeList.Add('VPSUBB,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPSUBB,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPSUBD,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPSUBD,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPSUBQ,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPSUBQ,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPSUBSB,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPSUBSB,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPSUBSW,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPSUBSW,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPSUBUSB,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPSUBUSB,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPSUBUSW,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPSUBUSW,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPSUBW,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPSUBW,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPTEST,1,1,YMMREG,YMMRM,,');
  FOpCodeList.Add('VPTEST,1,1,YMMREG,YMMRM,,');
  FOpCodeList.Add('VPTEST,1,1,XMMREG,XMMRM,,');
  FOpCodeList.Add('VPTEST,1,1,XMMREG,XMMRM,,');
  FOpCodeList.Add('VPUNPCKHBW,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPUNPCKHBW,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPUNPCKHDQ,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPUNPCKHDQ,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPUNPCKHQDQ,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPUNPCKHQDQ,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPUNPCKHWD,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPUNPCKHWD,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPUNPCKLBW,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPUNPCKLBW,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPUNPCKLDQ,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPUNPCKLDQ,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPUNPCKLQDQ,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPUNPCKLQDQ,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPUNPCKLWD,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPUNPCKLWD,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPXOR,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPXOR,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VRCPPS,1,1,XMMREG,XMMRM,,');
  FOpCodeList.Add('VRCPPS,1,1,YMMREG,YMMRM,,');
  FOpCodeList.Add('VRCPSS,1,1,XMMREG,XMMREG,MEM32,');
  FOpCodeList.Add('VRCPSS,1,1,XMMREG,XMMREG,XMMREG,');
  FOpCodeList.Add('VROUNDPD,1,1,YMMREG,YMMRM,IMM8,');
  FOpCodeList.Add('VROUNDPD,1,1,XMMREG,XMMRM,IMM8,');
  FOpCodeList.Add('VROUNDPS,1,1,YMMREG,YMMRM,IMM8,');
  FOpCodeList.Add('VROUNDPS,1,1,XMMREG,XMMRM,IMM8,');
  FOpCodeList.Add('VROUNDSD,1,1,XMMREG,XMMREG,MEM64,IMM8');
  FOpCodeList.Add('VROUNDSD,1,1,XMMREG,XMMREG,XMMREG,IMM8');
  FOpCodeList.Add('VROUNDSS,1,1,XMMREG,XMMREG,MEM32,IMM8');
  FOpCodeList.Add('VROUNDSS,1,1,XMMREG,XMMREG,XMMREG,IMM8');
  FOpCodeList.Add('VRSQRTPS,1,1,YMMREG,YMMRM,,');
  FOpCodeList.Add('VRSQRTPS,1,1,XMMREG,XMMRM,,');
  FOpCodeList.Add('VRSQRTSS,1,1,XMMREG,XMMREG,MEM32,');
  FOpCodeList.Add('VRSQRTSS,1,1,XMMREG,XMMREG,XMMREG,');
  FOpCodeList.Add('VSHUFPD,1,1,XMMREG,XMMREG,XMMRM,IMM8');
  FOpCodeList.Add('VSHUFPD,1,1,YMMREG,YMMREG,YMMRM,IMM8');
  FOpCodeList.Add('VSHUFPS,1,1,XMMREG,XMMREG,XMMRM,IMM8');
  FOpCodeList.Add('VSHUFPS,1,1,YMMREG,YMMREG,YMMRM,IMM8');
  FOpCodeList.Add('VSQRTPD,1,1,XMMREG,XMMRM,,');
  FOpCodeList.Add('VSQRTPD,1,1,YMMREG,YMMRM,,');
  FOpCodeList.Add('VSQRTPS,1,1,XMMREG,XMMRM,,');
  FOpCodeList.Add('VSQRTPS,1,1,YMMREG,YMMRM,,');
  FOpCodeList.Add('VSQRTSD,1,1,XMMREG,XMMREG,MEM64,');
  FOpCodeList.Add('VSQRTSD,1,1,XMMREG,XMMREG,XMMREG,');
  FOpCodeList.Add('VSQRTSS,1,1,XMMREG,XMMREG,MEM32,');
  FOpCodeList.Add('VSQRTSS,1,1,XMMREG,XMMREG,XMMREG,');
  FOpCodeList.Add('VSTMXCSR,1,1,MEM32,,,');
  FOpCodeList.Add('VSUBPD,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VSUBPD,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VSUBPS,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VSUBPS,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VSUBSD,1,1,XMMREG,XMMREG,MEM64,');
  FOpCodeList.Add('VSUBSD,1,1,XMMREG,XMMREG,XMMREG,');
  FOpCodeList.Add('VSUBSS,1,1,XMMREG,XMMREG,MEM32,');
  FOpCodeList.Add('VSUBSS,1,1,XMMREG,XMMREG,XMMREG,');
  FOpCodeList.Add('VTESTPD,1,1,XMMREG,XMMRM,,');
  FOpCodeList.Add('VTESTPD,1,1,YMMREG,YMMRM,,');
  FOpCodeList.Add('VTESTPS,1,1,YMMREG,YMMRM,,');
  FOpCodeList.Add('VTESTPS,1,1,XMMREG,XMMRM,,');
  FOpCodeList.Add('VUCOMISD,1,1,XMMREG,MEM64,,');
  FOpCodeList.Add('VUCOMISD,1,1,XMMREG,XMMREG,,');
  FOpCodeList.Add('VUCOMISS,1,1,XMMREG,MEM32,,');
  FOpCodeList.Add('VUCOMISS,1,1,XMMREG,XMMREG,,');
  FOpCodeList.Add('VUNPCKHPD,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VUNPCKHPD,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VUNPCKHPS,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VUNPCKHPS,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VUNPCKLPD,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VUNPCKLPD,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VUNPCKLPS,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VUNPCKLPS,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VXORPD,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VXORPD,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VXORPS,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VXORPS,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VZEROALL,1,1,,,,');
  FOpCodeList.Add('VZEROUPPER,1,1,,,,');


  FOpCodeList.Add('VBROADCASTI128,1,1,YMMREG,MEM128,,');
  FOpCodeList.Add('VEXTRACTI128,1,1,XMMRM,YMMREG,IMM8,');
  FOpCodeList.Add('VINSERTI128,1,1,YMMREG,YMMREG,XMMRM,IMM8');
  FOpCodeList.Add('VPBLENDD,1,1,YMMREG,YMMREG,YMMRM,IMM8');
  FOpCodeList.Add('VPBLENDD,1,1,XMMREG,XMMREG,XMMRM,IMM8');
  FOpCodeList.Add('VPBROADCASTB,1,1,XMMREG,XMMREG,,');
  FOpCodeList.Add('VPBROADCASTB,1,1,YMMREG,XMMREG,,');
  FOpCodeList.Add('VPBROADCASTB,1,1,XMMREG,MEM8,,');
  FOpCodeList.Add('VPBROADCASTB,1,1,YMMREG,MEM8,,');
  FOpCodeList.Add('VPBROADCASTD,1,1,YMMREG,XMMREG,,');
  FOpCodeList.Add('VPBROADCASTD,1,1,XMMREG,MEM32,,');
  FOpCodeList.Add('VPBROADCASTD,1,1,XMMREG,XMMREG,,');
  FOpCodeList.Add('VPBROADCASTD,1,1,YMMREG,MEM32,,');
  FOpCodeList.Add('VPBROADCASTQ,1,1,YMMREG,MEM64,,');
  FOpCodeList.Add('VPBROADCASTQ,1,1,XMMREG,MEM64,,');
  FOpCodeList.Add('VPBROADCASTQ,1,1,XMMREG,XMMREG,,');
  FOpCodeList.Add('VPBROADCASTQ,1,1,YMMREG,XMMREG,,');
  FOpCodeList.Add('VPBROADCASTW,1,1,XMMREG,MEM16,,');
  FOpCodeList.Add('VPBROADCASTW,1,1,XMMREG,XMMREG,,');
  FOpCodeList.Add('VPBROADCASTW,1,1,YMMREG,MEM16,,');
  FOpCodeList.Add('VPBROADCASTW,1,1,YMMREG,XMMREG,,');
  FOpCodeList.Add('VPERM2I128,1,1,YMMREG,YMMREG,YMMRM,IMM8');
  FOpCodeList.Add('VPERMD,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPERMPD,1,1,YMMREG,YMMRM,IMM8,');
  FOpCodeList.Add('VPERMPS,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPERMQ,1,1,YMMREG,YMMRM,IMM8,');
  FOpCodeList.Add('VPMASKMOVD,1,1,XMMREG,XMMREG,MEM128,');
  FOpCodeList.Add('VPMASKMOVD,1,1,MEM128,XMMREG,XMMREG,');
  FOpCodeList.Add('VPMASKMOVD,1,1,YMMREG,YMMREG,MEM256,');
  FOpCodeList.Add('VPMASKMOVD,1,1,MEM256,YMMREG,YMMREG,');
  FOpCodeList.Add('VPMASKMOVQ,1,1,XMMREG,XMMREG,MEM128,');
  FOpCodeList.Add('VPMASKMOVQ,1,1,MEM128,XMMREG,XMMREG,');
  FOpCodeList.Add('VPMASKMOVQ,1,1,YMMREG,YMMREG,MEM256,');
  FOpCodeList.Add('VPMASKMOVQ,1,1,MEM256,YMMREG,YMMREG,');
  FOpCodeList.Add('VPSLLVD,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPSLLVD,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPSLLVQ,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPSLLVQ,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPSRAVD,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPSRAVD,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPSRLVD,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPSRLVD,1,1,XMMREG,XMMREG,XMMRM,');
  FOpCodeList.Add('VPSRLVQ,1,1,YMMREG,YMMREG,YMMRM,');
  FOpCodeList.Add('VPSRLVQ,1,1,XMMREG,XMMREG,XMMRM,');
end;

function TAVXTestGenerator.InternalMakeTestFiles(aX64: boolean; aDestPath, aFileExt: String;
                                        aOpCodeList, aHeaderList, aFooterList: TStringList): boolean;
var
  i: integer;
  sl: TStringList;
  slAsm: TStringList;
  LastOpCode: String;
  NewOpCode: String;

  function SaveFile(aAsmList: TStringList; aOpcode, aDestPath, aFileExt: String; aHeaderList, aFooterList: TStringList): boolean;
  begin
    result := false;

    if aAsmList.Count > 0 then
    begin
      aAsmList.Insert(0, StringReplace(aHeaderList.Text, '$$$OPCODE$$$', aOpCode, []));
      aAsmList.AddStrings(aFooterList);

      aAsmList.SaveToFile(IncludeTrailingBackslash(aDestPath) + aOpCode + aFileExt);
    end;
  end;

begin
  result := false;

  aOpCodeList.Sort;

  sl := TStringList.Create;
  try
    slAsm := TStringList.Create;
    try
      LastOpCode := '';

      for i := 0 to aOpCodeList.Count - 1 do
      //for i := 0 to 0 do
      begin
        sl.Clear;
        sl.CommaText := aOpCodeList[i];

        while sl.Count < 7 do sl.Add('');

        NewOpCode := sl[0];
        if NewOpCode <> '' then
        begin
          if NewOpCode <> LastOpCode then
          begin
            if LastOpCode <> '' then
            begin
              SaveFile(slAsm, LastOpCode, aDestPath, aFileExt, aHeaderList, aFooterList);
              writeln(format('%s%s%s', [aDestPath, NewOpCode, aFileExt]));

              slAsm.Clear;
              LastOpCode := NewOpCode;
            end
            else LastOpCode := NewOpCode;
          end;  

          if (not(aX64) and (sl[1] = '1')) or // i386
             (aX64 and (sl[2] = '1')) then    // x86_64
          begin
            if (sl[3]  = '') and
               (sl[3]  = '') and
               (sl[3]  = '') and
               (sl[3]  = '') then
            begin                                        // Opcode with no Params, e.g. VZEROALL
              slAsm.Add('    ' + sl[0]);
            end
            else TAsmTestGenerator.CalcTestData(aX64, sl[0], sl[3], sl[4], sl[5], sl[6], slAsm);
          end;
        end;
      end;

      if NewOpCode <> '' then
      begin
        SaveFile(slAsm, NewOpCode, aDestPath, aFileExt, aHeaderList, aFooterList);
        writeln(format('%s%s%s', [aDestPath, NewOpCode, aFileExt]));
      end;

    finally
      FreeAndNil(slAsm);
    end;
  finally
    FreeAndNil(sl);
  end;
end;

function TAVXTestGenerator.MakeTestFiles(aTyp: TTestFileTyp; aX64: boolean;
  aDestPath: String): boolean;
var
  slHeader: TStringList;
  slFooter: TStringList;
  FileExt: String;
  i: integer;
const
  cPlatform: array[boolean] of String = (('i386'), ('x86_64'));  
begin
  result := false;

  slHeader := TStringList.Create;
  try
    slFooter := TStringList.Create;
    try
      case aTyp of
         tfFPC: begin
                  writeln(format('outputformat: fpc  platform: %s  path: %s',
                                 [cPlatform[aX64], aDestPath]));

                  FileExt := '.pp';

                  slHeader.Add('Program $$$OPCODE$$$;');
                  slHeader.Add('{$asmmode intel}');
                  slHeader.Add('begin');
                  slHeader.Add('  asm');

                  for i := 1 to 10 do
                   slHeader.Add('NOP');

                  for i := 1 to 10 do
                   slFooter.Add('NOP');

                  slFooter.Add('  end;');
                  slFooter.Add('end.');
                end;
        tfNasm: begin
                  writeln(format('outputformat: fpc  platform: %s  path: %s',
                                 [cPlatform[aX64], aDestPath]));

                  FileExt := '.asm';

                  if aX64 then slHeader.Add('[BITS 64]')
                   else slHeader.Add('[BITS 32]');

                  for i := 1 to 10 do
                   slHeader.Add('NOP');

                  for i := 1 to 10 do
                   slFooter.Add('NOP');
                end;
      end;

      InternalMakeTestFiles(aX64, aDestPath, Fileext, FOpCodeList, slHeader, slFooter);

    finally
      FreeAndNil(slFooter);
    end;
  finally
    FreeAndNil(slHeader);
  end;
end;

end.

