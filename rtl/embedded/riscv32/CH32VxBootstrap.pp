{
    Copyright (c) 1998-2023 by the Free Pascal development team

    Bootstrap code specific to WCH CH32Vxxx RISC-V microcontrollers.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit CH32VxBootstrap;

interface

{$PACKRECORDS 2}


//override ISR vectors by declaring a procedure with a public alias of 'ISRx'
const

NMI_ISR='ISR2';
HardFault_ISR='ISR3';
Ecall_M_ISR='ISR5';
Ecall_U_ISR='ISR8';
BreakPoint_ISR='ISR9';
SysTick_ISR='ISR12';
SW_ISR='ISR14';
WWDG_ISR='ISR16';
PVD_ISR='ISR17';
TAMPER_ISR='ISR18';
RTC_ISR='ISR19';
FLASH_ISR='ISR20';
RCC_ISR='ISR21';
EXTI0_ISR='ISR22';
EXTI1_ISR='ISR23';
EXTI2_ISR='ISR24';
EXTI3_ISR='ISR25';
EXTI4_ISR='ISR26';
DMA1_CH1_ISR='ISR27';
DMA1_CH2_ISR='ISR28';
DMA1_CH3_ISR='ISR29';
DMA1_CH4_ISR='ISR30';
DMA1_CH5_ISR='ISR31';
DMA1_CH6_ISR='ISR32';
DMA1_CH7_ISR='ISR33';
ADC1_2_ISR='ISR34';
USB_HP_CAN1_TX_ISR='ISR35';
USB_LP_CAN1_RX0_ISR='ISR36';
CAN1_RX1_ISR='ISR37';
CAN1_SCE_ISR='ISR38';
EXTI9_5_ISR='ISR39';
TIM1_BRK_ISR='ISR40';
TIM1_UP_ISR='ISR41';
TIM1_TRG_COM_ISR='ISR42';
TIM1_CC_ISR='ISR43';
TIM2_ISR='ISR44';
TIM3_ISR='ISR45';
TIM4_ISR='ISR46';
I2C1_EV_ISR='ISR47';
I2C1_ER_ISR='ISR48';
I2C2_EV_ISR='ISR49';
I2C2_ER_ISR='ISR50';
SPI1_ISR='ISR51';
SPI2_ISR='ISR52';
USART1_ISR='ISR53';
USART2_ISR='ISR54';
USART3_ISR='ISR55';
EXTI15_10_ISR='ISR56';
RTCAlarm_ISR='ISR57';
USBWakeUp_ISR='ISR58';
TIM8_BRK_ISR='ISR59';
TIM8_UP_ISR='ISR60';
TIM8_TRG_COM_ISR='ISR61';
TIM8_CC_ISR='ISR62';
RNG_ISR='ISR63';
FSMC_ISR='ISR64';
SDIO_ISR='ISR65';
TIM5_ISR='ISR66';
SPI3_ISR='ISR67';
UART4_ISR='ISR68';
UART5_ISR='ISR69';
TIM6_ISR='ISR70';
TIM7_ISR='ISR71';
DMA2_CH1_ISR='ISR72';
DMA2_CH2_ISR='ISR73';
DMA2_CH3_ISR='ISR74';
DMA2_CH4_ISR='ISR75';
DMA2_CH5_ISR='ISR76';
ETH_ISR='ISR77';
ETH_WKUP_ISR='ISR78';
CAN2_TX_ISR='ISR79';
CAN2_RX0_ISR='ISR80';
CAN2_RX1_ISR='ISR81';
CAN2_SCE_ISR='ISR82';
OTG_FS_ISR='ISR83';
USBHSWakeUp_ISR='ISR84';
USBHS_ISR='ISR85';
DVP_ISR='ISR86';
UART6_ISR='ISR87';
UART7_ISR='ISR88';
UART8_ISR='ISR89';
TIM9_BRK_ISR='ISR90';
TIM9_UP_ISR='ISR91';
TIM9_TRG_COM_ISR='ISR92';
TIM9_CC_ISR='ISR93';
TIM10_BRK_ISR='ISR94';
TIM10_UP_ISR='ISR95';
TIM10_TRG_COM_ISR='ISR96';
TIM10_CC_ISR='ISR97';
DMA2_CH6_ISR='ISR98';
DMA2_CH7_ISR='ISR99';
DMA2_CH8_ISR='ISR100';
DMA2_CH9_ISR='ISR101';
DMA2_CH10_ISR='ISR102';
DMA2_CH11_ISR='ISR103';


//used by user reset hook to init system from non-power up resets ( WDT, WakeFromSleep, etc.).
procedure InitMemAndStart; noreturn;

implementation

const
  RCC_RSTSCKR = $40021024;
  PORRSTF = 1 shl 27;
  RMVF = 1 shl 24;

  //Defaults to InitMemAndStart, can be overriden by user
procedure ResetISR; external Name 'ResetISR';

procedure HandleArchSpecificReset; noreturn;
begin
  if (PLongWord(RCC_RSTSCKR)^ and PORRSTF) > 0 then
  begin
    PLongWord(RCC_RSTSCKR)^ := PLongWord(RCC_RSTSCKR)^ or RMVF; //clear reset flags;
    // always init memory and system on power-on reset
    InitMemAndStart;
  end
  else
    ResetISR; //reset-events other than power-on
end;

  {$i riscv32_start.inc}

procedure DefaultISRHandler; external Name 'DefaultISRHandler';


procedure ISR1; external Name 'ISR1';
procedure ISR2; external Name 'ISR2';
procedure ISR3; external Name 'ISR3';
procedure ISR4; external Name 'ISR4';
procedure ISR5; external Name 'ISR5';
procedure ISR6; external Name 'ISR6';
procedure ISR7; external Name 'ISR7';
procedure ISR8; external Name 'ISR8';
procedure ISR9; external Name 'ISR9';
procedure ISR10; external Name 'ISR10';
procedure ISR11; external Name 'ISR11';
procedure ISR12; external Name 'ISR12';
procedure ISR13; external Name 'ISR13';
procedure ISR14; external Name 'ISR14';
procedure ISR15; external Name 'ISR15';
procedure ISR16; external Name 'ISR16';
procedure ISR17; external Name 'ISR17';
procedure ISR18; external Name 'ISR18';
procedure ISR19; external Name 'ISR19';
procedure ISR20; external Name 'ISR20';
procedure ISR21; external Name 'ISR21';
procedure ISR22; external Name 'ISR22';
procedure ISR23; external Name 'ISR23';
procedure ISR24; external Name 'ISR24';
procedure ISR25; external Name 'ISR25';
procedure ISR26; external Name 'ISR26';
procedure ISR27; external Name 'ISR27';
procedure ISR28; external Name 'ISR28';
procedure ISR29; external Name 'ISR29';
procedure ISR30; external Name 'ISR30';
procedure ISR31; external Name 'ISR31';
procedure ISR32; external Name 'ISR32';
procedure ISR33; external Name 'ISR33';
procedure ISR34; external Name 'ISR34';
procedure ISR35; external Name 'ISR35';
procedure ISR36; external Name 'ISR36';
procedure ISR37; external Name 'ISR37';
procedure ISR38; external Name 'ISR38';
procedure ISR39; external Name 'ISR39';
procedure ISR40; external Name 'ISR40';
procedure ISR41; external Name 'ISR41';
procedure ISR42; external Name 'ISR42';
procedure ISR43; external Name 'ISR43';
procedure ISR44; external Name 'ISR44';
procedure ISR45; external Name 'ISR45';
procedure ISR46; external Name 'ISR46';
procedure ISR47; external Name 'ISR47';
procedure ISR48; external Name 'ISR48';
procedure ISR49; external Name 'ISR49';
procedure ISR50; external Name 'ISR50';
procedure ISR51; external Name 'ISR51';
procedure ISR52; external Name 'ISR52';
procedure ISR53; external Name 'ISR53';
procedure ISR54; external Name 'ISR54';
procedure ISR55; external Name 'ISR55';
procedure ISR56; external Name 'ISR56';
procedure ISR57; external Name 'ISR57';
procedure ISR58; external Name 'ISR58';
procedure ISR59; external Name 'ISR59';
procedure ISR60; external Name 'ISR60';
procedure ISR61; external Name 'ISR61';
procedure ISR62; external Name 'ISR62';
procedure ISR63; external Name 'ISR63';
procedure ISR64; external Name 'ISR64';
procedure ISR65; external Name 'ISR65';
procedure ISR66; external Name 'ISR66';
procedure ISR67; external Name 'ISR67';
procedure ISR68; external Name 'ISR68';
procedure ISR69; external Name 'ISR69';
procedure ISR70; external Name 'ISR70';
procedure ISR71; external Name 'ISR71';
procedure ISR72; external Name 'ISR72';
procedure ISR73; external Name 'ISR73';
procedure ISR74; external Name 'ISR74';
procedure ISR75; external Name 'ISR75';
procedure ISR76; external Name 'ISR76';
procedure ISR77; external Name 'ISR77';
procedure ISR78; external Name 'ISR78';
procedure ISR79; external Name 'ISR79';
procedure ISR80; external Name 'ISR80';
procedure ISR81; external Name 'ISR81';
procedure ISR82; external Name 'ISR82';
procedure ISR83; external Name 'ISR83';
procedure ISR84; external Name 'ISR84';
procedure ISR85; external Name 'ISR85';
procedure ISR86; external Name 'ISR86';
procedure ISR87; external Name 'ISR87';
procedure ISR88; external Name 'ISR88';
procedure ISR89; external Name 'ISR89';
procedure ISR90; external Name 'ISR90';
procedure ISR91; external Name 'ISR91';
procedure ISR92; external Name 'ISR92';
procedure ISR93; external Name 'ISR93';
procedure ISR94; external Name 'ISR94';
procedure ISR95; external Name 'ISR95';
procedure ISR96; external Name 'ISR96';
procedure ISR97; external Name 'ISR97';
procedure ISR98; external Name 'ISR98';
procedure ISR99; external Name 'ISR99';
procedure ISR100; external Name 'ISR100';
procedure ISR101; external Name 'ISR101';
procedure ISR102; external Name 'ISR102';
procedure ISR103; external Name 'ISR103';
procedure ISR104; external Name 'ISR104';
procedure ISR105; external Name 'ISR105';
procedure ISR106; external Name 'ISR106';
procedure ISR107; external Name 'ISR107';
procedure ISR108; external Name 'ISR108';
procedure ISR109; external Name 'ISR109';
procedure ISR110; external Name 'ISR110';
procedure ISR111; external Name 'ISR111';
procedure ISR112; external Name 'ISR112';
procedure ISR113; external Name 'ISR113';
procedure ISR114; external Name 'ISR114';
procedure ISR115; external Name 'ISR115';
procedure ISR116; external Name 'ISR116';
procedure ISR117; external Name 'ISR117';
procedure ISR118; external Name 'ISR118';
procedure ISR119; external Name 'ISR119';
procedure ISR120; external Name 'ISR120';
procedure ISR121; external Name 'ISR121';
procedure ISR122; external Name 'ISR122';
procedure ISR123; external Name 'ISR123';
procedure ISR124; external Name 'ISR124';
procedure ISR125; external Name 'ISR125';
procedure ISR126; external Name 'ISR126';
procedure ISR127; external Name 'ISR127';
procedure ISR128; external Name 'ISR128';
procedure ISR129; external Name 'ISR129';
procedure ISR130; external Name 'ISR130';
procedure ISR131; external Name 'ISR131';
procedure ISR132; external Name 'ISR132';
procedure ISR133; external Name 'ISR133';
procedure ISR134; external Name 'ISR134';
procedure ISR135; external Name 'ISR135';
procedure ISR136; external Name 'ISR136';
procedure ISR137; external Name 'ISR137';
procedure ISR138; external Name 'ISR138';
procedure ISR139; external Name 'ISR139';
procedure ISR140; external Name 'ISR140';
procedure ISR141; external Name 'ISR141';
procedure ISR142; external Name 'ISR142';
procedure ISR143; external Name 'ISR143';
procedure ISR144; external Name 'ISR144';
procedure ISR145; external Name 'ISR145';
procedure ISR146; external Name 'ISR146';
procedure ISR147; external Name 'ISR147';
procedure ISR148; external Name 'ISR148';
procedure ISR149; external Name 'ISR149';
procedure ISR150; external Name 'ISR150';
procedure ISR151; external Name 'ISR151';
procedure ISR152; external Name 'ISR152';
procedure ISR153; external Name 'ISR153';
procedure ISR154; external Name 'ISR154';
procedure ISR155; external Name 'ISR155';
procedure ISR156; external Name 'ISR156';
procedure ISR157; external Name 'ISR157';
procedure ISR158; external Name 'ISR158';
procedure ISR159; external Name 'ISR159';
procedure ISR160; external Name 'ISR160';
procedure ISR161; external Name 'ISR161';
procedure ISR162; external Name 'ISR162';
procedure ISR163; external Name 'ISR163';
procedure ISR164; external Name 'ISR164';
procedure ISR165; external Name 'ISR165';
procedure ISR166; external Name 'ISR166';
procedure ISR167; external Name 'ISR167';
procedure ISR168; external Name 'ISR168';
procedure ISR169; external Name 'ISR169';
procedure ISR170; external Name 'ISR170';
procedure ISR171; external Name 'ISR171';
procedure ISR172; external Name 'ISR172';
procedure ISR173; external Name 'ISR173';
procedure ISR174; external Name 'ISR174';
procedure ISR175; external Name 'ISR175';
procedure ISR176; external Name 'ISR176';
procedure ISR177; external Name 'ISR177';
procedure ISR178; external Name 'ISR178';
procedure ISR179; external Name 'ISR179';
procedure ISR180; external Name 'ISR180';
procedure ISR181; external Name 'ISR181';
procedure ISR182; external Name 'ISR182';
procedure ISR183; external Name 'ISR183';
procedure ISR184; external Name 'ISR184';
procedure ISR185; external Name 'ISR185';
procedure ISR186; external Name 'ISR186';
procedure ISR187; external Name 'ISR187';
procedure ISR188; external Name 'ISR188';
procedure ISR189; external Name 'ISR189';
procedure ISR190; external Name 'ISR190';
procedure ISR191; external Name 'ISR191';
procedure ISR192; external Name 'ISR192';
procedure ISR193; external Name 'ISR193';
procedure ISR194; external Name 'ISR194';
procedure ISR195; external Name 'ISR195';
procedure ISR196; external Name 'ISR196';
procedure ISR197; external Name 'ISR197';
procedure ISR198; external Name 'ISR198';
procedure ISR199; external Name 'ISR199';
procedure ISR200; external Name 'ISR200';
procedure ISR201; external Name 'ISR201';
procedure ISR202; external Name 'ISR202';
procedure ISR203; external Name 'ISR203';
procedure ISR204; external Name 'ISR204';
procedure ISR205; external Name 'ISR205';
procedure ISR206; external Name 'ISR206';
procedure ISR207; external Name 'ISR207';
procedure ISR208; external Name 'ISR208';
procedure ISR209; external Name 'ISR209';
procedure ISR210; external Name 'ISR210';
procedure ISR211; external Name 'ISR211';
procedure ISR212; external Name 'ISR212';
procedure ISR213; external Name 'ISR213';
procedure ISR214; external Name 'ISR214';
procedure ISR215; external Name 'ISR215';
procedure ISR216; external Name 'ISR216';
procedure ISR217; external Name 'ISR217';
procedure ISR218; external Name 'ISR218';
procedure ISR219; external Name 'ISR219';
procedure ISR220; external Name 'ISR220';
procedure ISR221; external Name 'ISR221';
procedure ISR222; external Name 'ISR222';
procedure ISR223; external Name 'ISR223';
procedure ISR224; external Name 'ISR224';
procedure ISR225; external Name 'ISR225';
procedure ISR226; external Name 'ISR226';
procedure ISR227; external Name 'ISR227';
procedure ISR228; external Name 'ISR228';
procedure ISR229; external Name 'ISR229';
procedure ISR230; external Name 'ISR230';
procedure ISR231; external Name 'ISR231';
procedure ISR232; external Name 'ISR232';
procedure ISR233; external Name 'ISR233';
procedure ISR234; external Name 'ISR234';
procedure ISR235; external Name 'ISR235';
procedure ISR236; external Name 'ISR236';
procedure ISR237; external Name 'ISR237';
procedure ISR238; external Name 'ISR238';
procedure ISR239; external Name 'ISR239';
procedure ISR240; external Name 'ISR240';
procedure ISR241; external Name 'ISR241';
procedure ISR242; external Name 'ISR242';
procedure ISR243; external Name 'ISR243';
procedure ISR244; external Name 'ISR244';
procedure ISR245; external Name 'ISR245';
procedure ISR246; external Name 'ISR246';
procedure ISR247; external Name 'ISR247';
procedure ISR248; external Name 'ISR248';
procedure ISR249; external Name 'ISR249';
procedure ISR250; external Name 'ISR250';
procedure ISR251; external Name 'ISR251';
procedure ISR252; external Name 'ISR252';
procedure ISR253; external Name 'ISR253';
procedure ISR254; external Name 'ISR254';
procedure ISR255; external Name 'ISR255';


procedure Vectors; assembler; nostackframe;
asm
  .section ".init.interrupt_vectors"

    .set DefaultISRHandler, _haltproc

    jal x0, LowLevelStartup
    jal x0,  ISR1
    jal x0,  ISR2
    jal x0,  ISR3
    jal x0,  ISR4
    jal x0,  ISR5
    jal x0,  ISR6
    jal x0,  ISR7
    jal x0,  ISR8
    jal x0,  ISR9
    jal x0,  ISR10
    jal x0,  ISR11
    jal x0,  ISR12
    .long 0x00100073  // ebreak<--needed for hardware debugger to work
    jal x0,  ISR14
    jal x0,  ISR15
    jal x0, ISR16
    jal x0, ISR17
    jal x0, ISR18
    jal x0, ISR19
    jal x0, ISR20
    jal x0, ISR21
    jal x0, ISR22
    jal x0, ISR23
    jal x0, ISR24
    jal x0, ISR25
    jal x0, ISR26
    jal x0, ISR27
    jal x0, ISR28
    jal x0, ISR29
    jal x0, ISR30
    jal x0, ISR31
    jal x0, ISR32
    jal x0, ISR33
    jal x0, ISR34
    jal x0, ISR35
    jal x0, ISR36
    jal x0, ISR37
    jal x0, ISR38
    jal x0, ISR39
    jal x0, ISR40
    jal x0, ISR41
    jal x0, ISR42
    jal x0, ISR43
    jal x0, ISR44
    jal x0, ISR45
    jal x0, ISR46
    jal x0, ISR47
    jal x0, ISR48
    jal x0, ISR49
    jal x0, ISR50
    jal x0, ISR51
    jal x0, ISR52
    jal x0, ISR53
    jal x0, ISR54
    jal x0, ISR55
    jal x0, ISR56
    jal x0, ISR57
    jal x0, ISR58
    jal x0, ISR59
    jal x0, ISR60
    jal x0, ISR61
    jal x0, ISR62
    jal x0, ISR63
    jal x0, ISR64
    jal x0, ISR65
    jal x0, ISR66
    jal x0, ISR67
    jal x0, ISR68
    jal x0, ISR69
    jal x0, ISR70
    jal x0, ISR71
    jal x0, ISR72
    jal x0, ISR73
    jal x0, ISR74
    jal x0, ISR75
    jal x0, ISR76
    jal x0, ISR77
    jal x0, ISR78
    jal x0, ISR79
    jal x0, ISR80
    jal x0, ISR81
    jal x0, ISR82
    jal x0, ISR83
    jal x0, ISR84
    jal x0, ISR85
    jal x0, ISR86
    jal x0, ISR87
    jal x0, ISR88
    jal x0, ISR89
    jal x0, ISR90
    jal x0, ISR91
    jal x0, ISR92
    jal x0, ISR93
    jal x0, ISR94
    jal x0, ISR95
    jal x0, ISR96
    jal x0, ISR97
    jal x0, ISR98
    jal x0, ISR99
    jal x0, ISR100
    jal x0, ISR101
    jal x0, ISR102
    jal x0, ISR103
    jal x0, ISR104
    jal x0, ISR105
    jal x0, ISR106
    jal x0, ISR107
    jal x0, ISR108
    jal x0, ISR109
    jal x0, ISR110
    jal x0, ISR111
    jal x0, ISR112
    jal x0, ISR113
    jal x0, ISR114
    jal x0, ISR115
    jal x0, ISR116
    jal x0, ISR117
    jal x0, ISR118
    jal x0, ISR119
    jal x0, ISR120
    jal x0, ISR121
    jal x0, ISR122
    jal x0, ISR123
    jal x0, ISR124
    jal x0, ISR125
    jal x0, ISR126
    jal x0, ISR127
    jal x0, ISR128
    jal x0, ISR129
    jal x0, ISR130
    jal x0, ISR131
    jal x0, ISR132
    jal x0, ISR133
    jal x0, ISR134
    jal x0, ISR135
    jal x0, ISR136
    jal x0, ISR137
    jal x0, ISR138
    jal x0, ISR139
    jal x0, ISR140
    jal x0, ISR141
    jal x0, ISR142
    jal x0, ISR143
    jal x0, ISR144
    jal x0, ISR145
    jal x0, ISR146
    jal x0, ISR147
    jal x0, ISR148
    jal x0, ISR149
    jal x0, ISR150
    jal x0, ISR151
    jal x0, ISR152
    jal x0, ISR153
    jal x0, ISR154
    jal x0, ISR155
    jal x0, ISR156
    jal x0, ISR157
    jal x0, ISR158
    jal x0, ISR159
    jal x0, ISR160
    jal x0, ISR161
    jal x0, ISR162
    jal x0, ISR163
    jal x0, ISR164
    jal x0, ISR165
    jal x0, ISR166
    jal x0, ISR167
    jal x0, ISR168
    jal x0, ISR169
    jal x0, ISR170
    jal x0, ISR171
    jal x0, ISR172
    jal x0, ISR173
    jal x0, ISR174
    jal x0, ISR175
    jal x0, ISR176
    jal x0, ISR177
    jal x0, ISR178
    jal x0, ISR179
    jal x0, ISR180
    jal x0, ISR181
    jal x0, ISR182
    jal x0, ISR183
    jal x0, ISR184
    jal x0, ISR185
    jal x0, ISR186
    jal x0, ISR187
    jal x0, ISR188
    jal x0, ISR189
    jal x0, ISR190
    jal x0, ISR191
    jal x0, ISR192
    jal x0, ISR193
    jal x0, ISR194
    jal x0, ISR195
    jal x0, ISR196
    jal x0, ISR197
    jal x0, ISR198
    jal x0, ISR199
    jal x0, ISR200
    jal x0, ISR201
    jal x0, ISR202
    jal x0, ISR203
    jal x0, ISR204
    jal x0, ISR205
    jal x0, ISR206
    jal x0, ISR207
    jal x0, ISR208
    jal x0, ISR209
    jal x0, ISR210
    jal x0, ISR211
    jal x0, ISR212
    jal x0, ISR213
    jal x0, ISR214
    jal x0, ISR215
    jal x0, ISR216
    jal x0, ISR217
    jal x0, ISR218
    jal x0, ISR219
    jal x0, ISR220
    jal x0, ISR221
    jal x0, ISR222
    jal x0, ISR223
    jal x0, ISR224
    jal x0, ISR225
    jal x0, ISR226
    jal x0, ISR227
    jal x0, ISR228
    jal x0, ISR229
    jal x0, ISR230
    jal x0, ISR231
    jal x0, ISR232
    jal x0, ISR233
    jal x0, ISR234
    jal x0, ISR235
    jal x0, ISR236
    jal x0, ISR237
    jal x0, ISR238
    jal x0, ISR239
    jal x0, ISR240
    jal x0, ISR241
    jal x0, ISR242
    jal x0, ISR243
    jal x0, ISR244
    jal x0, ISR245
    jal x0, ISR246
    jal x0, ISR247
    jal x0, ISR248
    jal x0, ISR249
    jal x0, ISR250
    jal x0, ISR251
    jal x0, ISR252
    jal x0, ISR253
    jal x0, ISR254
    jal x0, ISR255

    .weak DefaultISRHandler
    .weak LowLevelStartup

    .weak ISR1
    .weak ISR2
    .weak ISR3
    .weak ISR4
    .weak ISR5
    .weak ISR6
    .weak ISR7
    .weak ISR8
    .weak ISR9
    .weak ISR10
    .weak ISR11
    .weak ISR12
    .weak ISR13
    .weak ISR14
    .weak ISR15
    .weak ISR16
    .weak ISR17
    .weak ISR18
    .weak ISR19
    .weak ISR20
    .weak ISR21
    .weak ISR22
    .weak ISR23
    .weak ISR24
    .weak ISR25
    .weak ISR26
    .weak ISR27
    .weak ISR28
    .weak ISR29
    .weak ISR30
    .weak ISR31
    .weak ISR32
    .weak ISR33
    .weak ISR34
    .weak ISR35
    .weak ISR36
    .weak ISR37
    .weak ISR38
    .weak ISR39
    .weak ISR40
    .weak ISR41
    .weak ISR42
    .weak ISR43
    .weak ISR44
    .weak ISR45
    .weak ISR46
    .weak ISR47
    .weak ISR48
    .weak ISR49
    .weak ISR50
    .weak ISR51
    .weak ISR52
    .weak ISR53
    .weak ISR54
    .weak ISR55
    .weak ISR56
    .weak ISR57
    .weak ISR58
    .weak ISR59
    .weak ISR60
    .weak ISR61
    .weak ISR62
    .weak ISR63
    .weak ISR64
    .weak ISR65
    .weak ISR66
    .weak ISR67
    .weak ISR68
    .weak ISR69
    .weak ISR70
    .weak ISR71
    .weak ISR72
    .weak ISR73
    .weak ISR74
    .weak ISR75
    .weak ISR76
    .weak ISR77
    .weak ISR78
    .weak ISR79
    .weak ISR80
    .weak ISR81
    .weak ISR82
    .weak ISR83
    .weak ISR84
    .weak ISR85
    .weak ISR86
    .weak ISR87
    .weak ISR88
    .weak ISR89
    .weak ISR90
    .weak ISR91
    .weak ISR92
    .weak ISR93
    .weak ISR94
    .weak ISR95
    .weak ISR96
    .weak ISR97
    .weak ISR98
    .weak ISR99
    .weak ISR100
    .weak ISR101
    .weak ISR102
    .weak ISR103
    .weak ISR104
    .weak ISR105
    .weak ISR106
    .weak ISR107
    .weak ISR108
    .weak ISR109
    .weak ISR110
    .weak ISR111
    .weak ISR112
    .weak ISR113
    .weak ISR114
    .weak ISR115
    .weak ISR116
    .weak ISR117
    .weak ISR118
    .weak ISR119
    .weak ISR120
    .weak ISR121
    .weak ISR122
    .weak ISR123
    .weak ISR124
    .weak ISR125
    .weak ISR126
    .weak ISR127
    .weak ISR128
    .weak ISR129
    .weak ISR130
    .weak ISR131
    .weak ISR132
    .weak ISR133
    .weak ISR134
    .weak ISR135
    .weak ISR136
    .weak ISR137
    .weak ISR138
    .weak ISR139
    .weak ISR140
    .weak ISR141
    .weak ISR142
    .weak ISR143
    .weak ISR144
    .weak ISR145
    .weak ISR146
    .weak ISR147
    .weak ISR148
    .weak ISR149
    .weak ISR150
    .weak ISR151
    .weak ISR152
    .weak ISR153
    .weak ISR154
    .weak ISR155
    .weak ISR156
    .weak ISR157
    .weak ISR158
    .weak ISR159
    .weak ISR160
    .weak ISR161
    .weak ISR162
    .weak ISR163
    .weak ISR164
    .weak ISR165
    .weak ISR166
    .weak ISR167
    .weak ISR168
    .weak ISR169
    .weak ISR170
    .weak ISR171
    .weak ISR172
    .weak ISR173
    .weak ISR174
    .weak ISR175
    .weak ISR176
    .weak ISR177
    .weak ISR178
    .weak ISR179
    .weak ISR180
    .weak ISR181
    .weak ISR182
    .weak ISR183
    .weak ISR184
    .weak ISR185
    .weak ISR186
    .weak ISR187
    .weak ISR188
    .weak ISR189
    .weak ISR190
    .weak ISR191
    .weak ISR192
    .weak ISR193
    .weak ISR194
    .weak ISR195
    .weak ISR196
    .weak ISR197
    .weak ISR198
    .weak ISR199
    .weak ISR200
    .weak ISR201
    .weak ISR202
    .weak ISR203
    .weak ISR204
    .weak ISR205
    .weak ISR206
    .weak ISR207
    .weak ISR208
    .weak ISR209
    .weak ISR210
    .weak ISR211
    .weak ISR212
    .weak ISR213
    .weak ISR214
    .weak ISR215
    .weak ISR216
    .weak ISR217
    .weak ISR218
    .weak ISR219
    .weak ISR220
    .weak ISR221
    .weak ISR222
    .weak ISR223
    .weak ISR224
    .weak ISR225
    .weak ISR226
    .weak ISR227
    .weak ISR228
    .weak ISR229
    .weak ISR230
    .weak ISR231
    .weak ISR232
    .weak ISR233
    .weak ISR234
    .weak ISR235
    .weak ISR236
    .weak ISR237
    .weak ISR238
    .weak ISR239
    .weak ISR240
    .weak ISR241
    .weak ISR242
    .weak ISR243
    .weak ISR244
    .weak ISR245
    .weak ISR246
    .weak ISR247
    .weak ISR248
    .weak ISR249
    .weak ISR250
    .weak ISR251
    .weak ISR252
    .weak ISR253
    .weak ISR254
    .weak ISR255

    .set ISR1, DefaultISRHandler
    .set ISR2, DefaultISRHandler
    .set ISR3, DefaultISRHandler
    .set ISR4, DefaultISRHandler
    .set ISR5, DefaultISRHandler
    .set ISR6, DefaultISRHandler
    .set ISR7, DefaultISRHandler
    .set ISR8, DefaultISRHandler
    .set ISR9, DefaultISRHandler
    .set ISR10, DefaultISRHandler
    .set ISR11, DefaultISRHandler
    .set ISR12, DefaultISRHandler
    .set ISR13, DefaultISRHandler
    .set ISR14, DefaultISRHandler
    .set ISR15, DefaultISRHandler
    .set ISR16, DefaultISRHandler
    .set ISR17, DefaultISRHandler
    .set ISR18, DefaultISRHandler
    .set ISR19, DefaultISRHandler
    .set ISR20, DefaultISRHandler
    .set ISR21, DefaultISRHandler
    .set ISR22, DefaultISRHandler
    .set ISR23, DefaultISRHandler
    .set ISR24, DefaultISRHandler
    .set ISR25, DefaultISRHandler
    .set ISR26, DefaultISRHandler
    .set ISR27, DefaultISRHandler
    .set ISR28, DefaultISRHandler
    .set ISR29, DefaultISRHandler
    .set ISR30, DefaultISRHandler
    .set ISR31, DefaultISRHandler
    .set ISR32, DefaultISRHandler
    .set ISR33, DefaultISRHandler
    .set ISR34, DefaultISRHandler
    .set ISR35, DefaultISRHandler
    .set ISR36, DefaultISRHandler
    .set ISR37, DefaultISRHandler
    .set ISR38, DefaultISRHandler
    .set ISR39, DefaultISRHandler
    .set ISR40, DefaultISRHandler
    .set ISR41, DefaultISRHandler
    .set ISR42, DefaultISRHandler
    .set ISR43, DefaultISRHandler
    .set ISR44, DefaultISRHandler
    .set ISR45, DefaultISRHandler
    .set ISR46, DefaultISRHandler
    .set ISR47, DefaultISRHandler
    .set ISR48, DefaultISRHandler
    .set ISR49, DefaultISRHandler
    .set ISR50, DefaultISRHandler
    .set ISR51, DefaultISRHandler
    .set ISR52, DefaultISRHandler
    .set ISR53, DefaultISRHandler
    .set ISR54, DefaultISRHandler
    .set ISR55, DefaultISRHandler
    .set ISR56, DefaultISRHandler
    .set ISR57, DefaultISRHandler
    .set ISR58, DefaultISRHandler
    .set ISR59, DefaultISRHandler
    .set ISR60, DefaultISRHandler
    .set ISR61, DefaultISRHandler
    .set ISR62, DefaultISRHandler
    .set ISR63, DefaultISRHandler
    .set ISR64, DefaultISRHandler
    .set ISR65, DefaultISRHandler
    .set ISR66, DefaultISRHandler
    .set ISR67, DefaultISRHandler
    .set ISR68, DefaultISRHandler
    .set ISR69, DefaultISRHandler
    .set ISR70, DefaultISRHandler
    .set ISR71, DefaultISRHandler
    .set ISR72, DefaultISRHandler
    .set ISR73, DefaultISRHandler
    .set ISR74, DefaultISRHandler
    .set ISR75, DefaultISRHandler
    .set ISR76, DefaultISRHandler
    .set ISR77, DefaultISRHandler
    .set ISR78, DefaultISRHandler
    .set ISR79, DefaultISRHandler
    .set ISR80, DefaultISRHandler
    .set ISR81, DefaultISRHandler
    .set ISR82, DefaultISRHandler
    .set ISR83, DefaultISRHandler
    .set ISR84, DefaultISRHandler
    .set ISR85, DefaultISRHandler
    .set ISR86, DefaultISRHandler
    .set ISR87, DefaultISRHandler
    .set ISR88, DefaultISRHandler
    .set ISR89, DefaultISRHandler
    .set ISR90, DefaultISRHandler
    .set ISR91, DefaultISRHandler
    .set ISR92, DefaultISRHandler
    .set ISR93, DefaultISRHandler
    .set ISR94, DefaultISRHandler
    .set ISR95, DefaultISRHandler
    .set ISR96, DefaultISRHandler
    .set ISR97, DefaultISRHandler
    .set ISR98, DefaultISRHandler
    .set ISR99, DefaultISRHandler
    .set ISR100, DefaultISRHandler
    .set ISR101, DefaultISRHandler
    .set ISR102, DefaultISRHandler
    .set ISR103, DefaultISRHandler
    .set ISR104, DefaultISRHandler
    .set ISR105, DefaultISRHandler
    .set ISR106, DefaultISRHandler
    .set ISR107, DefaultISRHandler
    .set ISR108, DefaultISRHandler
    .set ISR109, DefaultISRHandler
    .set ISR110, DefaultISRHandler
    .set ISR111, DefaultISRHandler
    .set ISR112, DefaultISRHandler
    .set ISR113, DefaultISRHandler
    .set ISR114, DefaultISRHandler
    .set ISR115, DefaultISRHandler
    .set ISR116, DefaultISRHandler
    .set ISR117, DefaultISRHandler
    .set ISR118, DefaultISRHandler
    .set ISR119, DefaultISRHandler
    .set ISR120, DefaultISRHandler
    .set ISR121, DefaultISRHandler
    .set ISR122, DefaultISRHandler
    .set ISR123, DefaultISRHandler
    .set ISR124, DefaultISRHandler
    .set ISR125, DefaultISRHandler
    .set ISR126, DefaultISRHandler
    .set ISR127, DefaultISRHandler
    .set ISR128, DefaultISRHandler
    .set ISR129, DefaultISRHandler
    .set ISR130, DefaultISRHandler
    .set ISR131, DefaultISRHandler
    .set ISR132, DefaultISRHandler
    .set ISR133, DefaultISRHandler
    .set ISR134, DefaultISRHandler
    .set ISR135, DefaultISRHandler
    .set ISR136, DefaultISRHandler
    .set ISR137, DefaultISRHandler
    .set ISR138, DefaultISRHandler
    .set ISR139, DefaultISRHandler
    .set ISR140, DefaultISRHandler
    .set ISR141, DefaultISRHandler
    .set ISR142, DefaultISRHandler
    .set ISR143, DefaultISRHandler
    .set ISR144, DefaultISRHandler
    .set ISR145, DefaultISRHandler
    .set ISR146, DefaultISRHandler
    .set ISR147, DefaultISRHandler
    .set ISR148, DefaultISRHandler
    .set ISR149, DefaultISRHandler
    .set ISR150, DefaultISRHandler
    .set ISR151, DefaultISRHandler
    .set ISR152, DefaultISRHandler
    .set ISR153, DefaultISRHandler
    .set ISR154, DefaultISRHandler
    .set ISR155, DefaultISRHandler
    .set ISR156, DefaultISRHandler
    .set ISR157, DefaultISRHandler
    .set ISR158, DefaultISRHandler
    .set ISR159, DefaultISRHandler
    .set ISR160, DefaultISRHandler
    .set ISR161, DefaultISRHandler
    .set ISR162, DefaultISRHandler
    .set ISR163, DefaultISRHandler
    .set ISR164, DefaultISRHandler
    .set ISR165, DefaultISRHandler
    .set ISR166, DefaultISRHandler
    .set ISR167, DefaultISRHandler
    .set ISR168, DefaultISRHandler
    .set ISR169, DefaultISRHandler
    .set ISR170, DefaultISRHandler
    .set ISR171, DefaultISRHandler
    .set ISR172, DefaultISRHandler
    .set ISR173, DefaultISRHandler
    .set ISR174, DefaultISRHandler
    .set ISR175, DefaultISRHandler
    .set ISR176, DefaultISRHandler
    .set ISR177, DefaultISRHandler
    .set ISR178, DefaultISRHandler
    .set ISR179, DefaultISRHandler
    .set ISR180, DefaultISRHandler
    .set ISR181, DefaultISRHandler
    .set ISR182, DefaultISRHandler
    .set ISR183, DefaultISRHandler
    .set ISR184, DefaultISRHandler
    .set ISR185, DefaultISRHandler
    .set ISR186, DefaultISRHandler
    .set ISR187, DefaultISRHandler
    .set ISR188, DefaultISRHandler
    .set ISR189, DefaultISRHandler
    .set ISR190, DefaultISRHandler
    .set ISR191, DefaultISRHandler
    .set ISR192, DefaultISRHandler
    .set ISR193, DefaultISRHandler
    .set ISR194, DefaultISRHandler
    .set ISR195, DefaultISRHandler
    .set ISR196, DefaultISRHandler
    .set ISR197, DefaultISRHandler
    .set ISR198, DefaultISRHandler
    .set ISR199, DefaultISRHandler
    .set ISR200, DefaultISRHandler
    .set ISR201, DefaultISRHandler
    .set ISR202, DefaultISRHandler
    .set ISR203, DefaultISRHandler
    .set ISR204, DefaultISRHandler
    .set ISR205, DefaultISRHandler
    .set ISR206, DefaultISRHandler
    .set ISR207, DefaultISRHandler
    .set ISR208, DefaultISRHandler
    .set ISR209, DefaultISRHandler
    .set ISR210, DefaultISRHandler
    .set ISR211, DefaultISRHandler
    .set ISR212, DefaultISRHandler
    .set ISR213, DefaultISRHandler
    .set ISR214, DefaultISRHandler
    .set ISR215, DefaultISRHandler
    .set ISR216, DefaultISRHandler
    .set ISR217, DefaultISRHandler
    .set ISR218, DefaultISRHandler
    .set ISR219, DefaultISRHandler
    .set ISR220, DefaultISRHandler
    .set ISR221, DefaultISRHandler
    .set ISR222, DefaultISRHandler
    .set ISR223, DefaultISRHandler
    .set ISR224, DefaultISRHandler
    .set ISR225, DefaultISRHandler
    .set ISR226, DefaultISRHandler
    .set ISR227, DefaultISRHandler
    .set ISR228, DefaultISRHandler
    .set ISR229, DefaultISRHandler
    .set ISR230, DefaultISRHandler
    .set ISR231, DefaultISRHandler
    .set ISR232, DefaultISRHandler
    .set ISR233, DefaultISRHandler
    .set ISR234, DefaultISRHandler
    .set ISR235, DefaultISRHandler
    .set ISR236, DefaultISRHandler
    .set ISR237, DefaultISRHandler
    .set ISR238, DefaultISRHandler
    .set ISR239, DefaultISRHandler
    .set ISR240, DefaultISRHandler
    .set ISR241, DefaultISRHandler
    .set ISR242, DefaultISRHandler
    .set ISR243, DefaultISRHandler
    .set ISR244, DefaultISRHandler
    .set ISR245, DefaultISRHandler
    .set ISR246, DefaultISRHandler
    .set ISR247, DefaultISRHandler
    .set ISR248, DefaultISRHandler
    .set ISR249, DefaultISRHandler
    .set ISR250, DefaultISRHandler
    .set ISR251, DefaultISRHandler
    .set ISR252, DefaultISRHandler
    .set ISR253, DefaultISRHandler
    .set ISR254, DefaultISRHandler
    .set ISR255, DefaultISRHandler

  .text
end;

end.
