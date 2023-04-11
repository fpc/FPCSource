unit rp2040;
interface
{$PACKRECORDS C}
{$GOTO ON}
{$SCOPEDENUMS ON}

type
  TIRQn_Enum = (
    NonMaskableInt_IRQn = -14,        
    HardFault_IRQn = -13,             
    SVC_IRQn    = -5,                 
    PendSV_IRQn = -2,                 
    SysTick_IRQn = -1,                
    TIMER_IRQ_0 = 0,
    TIMER_IRQ_1 = 1,
    TIMER_IRQ_2 = 2,
    TIMER_IRQ_3 = 3,
    PWM_IRQ_WRAP =4,
    USBCTRL_IRQ =5,
    XIP_IRQ     =6,
    PIO0_IRQ_0  =7,
    PIO0_IRQ_1  =8,
    PIO1_IRQ_0  =9,
    PIO1_IRQ_1  =10,
    DMA_IRQ_0   =11,
    DMA_IRQ_1   =12,
    IO_IRQ_BANK0=13,
    IO_IRQ_QSPI  =14,
    SIO_IRQ_PROC0=15,
    SIO_IRQ_PROC1 =16,
    CLOCKS_IRQ =17,
    SPI0_IRQ =18,
    SPI1_IRQ =19,
    UART0_IRQ =20,
    UART1_IRQ =21,
    ADC0_IRQ_FIFO=22,
    I2C0_IRQ=23,
    I2C1_IRQ=24,
    RTC_IRQ=25
  );

type
  TADC_Registers = record
    cs : longWord;
    result : longWord;
    fcs : longWord;
    fifo : longWord;
    &div : longWord;
    intr : longWord;
    inte : longWord;
    intf : longWord;
    ints : longWord;
  end;

  TBUSCTRL_Registers = record
    priority : longWord;
    priority_ack : longWord;
    perf : array[0..3] of record
      ctr : longWord;
      sel : longWord;
    end;
  end;

  TCLOCK_Registers = record
    ctrl : longWord;
    &div : longWord;
    selected : longWord;
  end;

  TFC_Registers = record
    ref_khz : longWord;
    min_khz : longWord;
    max_khz : longWord;
    delay : longWord;
    interval : longWord;
    src : longWord;
    status : longWord;
    result : longWord;
  end;

  TCLOCKS_Registers = record
    clk_gpout : array[0..3] of TCLOCK_Registers;
    clk_ref : TCLOCK_Registers;
    clk_sys : TCLOCK_Registers;
    clk_peri : TCLOCK_Registers;
    clk_usb : TCLOCK_Registers;
    clk_adc : TCLOCK_Registers;
    clk_rtc : TCLOCK_Registers;
    clk_sys_resus : record
      ctrl : longWord;
      status : longWord;
    end;
    fc0 : TFC_Registers;
    wake_en0 : longWord;
    wake_en1 : longWord;
    sleep_en0 : longWord;
    sleep_en1 : longWord;
    enabled0 : longWord;
    enabled1 : longWord;
    intr : longWord;
    inte : longWord;
    intf : longWord;
    ints : longWord;
  end;

  TDMACHANNEL_Registers = record
    read_addr : longWord;
    write_addr : longWord;
    transfer_count : longWord;
    ctrl_trig : longWord;
    al1_ctrl : longWord;
    al1_read_addr : longWord;
    al1_write_addr : longWord;
    al1_transfer_count_trig : longWord;
    al2_ctrl : longWord;
    al2_transfer_count : longWord;
    al2_read_addr : longWord;
    al2_write_addr_trig : longWord;
    al3_ctrl : longWord;
    al3_write_addr : longWord;
    al3_transfer_count : longWord;
    al3_read_addr_trig : longWord;
  end;

  TDMA_Registers = record
    ch : array[0..11] of TDMACHANNEL_Registers;
    RESERVED0 : array[0..63] of longWord;
    intr : longWord;
    inte0 : longWord;
    intf0 : longWord;
    ints0 : longWord;
    RESERVED1 : longWord;
    inte1 : longWord;
    intf1 : longWord;
    ints1 : longWord;
    timer : array[0..1] of longWord;
    RESERVED2 : array[0..1] of longWord;
    multi_channel_trigger : longWord;
    sniff_ctrl : longWord;
    sniff_data : longWord;
    RESERVED3 : longWord;
    fifo_levels : longWord;
    abort : longWord;
  end;

  TDMADEBUG_Registers = record
    ch : array[0..11] of record
      ctrdeq : longWord;
      tcr : longWord;
      RESERVED0 : array[0..13] of longWord;
    end;
  end;

  TI2C_Registers = record
    con : longWord;
    tar : longWord;
    sar : longWord;
    RESERVED0 : longWord;
    data_cmd : longWord;
    ss_scl_hcnt : longWord;
    ss_scl_lcnt : longWord;
    fs_scl_hcnt : longWord;
    fs_scl_lcnt : longWord;
    RESERVED1 : array[0..1] of longWord;
    intr_stat : longWord;
    intr_mask : longWord;
    raw_intr_stat : longWord;
    rx_tl : longWord;
    tx_tl : longWord;
    clr_intr : longWord;
    clr_rx_under : longWord;
    clr_rx_over : longWord;
    clr_tx_over : longWord;
    clr_rd_req : longWord;
    clr_tx_abrt : longWord;
    clr_rx_done : longWord;
    clr_activity : longWord;
    clr_stop_det : longWord;
    clr_start_det : longWord;
    clr_gen_call : longWord;
    enable : longWord;
    status : longWord;
    txflr : longWord;
    rxflr : longWord;
    sda_hold : longWord;
    tx_abrt_source : longWord;
    slv_data_nack_only : longWord;
    dma_cr : longWord;
    dma_tdlr : longWord;
    dma_rdlr : longWord;
    sda_setup : longWord;
    ack_general_call : longWord;
    enable_status : longWord;
    fs_spklen : longWord;
    RESERVED2 : longWord;
    clr_restart_det : longWord;
    RESERVED3 : array[0..17] of longWord;
    comp_param_1 : longWord;
    comp_version : longWord;
    comp_type : longWord;
  end;

  TIOIRQCTRL_Registers = record
    inte : array[0..3] of longWord;
    intf : array[0..3] of longWord;
    ints : array[0..3] of longWord;
  end;

  TIOBANK0_Registers = record
    io : array[0..29] of record
      status : longWord;
      ctrl : longWord;
    end;
    intr : array[0..3] of longWord;
    proc0_irq_ctrl : TIOIRQCTRL_Registers;
    proc1_irq_ctrl : TIOIRQCTRL_Registers;
    dormant_wake_irq_ctrl : TIOIRQCTRL_Registers;
  end;

  TIOQSPI_Registers = record
    io : array[0..5] of record
      status : longWord;
      ctrl : longWord;
    end;
  end;

  TPADSQSPI_Registers = record
    voltage_select : longWord;
    io : array[0..5] of longWord;
  end;

  TPADSBANK0_Registers = record
    voltage_select : longWord;
    io : array[0..29] of longWord;
  end;

  TPIO_Registers = record
    ctrl : longWord;
    fstat : longWord;
    fdebug : longWord;
    flevel : longWord;
    txf : array[0..1] of longWord;
    rxf : array[0..1] of longWord;
    irq : longWord;
    irq_force : longWord;
    input_sync_bypass : longWord;
    dbg_padout : longWord;
    dbg_padoe : longWord;
    dbg_cfginfo : longWord;
    instr_mem : array[0..31] of longWord;
    sm : array[0..1] of record
      clkdiv : longWord;
      execctrl : longWord;
      shiftctrl : longWord;
      addr : longWord;
      instr : longWord;
      pinctrl : longWord;
    end;
    intr : longWord;
    inte0 : longWord;
    intf0 : longWord;
    ints0 : longWord;
    inte1 : longWord;
    intf1 : longWord;
    ints1 : longWord;
  end;

  TPLL_Registers = record
    cs : longWord;
    pwr : longWord;
    fbdiv_int : longWord;
    prim : longWord;
  end;

  TPSM_Registers = record
    frce_on : longWord;
    frce_off : longWord;
    wdsel : longWord;
    done : longWord;
  end;

  TPWMSLICE_Registers = record
    csr : longWord;
    &div : longWord;
    ctr : longWord;
    cc : longWord;
    top : longWord;
  end;

  TPWM_Registers = record
    slice : array[0..7] of TPWMSLICE_Registers;
    en : longWord;
    intr : longWord;
    inte : longWord;
    intf : longWord;
    ints : longWord;
  end;

  TRESETS_Registers = record
    reset : longWord;
    wdsel : longWord;
    reset_done : longWord;
  end;

  TROSC_Registers = record
    ctrl : longWord;
    freqa : longWord;
    freqb : longWord;
    dormant : longWord;
    &div : longWord;
    phase : longWord;
    status : longWord;
    randombit : longWord;
    count : longWord;
    dftx : longWord;
  end;

  TRTC_Registers = record
    clkdiv_m1 : longWord;
    setup_0 : longWord;
    setup_1 : longWord;
    ctrl : longWord;
    irq_setup_0 : longWord;
    irq_setup_1 : longWord;
    rtc_1 : longWord;
    rtc_0 : longWord;
    intr : longWord;
    inte : longWord;
    intf : longWord;
    ints : longWord;
  end;

  TINTERP_Registers = record
    accum : array[0..1] of longWord;
    base : array[0..2] of longWord;
    pop : array[0..2] of longWord;
    peek : array[0..2] of longWord;
    ctrl : array[0..1] of longWord;
    add_raw : array[0..1] of longWord;
    base01 : longWord;
  end;

  TSIO_Registers = record
    cpuid : longWord;
    gpio_in : longWord;
    gpio_hi_in : longWord;
    RESERVED0 : longWord;
    gpio_out : longWord;
    gpio_set : longWord;
    gpio_clr : longWord;
    gpio_togl : longWord;
    gpio_oe : longWord;
    gpio_oe_set : longWord;
    gpio_oe_clr : longWord;
    gpio_oe_togl : longWord;
    gpio_hi_out : longWord;
    gpio_hi_set : longWord;
    gpio_hi_clr : longWord;
    gpio_hi_togl : longWord;
    gpio_hi_oe : longWord;
    gpio_hi_oe_set : longWord;
    gpio_hi_oe_clr : longWord;
    gpio_hi_oe_togl : longWord;
    fifo_st : longWord;
    fifo_wr : longWord;
    fifo_rd : longWord;
    spinlock_st : longWord;
    div_udividend : longWord;
    div_udivisor : longWord;
    div_sdividend : longWord;
    div_sdivisor : longWord;
    div_quotient : longWord;
    div_remainder : longWord;
    div_csr : longWord;
    RESERVED1 : longWord;
    interp : array[0..1] of TINTERP_Registers;
    spinlock : array[0..31] of longWord;
  end;

  TSPI_Registers = record
    cr0 : longWord;
    cr1 : longWord;
    dr : longWord;
    sr : longWord;
    cpsr : longWord;
    imsc : longWord;
    ris : longWord;
    mis : longWord;
    icr : longWord;
    dmacr : longWord;
  end;

  TSSI_Registers = record
    ctrlr0 : longWord;
    ctrlr1 : longWord;
    ssienr : longWord;
    mwcr : longWord;
    ser : longWord;
    baudr : longWord;
    txftlr : longWord;
    rxftlr : longWord;
    txflr : longWord;
    rxflr : longWord;
    sr : longWord;
    imr : longWord;
    isr : longWord;
    risr : longWord;
    txoicr : longWord;
    rxoicr : longWord;
    rxuicr : longWord;
    msticr : longWord;
    icr : longWord;
    dmacr : longWord;
    dmatdlr : longWord;
    dmardlr : longWord;
    idr : longWord;
    ssi_version_id : longWord;
    dr0 : longWord;
    RESERVED0 : array[0..34] of longWord;
    rx_sample_dly : longWord;
    spi_ctrlr0 : longWord;
    txd_drive_edge : longWord;
  end;

  TSYSCFG_Registers = record
    proc0_nmi_mask : longWord;
    proc1_nmi_mask : longWord;
    proc_config : longWord;
    proc_in_sync_bypass : longWord;
    proc_in_sync_bypass_hi : longWord;
    dbgforce : longWord;
    mempowerdown : longWord;
  end;

  TSYSINFO_Registers = record
    chip_id : longWord;
    platform : longWord;
    reserved0 : array[0..$3F-$08] of longWord;
    gitref_rp2040 : longWord;
  end;

  TTIMER_Registers = record
    timehw : longWord;
    timelw : longWord;
    timehr : longWord;
    timelr : longWord;
    alarm : array[0..3] of longWord;
    armed : longWord;
    timerawh : longWord;
    timerawl : longWord;
    dbgpause : longWord;
    pause : longWord;
    intr : longWord;
    inte : longWord;
    intf : longWord;
    ints : longWord;
  end;

  TUART_Registers = record
    dr : longWord;
    rsr : longWord;
    RESERVED0 : array[0..3] of longWord;
    fr : longWord;
    RESERVED1 : longWord;
    ilpr : longWord;
    ibrd : longWord;
    fbrd : longWord;
    lcr_h : longWord;
    cr : longWord;
    ifls : longWord;
    imsc : longWord;
    ris : longWord;
    mis : longWord;
    icr : longWord;
    dmacr : longWord;
  end;

  TUSBDEVICEDPRAM = record
    setup_packet : array[0..7] of byte;
    ep_ctrl : array[0..14] of record
      &in : longWord;
      &out : longWord;
    end;
    ep_buf_ctrl : array[0..15] of record
      &in : longWord;
      &out : longWord;
    end;
    ep0_buf_a : array[0..63] of byte;
    ep0_buf_b : array[0..63] of byte;
    epx_data : array[0..(4096-$180)-1] of byte;
  end;

  TUSBHOSTDPRAM = record
    setup_packet : array[0..7] of byte;
    int_ep_ctrl : array[0..14] of record
      ctrl : longWord;
      spare : longWord;
    end;
    epx_buf_ctrl : longWord;
    _spare0 : longWord;
    int_ep_buffer_ctrl : array[0..14] of record
      ctrl : longWord;
      spare : longWord;
    end;
    epx_ctrl : longWord;
    _spare1 : array[0..123] of byte;
    epx_data : array[0..(4096-$180)-1] of byte;
  end;

  TUSB_Registers = record
    dev_addr_ctrl : longWord;
    int_ep_addr_ctrl : array[1..15] of longWord;
    main_ctrl : longWord;
    sof_wr : longWord;
    sof_rd : longWord;
    sie_ctrl : longWord;
    sie_status : longWord;
    int_ep_ctrl : longWord;
    buf_status : longWord;
    buf_cpu_should_handle : longWord;
    abort : longWord;
    abort_done : longWord;
    ep_stall_arm : longWord;
    nak_poll : longWord;
    ep_nak_stall_status : longWord;
    muxing : longWord;
    pwr : longWord;
    phy_direct : longWord;
    phy_direct_override : longWord;
    phy_trim : longWord;
    linestate_tuning : longWord;
    intr : longWord;
    inte : longWord;
    intf : longWord;
    ints : longWord;
  end;

  TVREGANDCHIPRESET_Registers = record
    vreg : longWord;
    bod : longWord;
    chip_reset : longWord;
  end;

  TWATCHDOG_Registers = record
    ctrl : longWord;
    load : longWord;
    reason : longWord;
    scratch : array[0..7] of longWord;
    tick : longWord;
  end;

  TXIPCTRL_Registers = record
    ctrl : longWord;
    flush : longWord;
    stat : longWord;
    ctr_hit : longWord;
    ctr_acc : longWord;
    stream_addr : longWord;
    stream_ctr : longWord;
    stream_fifo : longWord;
  end;

  TXOSC_Registers = record
    ctrl : longWord;
    status : longWord;
    dormant : longWord;
    startup : longWord;
    RESERVED0 : array[0..2] of longWord;
    count : longWord;
  end;

  TMPU_Registers = record
    _type : longWord;
    ctrl : longWord;
    rnr : longWord;
    rbar : longWord;
    rasr : longWord;
  end;

  TSYSTICK_Registers = record
    csr : longWord;
    rvr : longWord;
    cvr : longWord;
    calib : longWord;
  end;

  TSCB_Reqisters = record
    cpuid : longWord;
    icsr : longWord;
    vtor : longWord;
    aircr : longWord;
    scr : longWord;
  end;

const
  __NVIC_PRIO_BITS= 2;
  SRAM0_BASE      = $21000000;
  SRAM1_BASE      = $21010000;
  SRAM2_BASE      = $21020000;
  SRAM3_BASE      = $21030000;
  SYSINFO_BASE    = $40000000;
  SYSCFG_BASE     = $40004000;
  CLOCKS_BASE     = $40008000;
  RESETS_BASE     = $4000c000;
  PSM_BASE        = $40010000;
  IO_BANK0_BASE   = $40014000;
  IO_QSPI_BASE    = $40018000;
  PADS_BANK0_BASE = $4001c000;
  PADS_QSPI_BASE  = $40020000;
  XOSC_BASE       = $40024000;
  PLL_SYS_BASE    = $40028000;
  PLL_USB_BASE    = $4002c000;
  BUSCTRL_BASE    = $40030000;
  UART0_BASE      = $40034000;
  UART1_BASE      = $40038000;
  SPI0_BASE       = $4003c000;
  SPI1_BASE       = $40040000;
  I2C0_BASE       = $40044000;
  I2C1_BASE       = $40048000;
  ADC_BASE        = $4004c000;
  PWM_BASE        = $40050000;
  TIMER_BASE      = $40054000;
  WATCHDOG_BASE   = $40058000;
  RTC_BASE        = $4005c000;
  ROSC_BASE       = $40060000;
  VREG_AND_CHIP_RESET_BASE = $40064000;
  TBMAN_BASE      = $4006c000;
  DMA_BASE        = $50000000;
  USBCTRL_BASE    = $50100000;
  USBCTRL_DPRAM_BASE = $50100000;
  USBCTRL_REGS_BASE = $50110000;
  PIO0_BASE       = $50200000;
  PIO1_BASE       = $50300000;
  XIP_AUX_BASE    = $50400000;
  SIO_BASE        = $d0000000;
  PPB_BASE        = $e0000000;

var
  SysInfo : TSysInfo_Registers absolute SYSINFO_BASE;
  SysCfg : TSYSCFG_REGISTERS absolute SYSCFG_BASE;
  Clocks : TCLOCKS_Registers absolute CLOCKS_BASE;
  Resets : TRESETS_Registers absolute RESETS_BASE;
  PSM : TPSM_Registers absolute PSM_BASE;
  IOBANK0 : TIOBANK0_Registers absolute IO_BANK0_BASE;
  IOQSPI : TIOQSPI_Registers absolute IO_QSPI_BASE;
  PADSBANK0 : TPADSBANK0_Registers absolute PADS_BANK0_BASE;
  PADSQSPI : TPADSQSPI_Registers absolute PADS_QSPI_BASE;
  XOSC : TXOSC_Registers absolute XOSC_BASE;
  PLLSYS : TPLL_Registers absolute PLL_SYS_BASE;
  PLLUSB : TPLL_Registers absolute PLL_USB_BASE;
  BUSCTRL : TBUSCTRL_Registers absolute BUSCTRL_BASE;
  UART0 : TUART_Registers absolute UART0_BASE;
  UART1 : TUART_Registers absolute UART1_BASE;
  SPI0 : TSPI_Registers absolute SPI0_BASE;
  SPI1 : TSPI_Registers absolute SPI1_BASE;
  I2C0 : TI2C_Registers absolute I2C0_BASE;
  I2C1 : TI2C_Registers absolute I2C1_BASE;
  ADC : TADC_Registers absolute ADC_BASE;
  PWM : TPWM_Registers absolute PWM_BASE;
  TIMER : TTIMER_Registers absolute TIMER_BASE;
  WATCHDOG : TWATCHDOG_Registers absolute WATCHDOG_BASE;
  RTC : TRTC_Registers absolute RTC_BASE;
  ROSC : TROSC_Registers absolute ROSC_BASE;
  VREGANDCHIPRESET : TVREGANDCHIPRESET_Registers absolute VREG_AND_CHIP_RESET_BASE;
  DMA : TDMA_Registers absolute DMA_BASE;
  //USBCTRL_BASE = $50100000
  //USBCTRL_DPRAM_BASE = $50100000
  USB : TUSB_Registers absolute USBCTRL_REGS_BASE;
  PIO0 : TPIO_Registers absolute PIO0_BASE;
  PIO1 : TPIO_Registers absolute PIO1_BASE;
  //XIP_AUX_BASE = $50400000
  SIO : TSIO_Registers absolute SIO_BASE;

implementation

procedure NMI_Handler; external name 'NMI_Handler';
procedure HardFault_Handler; external name 'HardFault_Handler';
procedure SVC_Handler; external name 'SVC_Handler';
procedure PendSV_Handler; external name 'PendSV_Handler';
procedure SysTick_Handler; external name 'SysTick_Handler';
procedure TIMER_IRQ_0_Handler;  external name 'TIMER_IRQ_0_Handler';
procedure TIMER_IRQ_1_Handler;  external name 'TIMER_IRQ_1_Handler';
procedure TIMER_IRQ_2_Handler;  external name 'TIMER_IRQ_2_Handler';
procedure TIMER_IRQ_3_Handler;  external name 'TIMER_IRQ_3_Handler';
procedure PWM_IRQ_WRAP_Handler;  external name 'PWM_IRQ_WRAP_Handler';
procedure USBCTRL_IRQ_Handler;  external name 'USBCTRL_IRQ_Handler';
procedure XIP_IRQ_Handler;  external name 'XIP_IRQ_Handler';
procedure PIO0_IRQ_0_Handler;  external name 'PIO0_IRQ_0_Handler';
procedure PIO0_IRQ_1_Handler;  external name 'PIO0_IRQ_1_Handler';
procedure PIO1_IRQ_0_Handler;  external name 'PIO1_IRQ_0_Handler';
procedure PIO1_IRQ_1_Handler; external name 'PIO1_IRQ_1_Handler';
procedure DMA_IRQ_0_Handler; external name 'DMA_IRQ_0_Handler';
procedure DMA_IRQ_1_Handler; external name 'DMA_IRQ_1_Handler';
procedure IO_IRQ_BANK0_Handler; external name 'IO_IRQ_BANK0_Handler';
procedure IO_IRQ_QSPI_Handler; external name 'IO_IRQ_QSPI_Handler';
procedure SIO_IRQ_PROC0_Handler; external name 'SIO_IRQ_PROC0_Handler';
procedure SIO_IRQ_PROC1_Handler; external name 'SIO_IRQ_PROC1_Handler';
procedure CLOCKS_IRQ_Handler; external name 'CLOCKS_IRQ_Handler';
procedure SPI0_IRQ_Handler; external name 'SPI0_IRQ_Handler';
procedure SPI1_IRQ_Handler; external name 'SPI1_IRQ_Handler';
procedure UART0_IRQ_Handler; external name 'UART0_IRQ_Handler';
procedure UART1_IRQ_Handler; external name 'UART1_IRQ_Handler';
procedure ADC_IRQ_FIFO_Handler; external name 'ADC_IRQ_FIFO_Handler';
procedure I2C0_IRQ_Handler; external name 'I2C0_IRQ_Handler';
procedure I2C1_IRQ_Handler; external name 'I2C1_IRQ_Handler';
procedure RTC_IRQ_Handler; external name 'RTC_IRQ_Handler';

{$i cortexm0p_start.inc}

procedure Vectors; assembler; nostackframe;
label interrupt_vectors;
asm
  .section ".init.interrupt_vectors"
  interrupt_vectors:
  .long _stack_top
  .long Startup
  .long NMI_Handler
  .long HardFault_Handler
  .long 0
  .long 0
  .long 0
  .long 0
  .long 0
  .long 0
  .long 0
  .long SVC_Handler
  .long 0
  .long 0
  .long PendSV_Handler
  .long SysTick_Handler
  .long TIMER_IRQ_0_Handler
  .long TIMER_IRQ_1_Handler
  .long TIMER_IRQ_2_Handler
  .long TIMER_IRQ_3_Handler
  .long PWM_IRQ_WRAP_Handler
  .long USBCTRL_IRQ_Handler
  .long XIP_IRQ_Handler
  .long PIO0_IRQ_0_Handler
  .long PIO0_IRQ_1_Handler
  .long PIO1_IRQ_0_Handler
  .long PIO1_IRQ_1_Handler
  .long DMA_IRQ_0_Handler
  .long DMA_IRQ_1_Handler
  .long IO_IRQ_BANK0_Handler
  .long IO_IRQ_QSPI_Handler
  .long SIO_IRQ_PROC0_Handler
  .long SIO_IRQ_PROC1_Handler
  .long CLOCKS_IRQ_Handler
  .long SPI0_IRQ_Handler
  .long SPI1_IRQ_Handler
  .long UART0_IRQ_Handler
  .long UART1_IRQ_Handler
  .long ADC_IRQ_FIFO_Handler
  .long I2C0_IRQ_Handler
  .long I2C1_IRQ_Handler
  .long RTC_IRQ_Handler
  .long 0
  .long 0
  .long 0
  .long 0
  .long 0
  .long 0


  .weak NMI_Handler
  .weak HardFault_Handler
  .weak SVC_Handler
  .weak PendSV_Handler
  .weak SysTick_Handler
  .weak TIMER_IRQ_0_Handler
  .weak TIMER_IRQ_1_Handler
  .weak TIMER_IRQ_2_Handler
  .weak TIMER_IRQ_3_Handler
  .weak PWM_IRQ_WRAP_Handler
  .weak USBCTRL_IRQ_Handler
  .weak XIP_IRQ_Handler
  .weak PIO0_IRQ_0_Handler
  .weak PIO0_IRQ_1_Handler
  .weak PIO1_IRQ_0_Handler
  .weak PIO1_IRQ_1_Handler
  .weak DMA_IRQ_0_Handler
  .weak DMA_IRQ_1_Handler
  .weak IO_IRQ_BANK0_Handler
  .weak IO_IRQ_QSPI_Handler
  .weak SIO_IRQ_PROC0_Handler
  .weak SIO_IRQ_PROC1_Handler
  .weak CLOCKS_IRQ_Handler
  .weak SPI0_IRQ_Handler
  .weak SPI1_IRQ_Handler
  .weak UART0_IRQ_Handler
  .weak UART1_IRQ_Handler
  .weak ADC_IRQ_FIFO_Handler
  .weak I2C0_IRQ_Handler
  .weak I2C1_IRQ_Handler
  .weak RTC_IRQ_Handler

  .set NMI_Handler, _NMI_Handler
  .set HardFault_Handler, _HardFault_Handler
  .set SVC_Handler, _SVC_Handler
  .set PendSV_Handler, _PendSV_Handler
  .set SysTick_Handler, _SysTick_Handler
  .set TIMER_IRQ_0_Handler, Haltproc
  .set TIMER_IRQ_1_Handler, Haltproc
  .set TIMER_IRQ_2_Handler, Haltproc
  .set TIMER_IRQ_3_Handler, Haltproc
  .set PWM_IRQ_WRAP_Handler, Haltproc
  .set USBCTRL_IRQ_Handler, Haltproc
  .set XIP_IRQ_Handler, Haltproc
  .set PIO0_IRQ_0_Handler, Haltproc
  .set PIO0_IRQ_1_Handler, Haltproc
  .set PIO1_IRQ_0_Handler, Haltproc
  .set PIO1_IRQ_1_Handler, Haltproc
  .set DMA_IRQ_0_Handler, Haltproc
  .set DMA_IRQ_1_Handler, Haltproc
  .set IO_IRQ_BANK0_Handler, Haltproc
  .set IO_IRQ_QSPI_Handler, Haltproc
  .set SIO_IRQ_PROC0_Handler, Haltproc
  .set SIO_IRQ_PROC1_Handler, Haltproc
  .set CLOCKS_IRQ_Handler, Haltproc
  .set SPI0_IRQ_Handler, Haltproc
  .set SPI1_IRQ_Handler, Haltproc
  .set UART0_IRQ_Handler, Haltproc
  .set UART1_IRQ_Handler, Haltproc
  .set ADC_IRQ_FIFO_Handler, Haltproc
  .set I2C0_IRQ_Handler, Haltproc
  .set I2C1_IRQ_Handler, Haltproc
  .set RTC_IRQ_Handler, Haltproc
  .text
  end;
end.
