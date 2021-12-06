unit gpio;

interface

const
    PeripheralBase = $3F000000;

    GPFSEL0        = PeripheralBase + $00200000;
    GPFSEL1        = PeripheralBase + $00200004;
    GPFSEL2        = PeripheralBase + $00200008;
    GPFSEL3        = PeripheralBase + $0020000C;
    GPFSEL4        = PeripheralBase + $00200010;
    GPFSEL5        = PeripheralBase + $00200014;
    GPSET0         = PeripheralBase + $0020001C;
    GPSET1         = PeripheralBase + $00200020;
    GPCLR0         = PeripheralBase + $00200028;
    GPLEV0         = PeripheralBase + $00200034;
    GPLEV1         = PeripheralBase + $00200038;
    GPEDS0         = PeripheralBase + $00200040;
    GPEDS1         = PeripheralBase + $00200044;
    GPHEN0         = PeripheralBase + $00200064;
    GPHEN1         = PeripheralBase + $00200068;
    GPPUD          = PeripheralBase + $00200094;
    GPPUDCLK0      = PeripheralBase + $00200098;
    GPPUDCLK1      = PeripheralBase + $0020009C;

implementation

end.