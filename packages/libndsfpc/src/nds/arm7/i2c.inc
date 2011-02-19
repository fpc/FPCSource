{$ifndef ARM7}
  {$error i2c header is for ARM7 only}
{$endif}

{$ifdef NDS_INTERFACE}
const
  REG_I2CDATA : pcuint8 = pointer($4004500);
  REG_I2CCNT	: pcuint8 = pointer($4004501);

procedure i2cWaitBusy(); inline;

type
  i2cDevices = integer;
const
	I2C_CAM0: i2cDevices = $7A;
	I2C_CAM1: i2cDevices = $78;
	I2C_UNK1: i2cDevices = $A0;
	I2C_UNK2: i2cDevices = $E0;
	I2C_PM  : i2cDevices = $4A;
	I2C_UNK3: i2cDevices = $40;
	I2C_GPIO: i2cDevices = $90;


  // Registers for Power Management (I2C_PM)
  I2CREGPM_BATUNK     = $00;
  I2CREGPM_PWRIF      = $10;
  I2CREGPM_PWRCNT     = $11;
  I2CREGPM_MMCPWR     = $12;
  I2CREGPM_BATTERY    = $20;
  I2CREGPM_CAMLED     = $31;
  I2CREGPM_VOL        = $40;
  I2CREGPM_RESETFLAG  = $70;


function i2cWriteRegister(device, reg, data: cuint8): cuint8; cdecl; external;
function i2cReadRegister(device, reg: cuint8): cuint8; cdecl; external;

{$endif NDS_INTERFACE}

{$ifdef NDS_IMPLEMENTATION}
procedure i2cWaitBusy(); inline;
begin
  while (REG_I2CCNT^ and $80) <> 0 do;
end;
{$endif NDS_IMPLEMENTATION}