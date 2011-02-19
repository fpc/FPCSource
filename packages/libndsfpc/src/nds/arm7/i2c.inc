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
	I2C_UNK4: i2cDevices = $90;


function i2cWriteRegister(device, reg, data: cuint8): cuint8; cdecl; external;
function i2cReadRegister(device, reg: cuint8): cuint8; cdecl; external;

{$endif NDS_INTERFACE}

{$ifdef NDS_IMPLEMENTATION}
procedure i2cWaitBusy(); inline;
begin
  while (REG_I2CCNT^ and $80) <> 0 do;
end;
{$endif NDS_IMPLEMENTATION}