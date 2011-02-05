{$ifdef NDS_INTERFACE}
type
  MotionCalibration = record
    xoff, yoff, zoff, goff: cshort;
    xsens, ysens, zsens, gsens: cshort;
  end;
  TMotionCalibration = MotionCalibration;
  PMotionCalibration = ^MotionCalibration;
  
function motion_init(): cint; cdecl; external;
procedure motion_deinit(); cdecl; external;
function motion_read_x(): csint; cdecl; external;
function motion_read_y(): csint; cdecl; external;
function motion_read_z(): csint; cdecl; external;
function motion_read_gyro(): csint; cdecl; external;
function motion_acceleration_x(): cint; cdecl; external;
function motion_acceleration_y(): cint; cdecl; external;
function motion_acceleration_z(): cint; cdecl; external;
procedure motion_set_sens_x(sens: cint); cdecl; external;
procedure motion_set_sens_y(sens: cint); cdecl; external;
procedure motion_set_sens_z(sens: cint); cdecl; external;
procedure motion_set_sens_gyro(sens: cint); cdecl; external;
procedure motion_set_offs_x(); cdecl; external;
procedure motion_set_offs_y(); cdecl; external;
procedure motion_set_offs_z(); cdecl; external;
procedure motion_set_offs_gyro(); cdecl; external;
function motion_rotation(): cint; cdecl; external;
function motion_get_calibration(): PMotionCalibration; cdecl; external;
procedure motion_set_calibration(cal: PMotionCalibration); cdecl; external;
procedure motion_enable_ain_1(); cdecl; external;
procedure motion_enable_ain_2(); cdecl; external;
function motion_read_ain_1(): cint; cdecl; external;
function motion_read_ain_2(): cint; cdecl; external;
{$endif NDS_INTERFACE}
