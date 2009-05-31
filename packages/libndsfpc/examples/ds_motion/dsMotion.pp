program dsMotion;


uses
  ctypes, nds9;

//sets the offset of the x and y and gyro and the 1 G offset of z
//to callivrate other offsets you would have to instruct the user to orient
//the DS in various positions  
procedure Calibrate();
begin
  scanKeys();
  
  consoleClear();
  
  iprintf('Set the DS on a flat table...'#10'Press A'#10);
  
  while ((not keysDown()) and KEY_A) <> 0 do
    scanKeys();
  
  //these set the zero points of the accelerometers and gryo
  motion_set_offs_x();
  motion_set_offs_y();
  motion_set_offs_gyro();
  
  //this should be set to the acceleration value at 1 z
  motion_set_sens_z(motion_read_z());
  
end;


begin	
  consoleDemoInit();
  
  motion_init();
  
  while true do
  begin	
    scanKeys();
    
    if (keysDown() and KEY_A) <> 0 then 
      Calibrate();
  
    consoleClear();
    if motion_init() <> 0 then
      iprintf('Nds is inserted'#10)
    else
      iprintf('Nds is not inserted'#10);
    
    iprintf('X: raw %i  miliG %i'#10, motion_read_x(), motion_acceleration_x());
    iprintf('Y: raw %i  miliG %i'#10, motion_read_y(), motion_acceleration_y());
    iprintf('Z: raw %i  miliG %i'#10, motion_read_z(), motion_acceleration_z());
    iprintf('R: raw %i  deg/sec %i'#10, motion_read_gyro(), motion_rotation());
    iprintf('Press A to calibrate'#10);	
    swiWaitForVBlank();
  end;

end.

