program timercallback;


{$mode objfpc}

uses
  ctypes, nds9;
  
  
procedure waitfor(keys: cint);
begin
  scanKeys();
  while ((keysDown() and keys) = 0) do
  begin
    swiWaitForVBlank();
    scanKeys();
  end;
end;

var
  channel: cuint = 0;
  play: boolean = true;

//this function will be called by the timer.
procedure timerCallBack();
begin
	if (play) then
		soundPause(channel)
	else
		soundResume(channel);

	play := not play;
end;

begin
	soundEnable();
	channel := soundPlayPSG(DutyCycle_50, 10000, 127, 64);

	//calls the timerCallBack function 5 times per second.
	timerStart(0, ClockDivider_1024, TIMER_FREQ_1024(5), @timerCallBack);

	waitfor(KEY_A);

end.
