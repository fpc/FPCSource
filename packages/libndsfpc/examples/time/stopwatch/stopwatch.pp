program stopwatch;

{$mode objfpc}

uses
  ctypes, nds9;

//the speed of the timer when using ClockDivider_1024
const
  TIMER_SPEED = (BUS_CLOCK div 1024);

type
  TTimerStates = (timerState_Stop, timerState_Pause, timerState_Running);

var
  ticks: cuint = 0;
	state: TTimerStates = timerState_Stop;
	down: cint;
      
begin
  consoleDemoInit();

  down := keysDown();

  while (down and KEY_START) = 0 do
  begin
		swiWaitForVBlank();
		consoleClear();
		scanKeys();
		down := keysDown();

		if (state = timerState_Running) then
			ticks := ticks + timerElapsed(0);

		if (down and KEY_A) <> 0 then
		begin
			if (state = timerState_Stop) then
			begin
				timerStart(0, ClockDivider_1024, 0, nil);
				state := timerState_Running;
			end else 
      if (state = timerState_Pause) then
			begin
				timerUnpause(0);
				state := timerState_Running;
			end else 
      if (state = timerState_Running) then
			begin
				ticks := ticks + timerPause(0);
				state := timerState_Pause;
			end;
		end else 
    if (down and KEY_B) <> 0 then
		begin
			timerStop(0);
			ticks := 0;
			state := timerState_Stop;
		end;

		iprintf('Press A to start and pause the '#10'timer, B to clear the timer '#10'and start to quit the program.'#10#10);
		iprintf('ticks:  %u'#10, ticks);
		iprintf('second: %u:%u'#10, ticks div TIMER_SPEED, ((ticks mod TIMER_SPEED) * 1000) div TIMER_SPEED);
	end;

	if (state <> timerState_Stop) then
		timerStop(0);

end.
