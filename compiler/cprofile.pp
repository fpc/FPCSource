{
    Copyright (c) 2018 by Florian Klaempfl

    Basic infrastructure for measuring timings of different compilation steps

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}
unit cprofile;

{$define COMPILER_TIMINGS}

{$i fpcdefs.inc}

interface

  uses
    globals;

  type
    TCTimer = (
      ct_none,
      ct_aopt);

  procedure ResumeTimer(t : TCTimer);
  procedure StopTimer;

implementation

{$ifndef COMPILER_TIMINGS}
{$else COMPILER_TIMINGS}
  uses
    epiktimer in '../../epiktimer/epiktimer.pas';

  var
    currenttimer : TCTimer;
    timers : array[TCTimer] of TEpikTimer;


  procedure ResumeTimer(t : TCTimer);
    begin
      timers[currenttimer].Stop;
      timers[t].Start;
      currenttimer:=t;
    end;


  procedure StopTimer;
    begin
      timers[currenttimer].Stop;
      currenttimer:=ct_none;
      timers[currenttimer].Start;
    end;

var
  t : TCTimer;

initialization
  for t:=low(timers) to high(timers) do
    timers[t]:=TEpikTimer.Create;
  currenttimer:=ct_none;
finalization
  timers[currenttimer].Stop;
  for t:=low(timers) to high(timers) do
    writeln(StdErr,t,' ',timers[t].Elapsed);
  for t:=low(timers) to high(timers) do
    timers[t].Free;
{$endif COMPILER_TIMINGS}
end.

