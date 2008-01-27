unit timer;

{ NOT PORTED YET, BUT NOT USED BY OTHER LIBS/AND OR DLL AND MOST DEMOES}


{$r-,s-}

INTERFACE

var
  timeractive: boolean;
  exacttime, mstime: longint;

function timervalue: longint;          {Return time in 10 usec units}
function mstimer: longint;             {Return time in ms}

IMPLEMENTATION

uses dos, crt;

var
  lowbyte, highbyte, ref: word;
  timerid: integer;
  saveint, exitsave: pointer;

function inport(x: integer): byte;     {Read i/o port}
  inline($5a/$eb/$00/$ec);

{$F+}
procedure clock(p: pointer); interrupt;
{$F-}
  {Interrupt service routine to update timer reference values}

  const
    incr = 5493;                       {Timer increment per interrupt}

  begin
    port[$43] := $00;                  {Latch timer 0}
    lowbyte := inport($40);
    highbyte := inport($40);
    ref := (highbyte shl 8) + lowbyte; {Base for subsequent readings
                                          within current clock interval}
    exacttime := exacttime + incr;     {New 10 usec timer value}
    mstime := mstime + 55;             {New ms timer value}
    inline($9c/$ff/$1e/saveint);       {Chain to old interrupt}
  end;

function timervalue: longint;

  {Get value of 10-usec timer}

  var
    dif, low, high: word;
    t: longint;

  begin
    inline($fa);                         {Disable interrupts}
    port[$43] := $00;                    {Latch timer}
    low := inport($40);                  {Timer LSB}
    high := inport($40);                 {MSB}
    dif := ref - ((high shl 8) + low);   {Delta from last sync}
    timervalue := exacttime + (longint(dif)*100 div 1193);
    inline($fb);                         {Re-enable interrupts}
  end;

function mstimer: longint;

  {Get value of millisecond timer}

  var
    dif, low, high: word;
    t: longint;

  begin
    inline($fa);
    port[$43] := $00;
    low := inport($40);
    high := inport($40);
    inline($fb);
    dif := ref - ((high shl 8) + low);
    mstimer := mstime + (dif div 1193);
  end;

procedure inittimer;

  begin
    exacttime := 0;
    mstime := 0;
    if not timeractive then
      begin
        port[$43] := $34;   {Mode 2 - countdown
                             (approx .84 microsecond ticks)}
        port[$40] := $ff;   {Initialize timer value}
        port[$40] := $ff;
        getintvec(8, saveint);         {Save old interrupt address}
        setintvec(8, @clock);          {Install new service routine}
        timeractive := true;
        delay(60);                     {Allow for first tick}
      end;
  end;

{$f+} procedure myexit; {$f-}

  {Assure timer interrupt restored before exit}

  begin
    if timeractive then
      setintvec(8, saveint);
    exitproc := exitsave;             {Restore TP exit chain}
  end;

begin  {unit initialization}
  timeractive := false;
  exitsave := exitproc;               {Insert exit routine}
  exitproc := @myexit;
  InitTimer
end.
