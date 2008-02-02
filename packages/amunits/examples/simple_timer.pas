program simpletimer;


uses exec, timer, amigados, amigalib;



{ manifest constants -- 'never will change' }
const
     SECSPERMIN   = (60);
     SECSPERHOUR  = (60*60);
     SECSPERDAY   = (60*60*24);

var
     seconds : longint;
     tr      : ptimerequest;      { IO block for timer commands }
     oldtimeval : ttimeval;   { timevals to store times     }
     mytimeval  : ttimeval;
     currentval : ttimeval;

Function Create_Timer(theUnit : longint) : pTimeRequest;
var
    Error : longint;
    TimerPort : pMsgPort;
    TimeReq : pTimeRequest;
begin
    TimerPort := CreatePort(Nil, 0);
    if TimerPort = Nil then
        Create_Timer := Nil;
    TimeReq := pTimeRequest(CreateExtIO(TimerPort,sizeof(tTimeRequest)));
    if TimeReq = Nil then begin
        DeletePort(TimerPort);
        Create_Timer := Nil;
    end;
    Error := OpenDevice(TIMERNAME, theUnit, pIORequest(TimeReq), 0);
    if Error <> 0 then begin
        DeleteExtIO(pIORequest(TimeReq));
        DeletePort(TimerPort);
        Create_Timer := Nil;
    end;
    TimerBase := pointer(TimeReq^.tr_Node.io_Device);
    Create_Timer := pTimeRequest(TimeReq);
end;

Procedure Delete_Timer(WhichTimer : pTimeRequest);
var
    WhichPort : pMsgPort;
begin

    WhichPort := WhichTimer^.tr_Node.io_Message.mn_ReplyPort;
    if assigned(WhichTimer) then begin
        CloseDevice(pIORequest(WhichTimer));
        DeleteExtIO(pIORequest(WhichTimer));
    end;
    if assigned(WhichPort) then
        DeletePort(WhichPort);
end;

procedure wait_for_timer(tr : ptimerequest; tv : ptimeval);
begin
    tr^.tr_node.io_Command := TR_ADDREQUEST; { add a new timer request }

    { structure assignment }
    tr^.tr_time.tv_secs := tv^.tv_secs;
    tr^.tr_time.tv_micro := tv^.tv_micro;

    { post request to the timer -- will go to sleep till done }
    DoIO(pIORequest(tr));
end;

{ more precise timer than AmigaDOS Delay() }
function time_delay(tv : ptimeval; theunit : longint): longint;
var
    tr : ptimerequest;
begin
    { get a pointer to an initialized timer request block }
    tr := create_timer(theunit);

    { any nonzero return says timedelay routine didn't work. }
    if tr = NIL then time_delay := -1;

    wait_for_timer(tr, tv);

    { deallocate temporary structures }
    delete_timer(tr);
    time_delay := 0;
end;

function set_new_time(secs : longint): longint;
var
    tr : ptimerequest;
begin
    tr := create_timer(UNIT_MICROHZ);

    { non zero return says error }
    if tr = nil then set_new_time := -1;

    tr^.tr_time.tv_secs := secs;
    tr^.tr_time.tv_micro := 0;
    tr^.tr_node.io_Command := TR_SETSYSTIME;
    DoIO(pIORequest(tr));

    delete_timer(tr);
    set_new_time := 0;
end;

function get_sys_time(tv : ptimeval): longint;
var
    tr : ptimerequest;
begin
    tr := create_timer( UNIT_MICROHZ );

    { non zero return says error }
    if tr = nil then get_sys_time := -1;

    tr^.tr_node.io_Command := TR_GETSYSTIME;
    DoIO(pIORequest(tr));

   { structure assignment }
   tv^ := tr^.tr_time;

   delete_timer(tr);
   get_sys_time := 0;
end;




procedure show_time(secs : longint);
var
   days,hrs,mins : longint;
begin
   { Compute days, hours, etc. }
   mins := secs div 60;
   hrs := mins div 60;
   days := hrs div 24;
   secs := secs  mod 60;
   mins := mins mod 60;
   hrs := hrs mod 24;

   { Display the time }
   writeln('*   Hour Minute Second  (Days since Jan.1,1978)');
   writeln('*   ', hrs, ':   ',mins,':   ', secs,'       (  ',days, ' )');
   writeln;
end;


begin
   writeln('Timer test');

   { sleep for two seconds }
   currentval.tv_secs := 2;
   currentval.tv_micro := 0;
   time_delay(@currentval, UNIT_VBLANK);
   writeln('After 2 seconds delay');

   { sleep for four seconds }
   currentval.tv_secs := 4;
   currentval.tv_micro := 0;
   time_delay(@currentval, UNIT_VBLANK);
   writeln('After 4 seconds delay');

   { sleep for 500,000 micro-seconds = 1/2 second }
   currentval.tv_secs := 0;
   currentval.tv_micro := 500000;
   time_delay(@currentval, UNIT_MICROHZ);
   writeln('After 1/2 second delay');

   writeln('DOS Date command shows: ');
   Execute('date', 0, 0);

   { save what system thinks is the time....we'll advance it temporarily }
   get_sys_time(@oldtimeval);
   writeln('Original system time is:');
   show_time(oldtimeval.tv_secs );

   writeln('Setting a new system time');

   seconds := 1000 * SECSPERDAY + oldtimeval.tv_secs;

   set_new_time( seconds );
   { (if user executes the AmigaDOS DATE command now, he will}
   { see that the time has advanced something over 1000 days }

   write('DOS Date command now shows: ');
   Execute('date', 0, 0);

   get_sys_time(@mytimeval);
   writeln('Current system time is:');
   show_time(mytimeval.tv_secs);

   { Added the microseconds part to show that time keeps }
   { increasing even though you ask many times in a row  }

   writeln('Now do three TR_GETSYSTIMEs in a row (notice how the microseconds increase)');
   writeln;
   get_sys_time(@mytimeval);
   writeln('First TR_GETSYSTIME      ',mytimeval.tv_secs,'.', mytimeval.tv_micro);
   get_sys_time(@mytimeval);
   writeln('Second TR_GETSYSTIME     ',mytimeval.tv_secs,'.', mytimeval.tv_micro);
   get_sys_time(@mytimeval);
   writeln('Third TR_GETSYSTIME      ',mytimeval.tv_secs,'.', mytimeval.tv_micro);
   writeln;
   writeln('Resetting to former time');
   set_new_time(oldtimeval.tv_secs);

   get_sys_time(@mytimeval);
   writeln('Current system time is:');
   show_time(mytimeval.tv_secs);

end.
