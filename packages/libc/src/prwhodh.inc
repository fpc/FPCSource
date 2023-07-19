
type
   Poutmp = ^outmp;
   outmp = record
        out_line : array[0..7] of AnsiChar;
        out_name : array[0..7] of AnsiChar;
        out_time : int32_t;
     end;

  whoent =  record
    we_utmp: outmp;
    we_idle: Integer;
  end;

   Pwhod = ^whod;
   whod = record
        wd_vers : AnsiChar;
        wd_type : AnsiChar;
        wd_pad : array[0..1] of AnsiChar;
        wd_sendtime : longint;
        wd_recvtime : longint;
        wd_hostname : array[0..31] of AnsiChar;
        wd_loadav : array[0..2] of longint;
        wd_boottime : longint;
        wd_we: packed array[0..(1024 div SizeOf(whoent))-1] of whoent;
     end;


const
   WHODVERSION = 1;
   WHODTYPE_STATUS = 1;

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
