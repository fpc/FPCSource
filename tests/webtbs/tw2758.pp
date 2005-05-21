{ Source provided for Free Pascal Bug Report 2758 }
{ Submitted by "marco" on  2003-10-31 }
{ e-mail:  }
Type
  sigcontextrec=record end;
  SignalHandler   = Procedure(Sig : Longint);cdecl;
  PSignalHandler  = ^SignalHandler;
  TSigAction = procedure(Sig: Longint; SigContext: SigContextRec);cdecl;

SigActionRec = packed record  // this is temporary for the migration
    sa_handler : signalhandler;
  end;

procedure signaltorunerror(Sig: Longint; SigContext: SigContextRec);cdecl;
begin end;  // tsigaction style


var r : sigactionrec;

begin
  r.sa_handler:=signalhandler(@signaltorunerror);
end.
