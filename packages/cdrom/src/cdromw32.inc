uses wincd;


Function ReadCDTOC(Device : String; Var CDTOC : Array of TTocEntry) : Integer;

Var
  Toc :  TToc;
  I,Res : Integer;

begin
 Res:=ReadTOC(Device,TOC);
 If Res<0 then
   Result:=Res
 else
   begin
   If Res>High(CDTOC)+1 then
     Result:=-2
   else
     begin
     Result:=0;
     For I:=TOC.FirstTrack to Toc.LastTrack do
       begin
       With CDTOC[Result],TOC.TocTrack[Result] do
         begin
         min:=Addr[1];
         sec:=Addr[2];
         frame:=Addr[3]; // ?? tocentry.cdte_addr.msf.frame;
         inc(frame,min*60*75);
         inc(frame,sec*75);
         end;
       Inc(Result);
       end;
     // Lead out
     With CDTOC[Result],TOC.TocTrack[Result] do
       begin
       min:=Addr[1];
       sec:=Addr[2];
       frame:=Addr[3]; // ?? tocentry.cdte_addr.msf.frame;
       inc(frame,min*60*75);
       inc(frame,sec*75);
       end;
     end;
   end;
end;


Function GetCDRomDevices(Var Devices : Array of string) : Integer;
begin
  Result:=enumcddrives(Devices);
end;
