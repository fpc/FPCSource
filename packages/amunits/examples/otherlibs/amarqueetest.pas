Program amarqueetest;

{FPC Pascal test program that uses amarquee.library.  Doesn't do much...}
{just connects and sets a variable, then disconnects.}
{
   Translated from PCQ pascal to FPC Pascal
   25 Aug 2000.

   Update for fpc 1.0.7
   30 Nov 2002.

   nils.sjoholm@mailbox.swipnet.se
}

uses amarquee, utility;

Var
  session : pQSession;
  setOpVal : longint;
  freeSessionVal : longint;
const
  errid : longint = 0;
begin


    {Connect to localhost}
    session := QNewSessionTags('localhost', 2957, 'pascal test',[QSESSION_ERRORCODEPTR,
                                                                @errid,TAG_DONE]);
    if session = nil then begin
      writeln('Could not create connection to localhost/2957');
      writeln('the error was ',QErrorName(errid));
      halt(20);
      end;

    {Set a variable}
    setOpVal := QSetOp(session, 'testVal',pchar('just a test'#0), 12);
    if setOpVal = 0 then begin
      writeln('Warning, QSetOp failed.');
      end;

    {Close the connection}
    freeSessionVal := QFreeSession(session);

end.
