// checks type cast of nil in const statement
   type
      THandle = longint;
      WSAEVENT = THandle;
   const
      WSA_INVALID_EVENT = WSAEVENT(nil);

   var
     l : longint;

begin
   l:=WSA_INVALID_EVENT*1;
end.
