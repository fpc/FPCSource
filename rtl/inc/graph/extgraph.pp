unit extgraph;

  interface

    function readkey : char;
    function keypressed : boolean;
    procedure delay(ms : word);

  var
     directvideo : boolean;

  implementation

    uses
       windows,graph;

    const
       keybuffersize = 16;

    var
       keyboardhandling : TCriticalSection;
       keybuffer : array[1..keybuffersize] of char;
       nextfree,nexttoread : longint;

    procedure inccyclic(var i : longint);

      begin
         inc(i);
         if i>keybuffersize then
           i:=1;
      end;

    procedure addchar(c : char);

      begin
         EnterCriticalSection(keyboardhandling);
         keybuffer[nextfree]:=c;
         inccyclic(nextfree);
         { skip old chars }
         if nexttoread=nextfree then
           inccyclic(nexttoread);
         LeaveCriticalSection(keyboardhandling);
      end;

    function readkey : char;

      begin
         while true do
           begin
              EnterCriticalSection(keyboardhandling);
              if nexttoread<>nextfree then
                begin
                   readkey:=keybuffer[nexttoread];
                   inccyclic(nexttoread);
                   LeaveCriticalSection(keyboardhandling);
                   exit;
                end;
              LeaveCriticalSection(keyboardhandling);
              { give other threads a chance }
              Windows.Sleep(0);
           end;
      end;

    function keypressed : boolean;

      begin
         EnterCriticalSection(keyboardhandling);
         keypressed:=nexttoread<>nextfree;
         LeaveCriticalSection(keyboardhandling);
      end;

    procedure delay(ms : word);

      begin
         Sleep(ms);
      end;

    function msghandler(Window: hwnd; AMessage, WParam,
      LParam: Longint): Longint;

      begin
         case amessage of
           WM_CHAR:
             begin
                addchar(chr(wparam));
                writeln('got char message: ',wparam);
             end;
           WM_KEYDOWN:
             begin

                writeln('got key message');
             end;
         end;
         msghandler:=0;
      end;

    var
       oldexitproc : pointer;

    procedure myexitproc;

      begin
         exitproc:=oldexitproc;
         DeleteCriticalSection(keyboardhandling);
      end;
begin
   charmessagehandler:=@msghandler;
   nextfree:=1;
   nexttoread:=1;
   InitializeCriticalSection(keyboardhandling);
   oldexitproc:=exitproc;
   exitproc:=@myexitproc;
end.
