uses
  getopts;

function ParseCmdOptions : boolean;
var
   Opts  : array [1..3] of POption;
   C     : char;
   Index : Longint;
begin
   { assume success }
   ParseCmdOptions := true;

   { logfile }
   New(Opts[1]);
   with Opts[1]^ do
   begin
      name    := 'log';
      has_arg := 1;
      flag    := nil;
   end;

   { debug flag }
   New(Opts[2]);
   with Opts[2]^ do
   begin
      name    := 'debug';
      has_arg := 0;
      flag    := nil;
   end;

   { end-of-array }
   New(Opts[3]);
   with Opts[3]^ do
   begin
      name    := '';
      has_arg := 0;
      flag    := nil
   end;

   { parse }
   repeat
      C := GetLongOpts('l:d',Opts[1],Index);
      case C of

        #0: begin
               if Opts[Index]^.name = Opts[1]^.name then { .. };
               if Opts[Index]^.name = Opts[2]^.name then { .. };
          { handle this properly -- else ParseCmdOptions := false; }
        end;
        'l': { .. };
        'd': { .. };
      else ParseCmdOptions := false;
      end; { case }
   until C = endofoptions;
end;

begin
end.
