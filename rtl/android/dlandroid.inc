
// On Android the dladdr() function does not return full path to modules.
// Emulate dladdr() by reading the /proc/self/maps to get full path to modules.

threadvar
  _ModuleName: ansistring;

function dladdr(Lib: pointer; info: Pdl_info): Longint; cdecl;
var
  F: Text;
  s, ss, curnode: ansistring;
  a1, a2, curbase: ptruint;
  i: longint;
  p, pp: PAnsiChar;
begin
{$PUSH}
{$I-}
  dladdr:=0;
  _ModuleName:='';
  if info = nil then
    exit;
  curbase:=0;
  curnode:='';
  Assign(F, '/proc/self/maps');
  Reset(F);
  if IoResult <> 0 then
    exit;
  while not Eof(F) do
    begin
      // Read the address range info
      ReadLn(F, ss);
      p:=PAnsiChar(ss);
      // Starting address
      pp:=p;
      while not (p^ in ['-', #0]) do
        Inc(p);
      SetString(s, pp, p - pp);
      Val('$' + s, a1, i);
      if i = 0 then
        begin
          // Ending address
          Inc(p);
          pp:=p;
          while p^ > ' ' do
            Inc(p);
          SetString(s, pp, p - pp);
          Val('$' + s, a2, i);
          if i = 0 then
            begin
              while p^ <= ' ' do Inc(p);  // Whitespace
              while p^ > ' ' do Inc(p);   // Skip perms
              while p^ <= ' ' do Inc(p);  // Whitespace
              while p^ > ' ' do Inc(p);   // Skip offset
              while p^ <= ' ' do Inc(p);  // Whitespace
              while p^ > ' ' do Inc(p);   // Skip dev
              while p^ <= ' ' do Inc(p);  // Whitespace
              // inode
              pp:=p;
              while p^ > ' ' do
                Inc(p);
              SetString(s, pp, p - pp);
              if s <> '0' then
                begin
                  if s <> curnode then
                    begin
                      curnode:=s;
                      curbase:=a1;
                    end;

                  if (ptruint(Lib) >= a1) and (ptruint(Lib) < a2) then
                    begin
                      while p^ <= ' ' do Inc(p);  // Whitespace
                      // File name
                      if p^ = '/' then
                        begin
                          _ModuleName:=p;
                          info^.dli_fname:=PAnsiChar(_ModuleName);
                          info^.dli_fbase:=pointer(curbase);
                          info^.dli_sname:=nil;
                          info^.dli_saddr:=nil;
                          dladdr:=1;
                        end;
                      break;
                    end;
                end;
            end;
        end;
    end;
  Close(F);
{$POP}
end;
