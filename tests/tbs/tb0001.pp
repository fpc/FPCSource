{ %CPU=i386 }
{ %OPT=-O2  }
{ Old file: tbs0002.pp }
{  tests for the endless bugs in the optimizer          OK 0.9.2 }

unit tb0001;

  interface

  implementation

{$message starting hexstr}
    function hexstr(val : longint;cnt : byte) : string;

      const
         hexval : string[16]=('0123456789ABCDEF');

      var
         s : string;
         l2,i : integer;
         l1 : longInt;

      begin
         s[0]:=char(cnt);
         l1:=longint($f) shl (4*(cnt-1));
         for i:=1 to cnt do
           begin
              l2:=(val and l1) shr (4*(cnt-i));
              l1:=l1 shr 4;
              s[i]:=hexval[l2+1];
           end;
         hexstr:=s;
      end;

{$message starting dump_stack}

    procedure dump_stack(bp : longint);

{$message starting get_next_frame}

      function get_next_frame(bp : longint) : longint;

        begin
           asm
              movl bp,%eax
              movl (%eax),%eax
              movl %eax,__RESULT
           end ['EAX'];
        end;

      procedure dump_frame(addr : longint);

        begin
           { to be used by symify }
           writeln('  0x',HexStr(addr,8));
        end;

{$message starting get_addr}

      function get_addr(BP : longint) : longint;

        begin
           asm
              movl BP,%eax
              movl 4(%eax),%eax
              movl %eax,__RESULT
           end ['EAX'];
        end;

{$message starting main}

      var
         i,prevbp : longint;

      begin
         prevbp:=bp-1;
         i:=0;
         while bp > prevbp do
           begin
              dump_frame(get_addr(bp));
              i:=i+1;
              if i>max_frame_dump then exit;
              prevbp:=bp;
              bp:=get_next_frame(bp);
           end;
      end;

end.
