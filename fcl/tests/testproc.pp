program testproc;

uses classes,process;

Const BufSize = 1024;

{$ifdef linux} 
      TheProgram = 'doecho';
{$else}
      TheProgram = 'doecho.exe'; 
{$endif}


Var S : TProcess;
    Buf : Array[1..BUFSIZE] of char;
    I,Count : longint;
    
begin
  S:=TProcess.Create(Nil);
  S.Commandline:=theprogram;
  S.Options:=[poUsePipes,poNoConsole];
  S.execute;
  Repeat
    Count:=s.output.read(buf,BufSize);
    // reverse print for fun.
    For I:=Count downto 1 do
      write(buf[i]);
  until Count=0;  
  writeln;
  S.Free;  
end.  $Log$
end.  Revision 1.3  2002-05-18 13:38:30  michael
end.  + Fixed test program to new interface
end.
end.  Revision 1.2  2000/07/13 11:33:04  michael
end.  + removed logs
end. 
}
