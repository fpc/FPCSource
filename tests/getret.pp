
{ return the error code of the compiled file }
{ checks also if first line of source contains
  $OPT= command line options needed }
program getret;

 uses dos;

  var com,args : string;
      filename,firstline : string;
      i : byte;	
      ppfile, retfile : text;	
      exefile : file;

begin
  assign(retfile,'retcode');
  rewrite(retfile);
  args:='';
  if paramcount>1 then
    begin
       filename:=paramstr(paramcount);
       if pos('.',filename)=0 then
         filename:=filename+'.pp';
       assign(ppfile,filename);
{$I-}
       reset(ppfile);
       if ioresult=0 then
         begin
{$I+}
            readln(ppfile,firstline);
            if pos('$OPT=',firstline)>0 then
              args:=copy(Firstline,pos('=',Firstline)+1,255);
            if pos('}',args)>0 then
            args:=copy(args,1,pos('}',args)-1);	
            close(ppfile);
         end;
    end;			
  for i:=2 to paramcount do
    args:=args+' '+paramstr(i);
  com:=paramstr(1);
{$ifndef linux}
  if pos('.',com)=0 then
    com:=com+'.exe';
{$endif not linux}

  assign(exefile,com);
{$I-}
  Writeln('testing ',com);
  reset(exefile,1);
  if ioresult<>0 then
    begin
       com:=fsearch(com,getenv('PATH'));
    end
  else
    close(exefile);
{$I+}
  Writeln('Executing "',com,' ',args,'"');
  Flush(output);
  swapvectors;
  exec(com,args);
  swapvectors;
  if doserror<>0 then
    write(retfile,512+doserror)
  else
    write(retfile,dosexitcode);
  close(retfile);
{$ifdef CPU86}
  { reset the FPU to avoid crashing make }
{$asmmode att}
  asm
    fninit
  end;
{$endif CPU86}
end.