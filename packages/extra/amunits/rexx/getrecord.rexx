/* getrecord.rexx

   This is a rexxscript to scan for pascal records.

   I made this one to check my translation of
   cheaders to fpc units. It will write two
   files one pascalfile and one cfile.

   The pascalfile you can almost everytime just
   compile with fpc. In the cfile you have to
   make some changes, just put in a line that
   include the cheader for you testprogram.

   So if you translate a cheader to fpc just
   let this script check it out, if you get
   the same result from both program you have
   probably made the translation correct.

   Usage:

   rx getrecord yourunit.pas

   nils.sjoholm@mailbox.swipnet.se

*/


SIGNAL ON BREAK_C
SIGNAL ON SYNTAX

parse arg name

if name = '' then do
   say 'Input filename to scan for records'
   parse pull name end
   if name = '' then do
   say 'Error no filename'
   exit 20
   end
   end

k = 1

thesource = name

if index(name,'.') > 0 then do
parse var name thesource '.' extension
end

pasname = thesource || 'rec1.pas'
cname = thesource || 'rec2.c'

IF ~Open('textfile',name,'READ') THEN DO
    say 'File not found'
    exit 20
end
else do
  say 'Scanning ' || name
  i = 1
  DO WHILE ~eof('textfile')
     line.i = ReadLn('textfile')
     line.i = Strip(line.i)
     myproc = Word(line.i,3)
     myproc = Upper(myproc)
     IF myproc = "RECORD" THEN DO
        CALL CheckLine(line.i)
        SAY "Doing line :" || i
     END
     i = i +1
  END
  CALL Close('textfile')
  if k > 1 then do
     call writepasfile
     call writecfile
     say 'Done'
  end
  else say 'No records found'
END
EXIT

pasheader:
       writeln('outfile','Program testrecords;')
       writeln('outfile','')
       writeln('outfile','uses exec,' || thesource || ';')
       writeln('outfile','')
       writeln('outfile','begin')
return

writepasfile:
    if ~Open('outfile',pasname,'W') then do
    say 'Can not create file'
    exit 20
    end
    else do
    SAY "Working on " || pasname
    call pasheader
    do j = 1 to k-1
    thename = record.j
    towrite = 'writeln(' || "'" || thename || "',' ':30-length(" || "'" ||thename || "'),"
    towrite = towrite || "':'"
    towrite = towrite || ',sizeof(' || thename || '));'

    writeln('outfile',towrite)
    end j
    writeln('outfile','end.')
    writeln('outfile','')
    CALL Close('outfile')

RETURN

cheader:
    writeln('outfile','');
    writeln('outfile','#include ' || '"stdio.h"')
    writeln('outfile','')
    writeln('outfile','main()')
    writeln('outfile','{')
    return

writecfile:
    if ~Open('outfile',cname,'W') then do
    say 'Can not create file'
    exit 20
    end
    else do
    SAY "Working on " || cname
    call cheader
    do j = 1 to k-1
    thename = record.j
    towrite = 'printf(' || '"%-30s:%d\n","' || thename || '",'
    towrite = towrite || 'sizeof(struct ' || right(thename,length(thename)-1) ||'));'

    writeln('outfile',towrite)
    end j
    writeln('outfile','}')
    writeln('outfile','')

    CALL Close('outfile')
return

CheckLine:
    PARSE ARG theline
    parse var theline thename thesep therecord therest
    if thesep = '=' then do
    thename = strip(thename)
    record.k = thename
    k = k +1
    end
RETURN



BREAK_C:
SYNTAX:
SAY "Sorry, error line" SIGL ":" ErrorText(RC) ":-("
EXIT


