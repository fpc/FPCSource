{ Source provided for Free Pascal Bug Report 2772 }
{ Submitted by "Sergey Kosarevsky" on  2003-11-08 }
{ e-mail: netsurfer@au.ru }
Type tWriteFlags=(WF_OVERWRITE,
                  WF_NOOVERWRITE,
                  WF_APPEND);

Type tFileWritingFlags=Set Of tWriteFlags;

Var A:tFileWritingFlags;

Begin
   A:=[WF_OVERWRITE,WF_NOOVERWRITE,WF_APPEND];
   WriteLn(WF_OVERWRITE In A);
End.
