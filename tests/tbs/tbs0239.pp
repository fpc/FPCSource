{$mode delphi}
  uses 
{$ifdef go32v2}
    dpmiexcp,
{$endif go32v2}
    sysutils;
   type
     ttest=class
     end;
     ttestclass=class of ttest;
   var
     i:ttest;
     tt:tclass;
   begin
     tt:=ttest;
     write(i is tt);
   end.

