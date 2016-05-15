{ %opt=-gh }

program outpar;
{$ifdef FPC}{$mode objfpc}{$h+}{$endif}
{$ifdef mswindows}{$apptype console}{$endif}
uses
 {$ifdef FPC}{$ifdef unix}cthreads,{$ifdef darwin}iosxwstr{$else}cwstring{$endif},{$endif}{$endif}
 sysutils;
{$ifndef FPC}
type
 sizeint = integer;
 psizeint = ^sizeint;
{$endif}

procedure testproc(out str);
begin
 ansistring(str):= '';
end;

var
 str1,str2: ansistring;

begin
 setlength(str1,5);
 move('abcde',str1[1],5);
 str2:= str1;
 testproc(str2);
 if psizeint(pchar(pointer(str1))-2*sizeof(sizeint))^ <> 1 then
   Halt(1);
 if str1<>'abcde' then
   Halt(2);  
end.
