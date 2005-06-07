{$mode objfpc}
{$H+}

uses
  sysutils;

Function PosIdx (Const Substr : AnsiString; Const Source : AnsiString;i:longint) : Longint;
var
  S : String;
begin
  PosIdx:=0;
  if Length(SubStr)=0 then
   exit;
  while (i <= length (Source) - length (substr)) do
   begin
     inc (i);
     S:=copy(Source,i,length(Substr));
     if S=SubStr then
      exit(i);
   end;
end;


   function trimspace(const s:string):string;
     var
       i,j : longint;
     begin
       i:=length(s);
       while (i>0) and (s[i] in [#9,' ']) do
        dec(i);
       j:=1;
       while (j<i) and (s[j] in [#9,' ']) do
        inc(j);
       trimspace:=Copy(s,j,i-j+1);
     end;

   function trimbegin(const s:string):string;
     var
       i,j : longint;
     begin
       i:=length(s);
       j:=1;
       while (j<i) and (s[j] in [#9,' ']) do
        inc(j);
       trimbegin:=Copy(s,j,i-j+1);
     end;

    procedure Replace(var s:string;const s1,s2:string);
      var
         last,
         i  : longint;
      begin
        last:=0;
        repeat
          i:=posidx(s1,uppercase(s),last);
          if (i>0) then
           begin
             Delete(s,i,length(s1));
             Insert(s2,s,i);
             last:=i+1;
           end;
        until (i=0);
      end;

procedure Conv(const fn: string);
var
  t,f : text;
  lasts,funcname,
  s,ups : string;
  k,i,j : integer;
  gotisfunc,
  impl : boolean;
begin
  writeln('processing ',fn);
  assign(t,fn);
  assign(f,'fixgtk.tmp');
  reset(t);
  rewrite(f);
  funcname:='';
  gotisfunc:=false;
  impl:=false;
  while not eof(t) do
   begin
     readln(t,s);
   { Remove unit part }
     if s='{$ifndef gtk_include_files}' then
      begin
        while not eof(t) do
         begin
           readln(t,s);
           if Pos('{$ifdef read_interface}',s)>0 then
            begin
              writeln(f,'{****************************************************************************');
              writeln(f,'                                 Interface');
              writeln(f,'****************************************************************************}');
              writeln(f,'');
              writeln(f,s);
              break;
            end;
           if Pos('{$ifdef read_implementation}',s)>0 then
            begin
              writeln(f,'{****************************************************************************');
              writeln(f,'                              Implementation');
              writeln(f,'****************************************************************************}');
              writeln(f,'');
              writeln(f,s);
              impl:=true;
              break;
            end;

  Revision 1.3  2005/02/14 17:13:20  peter
    * truncate log

}
