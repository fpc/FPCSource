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
  assign(f,'fixgdk.tmp');
  reset(t);
  rewrite(f);
  funcname:='';
  gotisfunc:=false;
  impl:=false;
  while not eof(t) do
   begin
     readln(t,s);

     Replace(s,'PROCEDURE','procedure');
     Replace(s,'FUNCTION','function');
     Replace(s,'FUNCTION  ','function ');
     Replace(s,'PPG','PPG');
     Replace(s,'PG','PG');
     Replace(s,'GCHAR','gchar');
     Replace(s,'GUCHAR','guchar');
     Replace(s,'GINT','gint');
     Replace(s,'GUINT','guint');
     Replace(s,'GBOOL','gbool');
     Replace(s,'GSHORT','gshort');
     Replace(s,'GUSHORT','gushort');
     Replace(s,'GLONG','glong');
     Replace(s,'GULONG','gulong');
     Replace(s,'GFLOAT','gfloat');
     Replace(s,'GDOUBLE','gdouble');
     Replace(s,'GPOINTER','gpointer');
     Replace(s,'GCONSTPOINTER','gconstpointer');
     Replace(s,'GDK','Gdk');
     Replace(s,'GDK_','gdk_');
     Replace(s,'GTK','Gtk');
     Replace(s,'GTK_','gtk_');

     ups:=UpperCase(s);

     if Pos('IMPLEMENTATION',ups)>0 then
      impl:=true;

     i:=Pos('PROCEDURE',ups);
     if i>0 then
      if Pos('_PROCEDURE',ups)>0 then
       i:=0;
     if i=0 then
      begin
        i:=Pos('FUNCTION',ups);
        if Pos('_FUNCTION',ups)>0 then
         i:=0;
      end;
     if i<>0 then
      begin
      { Remove Spaces }
        j:=PosIdx('  ',s,i);
        while (j>0) do
         begin
           Delete(s,j,1);
           i:=j-1;
           j:=PosIdx('  ',s,i);
         end;
        ups:=UpperCase(s);
      { Fix Cdecl }
        if (Pos('g_',s)<>0) or
           ((i>2) and (s[i-2] in [':','='])) then
         begin
           j:=Pos('CDECL;',ups);
           if j=0 then
            j:=Length(s)+1
           else
            begin
              k:=Pos('{$IFNDEF WIN32}CDECL;{$ENDIF}',ups);
              if k>0 then
               begin
                 j:=k;
                 k:=29;
               end
              else
               begin
                 k:=Pos('{$IFDEF WIN32}STDCALL;{$ELSE}CDECL;{$ENDIF}',ups);
                 if k>0 then
                  begin
                    j:=k;
                    k:=43;
                  end
                 else
                  k:=6;
               end;
              Delete(s,j,k);
            end;
           Insert('cdecl;',s,j);
         end;
        ups:=UpperCase(s);
      end;

     { Align function with procedure }
     if Copy(s,1,8)='function' then
      Insert(' ',s,9);

     lasts:=s;
     writeln(f,s);
   end;
  close(f);
  close(t);
  erase(t);
  rename(f,fn);
end;

var
 i : integer;
 dir : tsearchrec;
begin
  for i:=1to paramcount do
   begin
     if findfirst(paramstr(i),$20,dir)=0 then
      repeat
        Conv(dir.name);
      until findnext(dir)<>0;
     findclose(dir);
   end;
end.

  $Log: fixexmcdecl.pp,v $
  Revision 1.3  2005/02/14 17:13:20  peter
    * truncate log

}
