    function lower(const s : string) : string;
    {
      return lowercased string of s
    }
      var
         i : longint;
      begin
         for i:=1 to length(s) do
          if s[i] in ['A'..'Z'] then
           lower[i]:=char(byte(s[i])+32)
          else
           lower[i]:=s[i];
         lower[0]:=s[0];
      end;

    function upper(const s : string) : string;
    {
      return lowercased string of s
    }
      var
         i : longint;
      begin
         for i:=1 to length(s) do
          if s[i] in ['a'..'z'] then
           upper[i]:=char(byte(s[i])-32)
          else
           upper[i]:=s[i];
         upper[0]:=s[0];
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

    procedure Replace(var s:string;const s1,s2:string;single:boolean);
      var
         last,
         i  : longint;
      begin
        last:=0;
        repeat
          i:=pos(s1,upper(s));
          if i=last then
           i:=0;
          if (i>0) then
           begin
             Delete(s,i,length(s1));
             Insert(s2,s,i);
             last:=i;
           end;
        until single or (i=0);
      end;

    procedure fixreplace(var s:string);
      begin
        replace(s,'P_GDK','PGdk',false);
        replace(s,'= ^T_GDK','= ^TGdk',false);
        replace(s,'^T_GDK','PGdk',false);
        replace(s,'T_GDK','TGdk',false);
        replace(s,'^GDK','PGdk',false);
        replace(s,'EXTERNAL_LIBRARY','gdkdll',false);
      end;


var
  t,f : text;
  ssmall : string[20];
  hs,
  s   : string;
  name : string;
  i    : word;
  func,
  impl : boolean;
begin
  impl:=false;
  assign(t,paramstr(1));
  assign(f,'fixgdk.tmp');
  reset(t);
  rewrite(f);
  writeln(f,'{');
  writeln(f,'}');
  writeln(f,'');
  writeln(f,'{$ifndef gdk_include_files}');
  writeln(f,'  {$define read_interface}');
  writeln(f,'  {$define read_implementation}');
  writeln(f,'{$endif not gdk_include_files}');
  writeln(f,'');
  writeln(f,'{$ifndef gdk_include_files}');
  writeln(f,'');
  writeln(f,'  unit ',Copy(paramstr(1),1,pos('.',paramstr(1))-1),';');
  writeln(f,'  interface');
  writeln(f,'');
  writeln(f,'  uses');
  writeln(f,'    glib,gdkmain,');
  writeln(f,'    gtkobjects;');
  writeln(f,'');
  writeln(f,'  {$ifdef win32}');
  writeln(f,'    const');
  writeln(f,'      gtkdll=''gdk-1.1.dll''; { leave the .dll else .1.1 -> .1 !! }');
  writeln(f,'  {$else}');
  writeln(f,'    const');
  writeln(f,'      gtkdll=''gdk.so'';');
  writeln(f,'    {$linklib c}');
  writeln(f,'  {$endif}');
  writeln(f,'');
  writeln(f,'  Type');
  writeln(f,'    PLongint  = ^Longint;');
  writeln(f,'    PByte     = ^Byte;');
  writeln(f,'    PWord     = ^Word;');
  writeln(f,'    PINteger  = ^Integer;');
  writeln(f,'    PCardinal = ^Cardinal;');
  writeln(f,'    PReal     = ^Real;');
  writeln(f,'    PDouble   = ^Double;');
  writeln(f,'');
  writeln(f,'{$endif not gdk_include_files}');
  writeln(f,'');
  writeln(f,'{$ifdef read_interface}');
  writeln(f,'');
  while not eof(t) do
   begin
     read(t,ssmall);
     fixreplace(ssmall);

     if (not impl) and (copy(trimspace(ssmall),1,14)='implementation') then
      begin
        impl:=true;
        readln(t,s);
        writeln(f,'{$endif read_interface}');
        writeln(f,'');
        writeln(f,'');
        writeln(f,'{$ifndef gdk_include_files}');
        writeln(f,'  implementation');
        writeln(f,'{$endif not gdk_include_files}');
        writeln(f,'');
        writeln(f,'{$ifdef read_implementation}');
        writeln(f,'');
        continue;
      end;
     if (impl) and (copy(trimspace(ssmall),1,4)='end.') then
      begin
        writeln(f,'{$endif read_implementation}');
        writeln(f,'');
        writeln(f,'');
        writeln(f,'{$ifndef gdk_include_files}');
        writeln(f,'end.');
        writeln(f,'{$endif not gdk_include_files}');
        writeln(f,'');
        writeln(f,'{');
  $Log: fixgdk.pp,v $
  Revision 1.3  2005/02/14 17:13:20  peter
    * truncate log

}
