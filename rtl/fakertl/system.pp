unit system;
interface

{$Y-}

type
  integer=-32768..32767;
  byte=0..255;
  word=0..65535;
  longint=$80000000..$7fffffff;
  pchar=^char;

var
   a,b,c,d : longint;
   s1,s2 : string;
   i1,i2 : int64;

implementation

{ $i ../powerpc/powerpc.inc}

{
procedure p1(l1,l2,l3 : longint);

  begin
  end;


procedure do_exit;[public,alias:'FPC_DO_EXIT'];
begin
end;
}

begin
   b:=4;
   a:=b;
   i1:=i2;
   // p1(a,b,3);
   // s1:=s2;
end.

{
  $Log$
  Revision 1.4  2002-09-07 16:01:17  peter
    * old logs removed and tabs fixed

  Revision 1.3  2002/07/28 20:43:47  florian
    * several fixes for linux/powerpc
    * several fixes to MT

}
