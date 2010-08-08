{ %opt=-gw}
{ %interactive }
{$mode objfpc}

{
1) check that all fields/procedures are shown in the correct visibility section
   when doing "ptype TC"
2) check that "ptype TOBJECT" shows TOBJECT's methods even if the system
   unit is not compiled with debuginfo
}

type
  tc = class
   private
    f: longint;
    procedure priv(a: longint);
   protected
    d: byte;
    procedure prot; virtual;
   public
    c: longint;
    procedure pub;
  end;

procedure tc.priv(a: longint);
begin
end;

procedure tc.prot;
begin
end;

procedure tc.pub;
begin
end;

procedure myproc(a,b,c: longint);
begin
end;


begin
end.

