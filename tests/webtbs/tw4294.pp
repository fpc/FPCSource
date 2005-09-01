{ Source provided for Free Pascal Bug Report 4294 }
{ Submitted by "Martin Schreiber" on  2005-08-19 }
{ e-mail:  }
unit tw4294;
interface
{$ifdef fpc}{$mode objfpc}{$endif}
{$H+}

const
 widestrconst = widestring('abc');
 ansistrconst = 'abc';

procedure wtest(const par1: widestring = widestrconst);
procedure atest(const par1: string = ansistrconst);

implementation

procedure atest(const par1: string = ansistrconst); //ok
begin
end;

procedure wtest(const par1: widestring = widestrconst); //error
//procedure wtest(const par1: widestring);              //ok
begin
end;

end.
