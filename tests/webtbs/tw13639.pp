program t2;

{$IFDEF FPC}
  {$mode Delphi}
{$ENDIF}

uses
  SysUtils;

type
  Tb = array of byte;
  int = integer;
  TMeS = class
    private
     FD: Tb;
     Fp: Integer;
    public
     constructor Create(cty: int);
     procedure Write(const Buffer: TB; Offset: int; Count: int); overload;
     procedure Write(Buffer: PAnsiChar; Offset: int; Count: int); overload;
  end;

constructor TMeS.Create(cty: int);
begin
  inherited Create;
  SetLength(FD, cty);
end;

procedure TMeS.Write(Buffer: PAnsiChar; Offset: int; Count: int);
begin
  Move(Buffer[Offset], PAnsiChar(@FD[FP])^, Count);
  Inc(FP, Count);
end;

procedure TMeS.Write(const Buffer: TB; Offset: int; Count: int);
begin
  Write(PAnsiChar(@Buffer[0]), Offset, Count);
end;

var vmes:tmes;

const  vac:string='test1 copy string';
       vtb:string='test2 copy bytes 10';

var
  s: string;
begin
   vmes:=tmes.Create(16);
   vmes.write(Pansichar(vac),1,10);
   vmes.Write(tb(vtb),10,5);
   writeln('"',string(vmes.FD),'"');
   s:=pchar(vmes.fd);
   if (s<>'est1 copy  byte') then
     halt(1);
end.
