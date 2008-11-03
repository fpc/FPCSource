{ %opt=-Sew -vw }

{ Source provided for Free Pascal Bug Report 4006 }
{ Submitted by "Torsten Kildal" on  2005-05-23 }
{ e-mail: kildal@mx }
program Val_Test;
{$IFDEF FPC}
  {$MODE TP}  {BP/TP compatible}
  {$H-}       {short strings!}
{$ENDIF}

{$R+}

VAR
  s     : string;
  iVar  : smallint;
  lVar  : longint;             { BP7  = Borland Pascal }
  rVar  : real;                { 1010 = FPC 1.0.10     }
  iCode : integer;             { 196  = FPC 1.9.6      }
  wCode : word;                { 200  = FPC 2.0.0      }

begin
  s:='3.14'; {content plays no roll}

  VAL(s,iVar,iCode);
  {BP7 : ok}
  {1010: ok}
  {196 : Warning: Type size mismatch, possible loss of data / range check error}
  {200 : Warning: Type size mismatch, possible loss of data / range check error}

  VAL(s,lVar,iCode);
  {BP7 : ok}
  {1010: ok}
  {196 : ok}
  {200 : ok}

  VAL(s,rVar,iCode);
  {BP7 : ok}
  {1010: ok}
  {196 : Warning: Type size mismatch, possible loss of data / range check error}
  {200 : Warning: Type size mismatch, possible loss of data / range check error}
(*
  VAL(s,iVar,wCode);
  {BP7 : ok}
  {1010: ok}
  {196 : Warning: Type size mismatch, possible loss of data / range check error}
  {200 : Warning: Type size mismatch, possible loss of data / range check error}

  VAL(s,lVar,wCode);
  {BP7 : ok}
  {1010: ok}
  {196 : ok}
  {200 : ok}

  VAL(s,rVar,wCode);
  {BP7 : ok}
  {1010: ok}
  {196 : Warning: Type size mismatch, possible loss of data / range check error}
  {200 : Warning: Type size mismatch, possible loss of data / range check error}
*)
end.
