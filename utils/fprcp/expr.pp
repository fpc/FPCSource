{$ifdef win32}
{$H-}
{$endif}
{$ifndef fpc}{$N+}{$endif}
Unit Expr;
interface
const
 IntSize2:longbool=false;
PROCEDURE Eval(Formula : String;    { Expression to be evaluated}
               VAR Value   : double;      { Return value }
               VAR ErrPos  : Integer);  { error position }

{
  Simple recursive expression parser based on the TCALC example of TP3.
  Written by Lars Fosdal 1987
  Released to the public domain 1993
}
implementation
type
 real=double;
PROCEDURE Eval(Formula : String;    { Expression to be evaluated}
               VAR Value   : double;      { Return value }
               VAR ErrPos  : Integer);  { error position }
  CONST
    Digit: Set of Char = ['0'..'9'];
  VAR
    Posn  : Integer;   { Current position in Formula}
    CurrChar   : Char;      { character at Posn in Formula }


PROCEDURE ParseNext; { returnerer neste tegn i Formulaen  }
BEGIN
  REPEAT
    Posn:=Posn+1;
    IF Posn<=Length(Formula) THEN CurrChar:=Formula[Posn]
     ELSE CurrChar:=^M;
  UNTIL CurrChar<>' ';
END  { ParseNext };


FUNCTION add_subt: Real;
  VAR
    E   : Real;
    Opr : Char;

  FUNCTION mult_DIV: Real;
    VAR
      S   : Real;
      Opr : Char;

    FUNCTION Power: Real;
      VAR
        T : Real;

      FUNCTION SignedOp: Real;

        FUNCTION UnsignedOp: Real;
          TYPE
            StdFunc = (fabs,    fsqrt, fsqr, fsin, fcos,
                       farctan, fln,   flog, fexp, ffact,
                       fpred,fsucc,fround,ftrunc);
            StdFuncList = ARRAY[StdFunc] of String[6];

          CONST
            StdFuncName: StdFuncList =
            ('ABS','SQRT','SQR','SIN','COS',
            'ARCTAN','LN','LOG','EXP','FACT',
            'PRED','SUCC','ROUND','TRUNC');
          VAR
            L, Start    : Integer;
            Funnet         : Boolean;
            F              : Real;
            Sf             : StdFunc;

              FUNCTION Fact(I: Integer): Real;
              BEGIN
                IF I > 0 THEN BEGIN Fact:=I*Fact(I-1); END
                ELSE Fact:=1;
              END  { Fact };

          BEGIN { FUNCTION UnsignedOp }
            IF CurrChar in Digit THEN
            BEGIN
              Start:=Posn;
              REPEAT ParseNext UNTIL not (CurrChar in Digit);
              IF CurrChar='.' THEN REPEAT ParseNext UNTIL not (CurrChar in Digit);
              IF CurrChar='E' THEN
              BEGIN
                ParseNext;
                REPEAT ParseNext UNTIL not (CurrChar in Digit);
              END;
              Val(Copy(Formula,Start,Posn-Start),F,ErrPos);
            END ELSE
            IF CurrChar='(' THEN
            BEGIN
              ParseNext;
              F:=add_subt;
              IF CurrChar=')' THEN ParseNext ELSE ErrPos:=Posn;
            END ELSE
            BEGIN
              Funnet:=False;
              FOR sf:=fabs TO ftrunc DO
              IF not Funnet THEN
              BEGIN
                l:=Length(StdFuncName[sf]);
                IF Copy(Formula,Posn,l)=StdFuncName[sf] THEN
                BEGIN
                  Posn:=Posn+l-1; ParseNext;
                  f:=UnsignedOp{$ifdef fpc}(){$endif};
                  CASE sf of
                    fabs:     f:=abs(f);
                    fsqrt:    f:=SqrT(f);
                    fsqr:     f:=Sqr(f);
                    fsin:     f:=Sin(f);
                    fcos:     f:=Cos(f);
                    farctan:  f:=ArcTan(f);
                    fln :     f:=LN(f);
                    flog:     f:=LN(f)/LN(10);
                    fexp:     f:=EXP(f);
                    ffact:    f:=fact(Trunc(f));
                    fpred:f:=f-1;
                    fsucc:f:=f+1;
                    fround:f:=round(f)+0.0;
                    ftrunc:f:=trunc(f)+0.0;
                  END;
                  Funnet:=True;
                END;
              END;
              IF not Funnet THEN
              BEGIN
                ErrPos:=Posn;
                f:=0;
              END;
            END;
            UnsignedOp:=F;
          END { UnsignedOp};

        BEGIN { SignedOp }
          IF CurrChar='-' THEN
          BEGIN
            ParseNext; SignedOp:=-UnsignedOp;
          END
          ELSE IF CurrChar='!' THEN
           BEGIN
            ParseNext; SignedOp:=not longint(round(UnsignedOp))+0.0;
           END
          ELSE SignedOp:=UnsignedOp;
        END { SignedOp };

      BEGIN { Power }
        T:=SignedOp;
        WHILE CurrChar='^' DO
        BEGIN
          ParseNext;
          IF t<>0 THEN t:=EXP(LN(abs(t))*SignedOp) ELSE t:=0;
        END;
        Power:=t;
      END { Power };


    BEGIN { mult_DIV }
      s:=Power;
      WHILE CurrChar in ['*','/','&','¬','\','«','¯'] DO
      BEGIN
        Opr:=CurrChar; ParseNext;
        CASE Opr of
          '*': s:=s*Power;
          '/': s:=s/Power;
          '&': s:=longint(round(s)) and longint(round(power))+0.0;
          '¬': s:=longint(round(s)) mod longint(round(power))+0.0;
          '\': s:=trunc(s/Power);
          '«': s:=longint(round(s)) shl longint(round(power))+0.0;
          '¯': s:=longint(round(s)) shr longint(round(power))+0.0;
        END;
      END;
      mult_DIV:=s;
    END { mult_DIV };

  BEGIN { add_subt }
    E:=mult_DIV;
    WHILE CurrChar in ['+','-','|','å'] DO
    BEGIN
      Opr:=CurrChar; ParseNext;
      CASE Opr of
        '+': e:=e+mult_DIV;
        '-': e:=e-mult_DIV;
        '|': e:=longint(round(e))or longint(round(mult_DIV))+0.0;
        'å': e:=longint(round(e))xor longint(round(mult_DIV))+0.0;
      END;
    END;
    add_subt:=E;
  END { add_subt };
procedure Replace(const _from,_to:string);
 var
  p:longint;
 begin
  repeat
   p:=pos(_from,formula);
   if p>0 then
    begin
     delete(formula,p,length(_from));
     insert(_to,formula,p);
    end;
  until p=0;
 end;
function HexToDecS:longbool;
 var
  DecError:longbool;
 procedure Decim(const pattern:string);
  var
   p,b:longint;
   x: Longword;
   ss,st:string;
  begin
   repeat
    p:=pos(pattern,formula);
    if p>0 then
     begin
      b:=p+length(pattern);
      ss:='';
      if b<=length(formula)then
       begin
        while formula[b]in['0'..'9','a'..'f','A'..'F']do
         begin
          ss:=ss+formula[b];
          inc(b);
          if b>length(formula)then
           break;
         end;
        val('$'+ss,x,posn);
        DecError:=posn<>0;
        str(x:0,st);
        delete(formula,p,length(pattern)+length(ss));
        insert(st,formula,p);
       end;
     end;
   until p=0;
  end;
 begin
  DecError:=false;
  Decim('0x');
  if not DecError then
   Decim('$');
  HexToDecS:=not DecError;
 end;

BEGIN {PROC Eval}
  if not HexToDecS then
   begin
    value:=0;
    ErrPos:=Posn;
    exit;
   end;
  IF Formula[1]='.'
  THEN Formula:='0'+Formula;
  IF Formula[1]='+'
  THEN Delete(Formula,1,1);
  FOR Posn:=1 TO Length(Formula)
  DO Formula[Posn] := Upcase(Formula[Posn]);
  replace('<<','«');
  replace('>>','¯');
  replace('^','å');
  replace('**','^');
  replace('DIV','\');
  replace('MOD','¬');
  replace('AND','&');
  replace('XOR','å');
  replace('SHR','¯');
  replace('SHL','«');
  replace('NOT','!');
  replace('OR','|');
  Posn:=0;
  ParseNext;
  Value:=add_subt;
  IF CurrChar=^M THEN ErrPos:=0 ELSE ErrPos:=Posn;
END {PROC Eval};

END.
