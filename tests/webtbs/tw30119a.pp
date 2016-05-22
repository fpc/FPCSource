//{$codepage cp_acp}
//{$codepage cp1250}


program tw30119;
//uses
//  fkdane,
 //  fkrecord;
//uses
// cwstring;
{$H+}


type

    rButikPtr = ^rButik;
  rButik =
    packed record
      data                 : word;      {2}
      dowod                : word;      {4}
      numer                : int64;      {12}
      dataDost             : word;      {14}                                       {w zamowieniu  otwarte 00  }
      memoa,                             {16}   {  1- dot zamowienia                     zamkniete 10
                                                   2- par fisk    --> WNT z importu      zawieszone 01 anulowane 11

                                                   4- paleta                            pilne
                                                   8- (nazwa zestawu) (stare :dot produkcji
                                                   16- numer odbiorcy w tww
                                                   32- zablokowane                       (stare:druga jednostkia cenaP
                                                   64- poprano z produkcji - wyrob       (stere data WZ w twn (norm numer WZ))
                                                   128 : pole memo w tww

                                                }
      vat                  : byte;
      towar,                            {18}
      dostawca,                         {20}
      odbiorca             : word;    {22}
      typ                  : shortint;  {23}
      ilosc,                            {27}
      cenaZ,                            {31}
      cenaS                : single;    {35}
      akwizytor            : byte;      {36}
      wolne1               : word;      {38} {nr dostawy}
      cenaE                : single;    {42}

    end;


const

  ntFakturaIMP    = 2;
  ntKoszt         = 3;
  ntWOC           = 4;
  ntFI            = 5;
  ntPW            = 14;
  ntPrzyjecieTow  = 15;
  ntFakturaExp    = 25;
  ntFakturaSprz   = 26;
  ntKP            = 27;
  ntUslUnia       = 28;
  ntfakturaUSL    = 29;{ *}
  ntPzRWS         = 30;
  ntFakturaSprzVat= 34;
  ntFakturaUslVat = 35;  { faktura do paragonu*}
  ntZamowienieSprz= 36;
  ntDetal         = 55;
  ntRW            = 65;
  ntPrzesun       = 66;
  ntDekompl       = 67;
  ntPZO           = 68;
//  ntWymiana       = 69;{*}
  ntFakturaSprzDew =  69;
  ntReklamacjaVAT = 70;
  ntPZOVat        = 71;
  ntBO            = 77;
  ntZobowImp      = 78;
  ntWZ            = 79;
  ntZwrotPW       = 80;
  ntKpZ           = 81;
  ntKWz           = 82;
  ntPZOEXP        = 83;
  ntFakturaW      = 84;



  ntExpUnia       = 98;
  ntImpUnia       = 99;
  ntImpUniaM      = 100;


  ntPZOUnia       = 103;

var
     tab : array [0..10] of double;


procedure testCaseRaw(const rr : rButik);
var
  kwz: Double;
begin
  kwz:=20;


    case rr.typ of
         ntFakturaSprz,ntFakturaSPrzVat,
         ntPzoExp,ntPzoUnia,
         ntPzoVat,ntPzo,

         ntDetal,ntPzRWS,

         ntExpUnia,ntFakturaEXP:     begin
            tab[1]:=tab[1]+rr.ilosc;
            tab[2]:=tab[2]+kwz;
           end;
         ntRW   : begin
            tab[4]:=tab[4]+kwz;
         end;

         -ntFakturaIMP,
         -ntPrzyjecietow : begin
           tab[3]:=tab[3]+kwz;
         end;
         -ntBO : begin
           tab[5]:=tab[5]+kwz;
         end;

    end;

end;

procedure testCaseA;
var
   r : rButik;
begin
 //  zeruj(r);
   r.ilosc:=1;
   r.cenaZ:=10;
   r.typ:=65;
   testCaseRaw(r);
//   checkEquals(10,tab[4]);
   r.typ:=-ntPrzyjecieTow;
   testCaseRaw(r);
//   checkEquals(10,tab[3]);
    r.typ:=65;
    testCaseRaw(r);
end;



begin
    writeln('start');
  testCaseA;
    writeln('stop') ;
end.
