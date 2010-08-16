program testmobile;
{$mode objfpc}
{$DEFINE XHTML}
{$DEFINE KONSOLA}
{$IFDEF KONSOLA}
{$APPTYPE CONSOLE}
{$ELSE}
{$APPTYPE GUI}
{$ENDIF}













 const


    _Nazwisko                = 61;
    _Imie                    = 62;
    _adresowanie             = 63;
    _wlasciwosci             = 64;
    _KopiujAdres             = 65;
    _KopiujAdresH            = 66;
    _powodDecyzji            = 67;
    _RodzajZgloszenia        = 68;
    _FirmeZna                = 69;
    _polaDL                  = 70;
    _polaDanych              = 71;
    _podwojonaPozycja        = 72;
    _wprowadzony             = 73;
    _aktualizowany           = 74;
    _ostatniKontakt          = 75;
    _baza                    = 76;
    _selekcja                = 77;
    _przepustka              = 78;
    _odpowiednik             = 79;
    _atrybut                 = 80;
    _bazaKontakt             = 81;
    _rodzajKontakt           = 82;
    _tytul                   = 83;
    _prowadzi                = 84;
    _jezyk                   = 85;
    _temat                   = 86;
    _dostawa                 = 87;
    _koszt                   = 88;
    _platnosc                = 89;
    _waluta                  = 90;
    _cennik                  = 91;
    _status                  = 95;
    _kontrahent              = 96;
    _rozmowca                = 97;
    _warunkiTransakcji       = 98;
    _przebiegRealizacji      = 99;
    _zdecydowalo             = 100;
    _sukces                  = 101; //sukces
    _porazka                 = 102; //pora¾ka
    _jakosc                  = 103; //jako
    _termin                  = 104;
    _terminzak               = 105;
    _stale                   = 106;
    _Bodlozone               = 107; //odo¾one
    _Bzakonczone             = 108; //odo¾one
    _BKoszu                  = 109;
    _bzaniechane             = 110;
    _Telefondomowy           = 111;
    _telefonpraca            = 112;
    _telefonGSM              = 113;
    _pismo                   = 114;
    _fax                     = 115;
    _godzina                 = 116;
    _lp                      = 117;
    _korespondent            = 118;
    _bcykliczne              = 119;
    _parametry               = 120;
    _KHadresowanie           = 121;
    _kraj                    = 123;
    _producent               = 124;
    _nr                      = 125;
    _test                    = 126;
    _testh                   = 127;
    _pracownik               = 128;
    _stanowiskoK             = 129;
    _handlowiec              = 130;
    _kwota                   = 131;
    _Botwarte                = 132; //odo¾one



    _LKontaktRodzaj          = 264;
    _LkontaktSelekcja        = 265;
    _khistoria               = 266;
    _lszablonyPlikow         = 267;
    _lOperatorRodzaj         = 268;
    _kredyt                  = 269;
    _lPromocjaRodzaj         = 270;
    _kzapas                  = 271;
    _kkartoteka              = 272;



    _KontaktRodzaj           = 301;
    _sprawarodzaj            = 302;
    _kontaktPole             = 303;
    _KontaktWiazanie         = 304;
    _KontaktOpis             = 305;
    _kKliknij2               = 306;
    _sprawaCena1             = 307;
    _sprawacena2             = 308;
    _swalutaR                = 309;
    _sjezykR                 = 310;
    _punktualnoscR           = 311;
    _potrzebnyczasR          = 312;
    _sprawaRealizacja        = 313;
    _sprawaOtwarcie          = 314;
    _stypDokumentu           = 315;
    _ssprawaTowary           = 316;
    _sSprawaZalacznik        = 317;
    _sTreerodzaj             = 318;
    _sbazadokumentow         = 319;
    _sstatuszalacznik        = 320;
    _ssprawawykonanie        = 321;
    _kontaktImport           = 322;
    _kancelariaWysylka       = 323;
    _nowyPracownikH          = 324;
    _nowyOddzialH            = 325;
    _nowyOddzial             = 326;
    _nowyPracownik           = 327;
    _nowewiazanie            = 328;
    _kasujWiazanie          = 329;



    _NfakturaVAT             = 401;
    _nRachunekUproszczony    = 402;
    _nFakturaVatKorekta      = 403;
    _nKorektaFaVAt           = 404;
    _nKorektaRU              = 405;


    _ustawKontakt            = 423;
    _ustawTemat              = 424;
    _zaznaczKontakt          = 425;
    _nowykontrahent          = 426;
    _nowynamiar              = 427;
    _zmianauzytkownika       = 428;
    _Oprogramie              = 429;
    _zmienicmagazyn          = 430;
    _anulacjadokumentu       = 431;
    _niewlasciwydokument     = 432;



    _dyskbladzapisu          = 501;
    _dyskzapisaczmiany       = 502;
    _dyskzapisaczmianyw      = 503;
    _ustawianiekonfiguracji  = 504;
    _Btrwadrukowanie         = 505;
    _Bplikzajety             = 506;
    _Bzlezwolniono           = 507;
    _bniezgodneindeksy       = 508;
    _bdostepzarezerwowany    = 509;
    _bpowtorzonynumer        = 510;
    _bbrakkontrahenta        = 511;
    _bzakazzmianytowaru      = 512;
    _Btworzony               = 513;
    _bBrakpamieci            = 514;
    _bbladdysku              = 515;
    _bbladotwarcia           = 516;
    _bwykonajsortowanie      = 517;
    _bTrybczytania           = 518;
    _bloginfailure           = 519;
    _bwyslanieniemozliwe     = 520;
    _bnadpisac               = 521;
    _bbraktwain              = 522;
    _bwszystkiepliki         = 523;
    _Bplikitekstowe          = 524;
    _bplikiAcrobat           = 525;
    _bplikigraficzne         = 526;
    _wybranecechy            = 527;
    _bladwyborupozycji       = 528;
    _wybierzwlasciwapozycje  = 529;
    _bplikiExcel             = 530;
    _bplikiCSV               = 531;
    _bplikiDBF               = 532;
    _bplikixml               = 533;
    _BplikiLOG               = 534;
    _BplikiEML               = 535;
    _bzakazwydruku           = 536;


    _bzakonczonowysylanie    = 537;
    _bwpisznazwecechy        = 538;
    _bniewypelnionepola      = 539;
    _bbladzapisywania        = 540;
    _bdrugakopiadrukowania   = 541;
    _bbrakpliku              = 542;
    _bbladwysylania          = 543;
    _bbrakFunkcji            = 544;
    _bbraknumeru             = 545;
    _bpozycjaZablokowana     = 546;
    _bzakazzmianykontrahenta = 547;
    _bnieprawidlowyVat       = 548;



    _grupowanie              = 636;
    _wgstanowiska            = 637;
    _wg                      = 638;
    _wgakwizytora            = 639;
    _wgregionu               = 640;
    _wgdatysprz              = 641;
    _wgstoiska               = 642;
    _wczytajMail             = 643;
    _dziennikRozmow          = 644;
    _ExportExcel             = 645;
    _ExportWAP               = 646;
//    _listaTematow            = 647;
    _KwgDzialow               = 648;
    _odDnia                  = 649;
    _doDnia                  = 650;
    _kierownik               = 651;
    _dzial                   = 652;
    _rodzajDatyOfe           = 653;

    _wgDaty                  = 654;
    _tylkoPodsumowania       = 655;
    _wPrzygotowaniu          = 656;
    _realizowane             = 657;
    _wydruk                  = 658;
    _zalaczniki              = 659;
    _rozdzielnik             = 660;
    _zapiszJako              = 661;
    _otworz                  = 662;
    _skasuj                  = 663;
    _wsrodZaznaczonych       = 664; //wród zaznaczonych
    _funkcje                 = 665;
    _korespondencjaSeryjna   = 666;
    _zatelefonuj             = 667;
    _wyslijEmail             = 668;
    _wklejDoSchowka          = 669;
    _operacje                = 670;
    _konfiguracja            = 671;
    _sortowanie              = 672;
    _obraz                   = 673;
    _narzedzia               = 674;
    _uzytkownicy             = 675;



    _tMadar                  = 676;
    _TDOS                    = 677;
    _TMAG                    = 678;
    _TCLO                    = 679;
    _czwartaNazwa            = 680;
    _braknazwy               = 681;
    _ScenaSprzedazy          = 682;
    _miara                   = 683;
    _stanMin                 = 684;
    _stawkaVAT               = 685;
    _drugaNazwa              = 686;
    _cenaKat                 = 687;
    _trzeciaNazwa            = 688;
    _tNazwa2                 = 689;

    _dlStrony                = 691;
    _cennik3KOLUMNY          = 692;
    _MIZapiszNaDysk          = 693;
    _ozakupie                = 694;
    _zrodlo                  = 695;
    _firmeznaz               = 696;
    _zdecydowaloA            = 697;
    _rodzajDatyBIS           = 698;
    _zapisaczmiany           = 699;
    _doKosza                 = 701; //do kosza
    _pilne                   = 702;
    _wWolnejChwili           = 703;
    _potwierdzone            = 704;


    //    _aprobata                = 704;
    _rabat                   = 705;
    _upust                   = 706;
    _cewidencyjna            = 707;
    _uprawnienia             = 708;
    _sprawyotwarte           = 709;
    _sprawyzamkn             = 710;
    _zamykanie               = 711;
    _okrespoprz              = 712;
    _okresmies               = 713;
    _okresrok                = 714;
    _okresnast               = 715;
    _kHwizyty                = 716;
    _kHlisty                 = 717;
    _kHtelefony              = 718;
    _grupytowar              = 719;
    _stanMax                 = 721;
    _SwartSprzedazy          = 722;
    _rok                     = 723;
    _zablokowane             = 724;
    _wyrob                   = 725;
    _odpad                   = 726;
    _Kduplikaty              = 727;

    _drugiAdres              = 760;
    _stosujDrugiAdres        = 761;
    _stosujAdresFirmy        = 762;
    _stosujDanePracownikow   = 763;
    _wyksztalcenieszkola     = 764;
    _wyksztalcenietyp        = 765;


    _sOfertaPriorytet    = 768; //'|pilne|w wolnej chwili|aprobata ';
    _odpowiedz               = 769;
    _nieDrukujCeny           = 770;
    _nieDrukujIlosci         = 771;
    _nieDrukujNaOfercie      = 772;
    _nieDrukujNaZapytaniu    = 773;
    _skladnik                = 774;
    _wycena                  = 775;
    _brakSzablonu            = 776;
    _glownyOddzial           = 777;
    _nazwaFirmy              = 778;
    _katalog                 = 779;
    _oddzial                 = 780;
    _zamknijNowy             = 781;
    _nowyetap                = 782;
    _priorytet               = 783;
    _wysylanie               = 784;
    _nieWysylaj              = 785;
    _oferty                  = 786;
    _kartaSwiateczna         = 787;
    _urodziny                = 788;
    _rocznica                = 789;
    _zaopatrzenie            = 790;
    _prowizja                = 791;
    _miejscowosc             = 792;

    _mtWydrukDUP             = 901;
    _mtWydrukDUPH            = 902;

    _mtWydrukZ               = 910;
    _mtWydrukBZ              = 911;
    _mtWydrukWZ              = 912;
    _mtWydrukD               = 913;
    _mtWydrukP               = 914;
    _mtWydrukKZ              = 915;
    _mtWydrukZH              = 920;
    _mtWydrukBZH             = 921;
    _mtWydrukWZH             = 922;
    _mtWydrukDH              = 923;
    _mtWydrukPH              = 924;
    _mtWydrukKZH             = 925;
    _mtWydrukCaption         = 929;
    _mtWydrukKB              = 930;
    _mtWydrukKBH             = 931;

    _ePrint                  = 932;
    _eSave                   = 933;
    _eFile                   = 934;
    _eEdit                   = 935;
    _eInsert                 = 936;
    _eFormat                 = 937;
    _ePaste                  = 938;
    _ePasteSpecial           = 939;
    _eNowy                   = 942;
    _eExit                   = 943;
    _eObiekt                 = 944;
    _eObraz                  = 945;
    _eFont                   = 946;
    _eAkapit                 = 947;
    _eZabezpiecz             = 948;
    _eWylacz                 = 949;
    _eUkryj                  = 950;
    _eCofnij                 = 951;
    _ePonow                  = 952;
    _eWytnij                 = 953;
    _eKopiuj                 = 954;
    _eSzukaj                 = 955;
    _eSzukajDalej            = 956;
    _eZastap                 = 957;
    _eTekstNieZnaleziony     = 958; //Tekst "%s" nie znaleziony.
    _eZmodyfikowany          = 959;
    _KHOpis                  = 960; //edycja opisu
    _KHDodajPlik             = 961; //dodaj plik
    _enazwaPliku             = 962;
    _nadzien                 = 963;
    _EzapiszJako             = 964;
    _Eotworz                 = 965;
    _mtConfirmation          = 966;
    _mtYes                   = 967;
    _mtNo                    = 968;
    _mtCancel                = 969;
    _mtOK                    = 970;
    _mtAsk                   = 971;
    _mtMessage               = 972;
    _podwojonaNazwa          = 973;






    _Filosc                  = 1101;
    _Fcena                   = 1102;
    _Fwartosc                = 1103;
    _Fopis                   = 1104;
    _Fnumer                  = 1105;
    _Fdatadokumentu          = 1106;
    _Fcennik                 = 1107;
    _Fnabywca                = 1108;
    _Fdatasprzedazy          = 1109;
    _Fkurs                   = 1110;
    _FterminP                = 1111;
    _Fdotyczy                = 1112;
    _FKbrakkontrahenta       = 1113;
    _FKbrakpozycji           = 1114;
    _FMdokumenty             = 1115;
    _FMniezaplacone          = 1116;
    _FMstopka                = 1117;
    _FMbazaniezaplaconych    = 1118;
    _FMbazaOpisow            = 1119;
    _FKwczytacdokument       = 1120;
    _FKniedopisywac          = 1121;
    _FKfakturazaplacona      = 1122;
    _FKpozycjenie            = 1123;
    _Fmagazyn                = 1124;
    _FKontrahent             = 1125;
    _Fkonto                  = 1126;


    _Fdostawca               = 1128;
    _Fodbiorca               = 1129;
    _Fdokument               = 1130;
    _Ftyp                    = 1131;
    _FKNumerDokumentu        = 1132;
    _FMfakturycykliczne      = 1133;
    _Fpoletowaru             = 1134;
    _Finfo                   = 1135;
    _Fplatnik                = 1136;
    _Fakwizytor              = 1137;
    _FzmianaNumeru           = 1138;
    _fPoprawaDokumentu       = 1139;
    _FKbrakpozycjiSkasowac   = 1140;
    _FdrukNiemiecki          = 1141;
    _FdrukFrancuski          = 1142;
    _FdrukHiszpanski         = 1143;
    _FterminDOS              = 1144;
    _FterminSprz             = 1145;
    _FdrukPolski             = 1146;
    _FdrukANgielski          = 1147;
    _Fjm                     = 1149;
    _Fazdnia                 = 1150;
    _Fkorektadopozycji       = 1151;
    _Fkorektawinnobyc        = 1152;
    _FKbrakrodzaju           = 1153;
    _Fpoprzedniezaliczki     = 1154;
    _Fdanezamowienia         = 1155;
    _Fwartosczaliczki        = 1156;
    _Fdatarozliczenia        = 1157;
    _fwystawicdo             = 1158;
    _fostatniafaktura        = 1159;
    _fUwagiPoz               = 1160;
    _Fdatawplaty             = 1161;
    _FkodPCN                 = 1162;
    _fKodKraju               = 1163;
    _FMbazaMemo              = 1164;
    _Fkategoria              = 1165;
    _Fwytop                  = 1166;
    _MIcennikpromocyjny      = 1167;
    _Fplanprodukcji          = 1168;
    _FterminWaznosci          = 1169;
    _Fatest                  = 1170;
    _fZamowienia             = 1171;
    _fZaliczki               = 1172;
    _fTransakcje             = 1173;
    _fTransOpen              = 1174;
    _Fdatafaktury            = 1175;
    _fdatazwrotutowaru       = 1176;
    _fwazneod                = 1177;
    _fTypyKasa               = 1178;
    _fOstatniDokument        = 1179;
    _fKsPrzychod             = 1180;
    _fKsRozchod              = 1181;
    _fksKto                  = 1182;
    _fSaldo                  = 1183;
    _fWystawil               = 1184;
    _fOdebral                = 1185;
    _fWplata                 = 1186;


    _oferta                  = 1201;
    _zamowienie              = 1202;
    _faktura                 = 1203;
    _zaplata                 = 1204;
    _wniosek                 = 1205;
    _nota                    = 1206;
    _dekretacja              = 1207;
    _aprobata                = 1208;
    _diagnoza                = 1209;
    _przyjecie               = 1210;
    _naprawa                 = 1211;
    _odebranie               = 1212;
    _zapytanie               = 1213;
    _Rprojekt                = 1214;
    _Rharmonogram            = 1215;
    _Rrealizacja             = 1216;
    _Rodbior                 = 1217;
    _zapotrzebowanie         = 1218;
    _nrMagazynu              = 1219;


    _parametrySprawy         = 1220; //parametry sprawy
    _PzmienicKontakt         = 1221; //'zmieniæ kontakt w etapach'
    _PAktualizowacKontakt    = 1222; //'aktualizowaæ kontakt'
    _pustawFiltr             = 1223;
    _SerwisOpis              = 1224;
    _snieodebrane            = 1225;
    _sniezalatwione          = 1226;
    _srodzajpelny            = 1227;
    _midanesystemu           = 1228;
    _nowanumeracja           = 1229;
    _zgubiononumer           = 1230;
    _Rwykazadresow           = 1231;
    _parametryKontaktu       = 1232; //parametry sprawy
//    _ptresc                  = 1233;
//    _dostawca                = 1233;


    _MIDopisywanie           = 1300;
    _MIAktualizacja          = 1301;
    _MISortowanie            = 1302;
    _MISzukanie              = 1303;
    _MIKasowanie             = 1304;
    _MIKopiowanie            = 1305;
    _MIPodglad               = 1306;
    _MIZamknij               = 1307;
    _MICzytajZDysku          = 1308;
    _MIGenerujSzablon        = 1309;
    _MIEdytujSzablon         = 1310;
    _MIczydopisac            = 1311;
    _Miszukanywyraz          = 1312;
    _MiPozycjaSkasowana      = 1313;
    _miSkasowacrekord        = 1314;
    _MiskasowacRekord2       = 1315;
    _miZakonczyc             = 1316;
    _miskasowac              = 1317;
    _miBaza                  = 1318;
    _mirodzajkonwersji       = 1319;
    _mizakazwtrakcieedycji   = 1320;
    _mizakazwtrakciedopisywania = 1321;
    _miCzyIstniejeNowa       = 1322;
    _miWyrazunieznaleziono   = 1323;



    _sprawa                  = 1405;
    _opis                    = 1406;
    _etapy                   = 1407;
    _przebieg                = 1408;
    _kontaktow               = 1409;
    _kilosc                  = 1410;
    _suma                    = 1411;
    _produkty                = 1412;
    _etap                    = 1413;
    _ilosc                   = 1414;
    _wartosc                 = 1415;
    _razem                   = 1416;
    _scenariusz              = 1417;
    _wykonano                = 1418;
    _tresc                   = 1419;
    _zaniechano              = 1420;
    _oczekuj                 = 1421;
    _dokument                = 1422;
    _notatka                 = 1423;
    _inicjacja               = 1424;
    _harmonogram             = 1425;
    _punktualnosc            = 1426;
    _potrzebnyczas           = 1427;
    _realizacja              = 1428;  //
    _numer                   = 1429;
    _zakupumowa              = 1430;
    _stKontrahenci           = 1431;
    _stPracownicy            = 1432;
    _stnamiary               = 1433;
    _zapamietajzal           = 1434;
    _Dokumentzal             = 1435;
    _Dodnieaktualne          = 1436;
    _Dodzastrzezone          = 1437;
    _wymagane                = 1438;
    _powiazanedokumenty      = 1439;
    _sygnatura               = 1440;
    _przyjal                 = 1441;
    _harmonogramB            = 1442;
    _partia                  = 1443;
    _kaseta                  = 1444;
    _tindex                  = 1445;
    _tNumer                  = 1446;
    _gniazdo                 = 1447;
    _regal                   = 1448;

    _Szablon                 = 1450;
    _zwroty                  = 1451;
    _odzew                   = 1452;
    _zysk                    = 1453;
    _wyslano                 = 1454;
    _kampania                = 1457;
    _data                    = 1458;
    _strona                  = 1459;
    _arkuszDzielonyNa        = 1460;
    _podzialArkusza          = 1461;
    _podzialArkusza2         = 1462;
    _odstepPoziomy           = 1463;
    _odstepPionowy           = 1464;
    _margineslewy            = 1465;
    _kopertaWymiar           = 1466;
    _kopertaAdres            = 1467;
    _kopertaNadawca          = 1468;
    _koperta                 = 1469;
    _kopertaDluga            = 1470;
    _kopertaMargines         = 1471;
    _kopertaPolozenie        = 1472;
    _kopertaPolozenieL       = 1473;

    _brakAdresu              = 1474;
    _zlyAdres                = 1475;
    _bylyBledy               = 1476; //byly bledy : wysylac  strona
    _wyeksportowanych        = 1477;
    _okres                   = 1478;
    _od                      = 1479;
    _do                      = 1480;
    _cechy                   = 1481;
    _PowtorneWysylanie       = 1482;
    _formatExcel             = 1483;
    _poziomy                 = 1484;
    _gorny                   = 1485;


    _psekretarz              = 1500;
    _pPocztawchodzaca        = 1501;
    _ppocztawychodzaca       = 1502;
    _pnadanefaksy            = 1503;
    _pdokumenty              = 1504;
    _ppocztaDziennik         = 1505;
    _pilzal                  = 1506;
    _pkomudano               = 1507;
    _ppodpisane              = 1508;
    _podebrania              = 1509;
    _pzalatwienia            = 1510;
    _pkorespondent           = 1512;
    _poplaty                 = 1513;
    _psumaoplat              = 1514;
    _ppodpisanieodbioru      = 1515;
    _pdruklistdopodpisu      = 1516;
    _listakodow              = 1517;
    _kontynuowac             = 1518;
    _ppomijam                = 1519;
    _ppodpisuje              = 1520;
    _Ppozycja                = 1521;
    _pwyslane                = 1522;
    _pmasa                   = 1524;
    _psumaMas                = 1525;
    _ppotwierdzenieodbioru   = 1526;
    _pnrNadania              = 1527;
    _pzaco                   = 1528;
    _praport                 = 1529;
    _pzalozycnowyraport      = 1530;
    _pzmienicbiezacyraport   = 1531;
    _pdokancelarii           = 1532;
    _poryginal               = 1533;
    _pniewaznyraport         = 1534;

    _rodzaj                  = 1600;
    _przeterminowane         = 1601;
    _odejmij                 = 1602;
    _kategorie               = 1603;
    _dodaj                   = 1604;
    _i                       = 1605;
    _sklep                   = 1606;
    _posiada                 = 1607;
    _adres                   = 1608;
    _kodPocztowy             = 1609;
    _klient                  = 1610;
    _wykonacKonwersjeKamp    = 1611;
    _grupa                   = 1612;
    _kod                     = 1613;
    _operator                = 1614;
    _uwagi                   = 1615;
    _pseudonim               = 1616;
    _zalacznik               = 1618;
    _zastosowanie            = 1619;
    _akt                     = 1620;
    _warunek                 = 1621;
    _zaznaczanie             = 1622;
    _modyfikacja             = 1623;
    _stypremont              = 1632;
    _sstatusremont           = 1633;
    _kankieta                = 1634;
    _nalepkiadresowe         = 1635;
    _zestawienieskrot        = 1636;
    _zestawieniepelne        = 1637;
    _formularze              = 1638;
    _archiwizacja            = 1639;
    _transmisje              = 1640;
    _teczka                  = 1641;
    _kolor                   = 1642;
    _postac                  = 1643;
    _sstatuszamowienie       = 1644;
    _obecnosc                = 1645;
    _pokoj                   = 1646;
    _kompetencje             = 1647;
    _stanowisko              = 1648;
    _daneAdresowe            = 1649;
    _dopiseknafakturze       = 1650;
    _dopiseknaFakturzeL      = 1651;
    _sstatusSprzedaz         = 1652;
    _sstatusDostawa          = 1653;
    _sstatusSerwis           = 1654;
    _Sosoba                  = 1701;
    _sTelefon                = 1702;
    _sEmail                  = 1703;
    _fProdukt                = 1704;
    _Sdatazakupu             = 1705;
    _Szbiorwzorca            = 1706;
    _saktualizowac           = 1707;

    _splanowanytr            = 1709;
    _sedytorwbudowany        = 1710;
    _sIloscKopii             = 1711;
    _sNumerDokumentu         = 1712;
    _sNotatkalista           = 1713;

    _Szakonczenie            = 1715;
    _Srejestrsprzedazy       = 1716;
    _Sanalizasyntetyczna     = 1717;
    _Strescispraw            = 1718;
    _szapisprzebiegusprawy   = 1719;
    _Sutworzycwzorzec        = 1720;
    _Swprowadza              = 1721;
    _Swykona                 = 1722;
    _snastepny               = 1723;
    _sdnioczekiwania         = 1724;
    _spsukcesu               = 1725;
    _spytania                = 1726;
    _sSprawaElement          = 1727;
    _sStatusOferta           = 1728;
    _ssprawaOpis             = 1729;
    _sSprawaObraz            = 1730;
    _sSprawakalendarz        = 1731;
    _Sinformacja             = 1732;
    _RodzajDanych            = 1733;
    _Szerokosc               = 1734;
    _ssprawadziennik         = 1735;
    _Sdotyczy                = 1736;
    _scenazakupu             = 1737;
    _Srejestr                = 1738;
    _sWydrukrejestru         = 1739;
    _plik                    = 1751;
    _niedopisano             = 1752;
    _KImagazyn               = 1753;
    _podgladPozycji          = 1754;
    _brakdanych              = 1755;
    _ROperatopra             = 1758;
    _RDEExel                 = 1759;
    _wyborexport             = 1761;
    _zerowanie               = 1762;
    _tworzony                = 1763;
    _wczytano                = 1764;
    _Tkolejnywolny           = 1765;
    _tWpisac                 = 1766;
    _tOdrzucic               = 1767;
    _edycjapism              = 1768;
    _dziennikWYCH            = 1769;
    _dziennikWCHODZ          = 1770;
    _nadanefaksy             = 1771;
    _podpisanieodb           = 1772;
    _sekwarunek              = 1773;
    _rodzajDatyS             = 1774;  //'sprawy|zamówienia|faktury|zap³aty|dostawy '
    _testujduplikaty         = 1775;

    _czytajPlikDAT           = 1781;
    _czytajPlikDBF           = 1782;
    _czytajPlikCSV           = 1783;
    _czytajPlikEXCEL         = 1784;
    _adresyZOutlooka         = 1785;
    _eksport                 = 1786;
    _kopiujPlikDAT           = 1787;
    _ExportDBF               = 1788;
    _ExportTXT               = 1789;
    _ExportCSV               = 1790;
    _Import                  = 1791;
    _czytajPlikXML           = 1792;

    _Lgroznyblad             = 1801;
    _LgroznybladBIS          = 1802;
    _Lbrakprocedury          = 1803;
    _lniezakonczonyetap      = 1804;
    _lprzepisacetap          = 1805;
    _lbraktrybuzgloszenia    = 1806;
    _lbrakkataloguutworzyc   = 1807;
    _lbladzmianykatalogu     = 1808;
    _lwpisacwykonanie        = 1809;
    _lproszsortowanie        = 1810;
    _lbladinicjalizacji      = 1811;
    _lIndexPodwojony         = 1812;
    _lNIPPodwojony           = 1813;
    _lodbilansowac           = 1814;
    _lwczytacDane            = 1815;
    _lPrzeliczycceny         = 1816;






    //IDENTYFIKATORY

    _Haslo                   = 1900;
    _haslozmiana             = 1901;
    _hasloprzepisz           = 1902;
    _haslozakaz              = 1903;
    _haslozmianaNie          = 1904;
    _haslodostep             = 1905;
    _haslozapis              = 1906;
    _haslopisanie            = 1909;
    _hasloczytanie           = 1911;
    _Haslonazwa              = 1912;
    _haslobiezace            = 1913;
    _haslostare              = 1914;
    _haslofirma              = 1915;
    _haslobraknazwy          = 1916;
    _haslobrakhasla          = 1917;
    _haslozakres             = 1918;
    _hasloPodwojone          = 1919;

{    _haslozmiana             = 1916;
    _haslozmianaH
}
//    _edycjaszablonow          = 1916;
//    _oprogramie              = 1917;
    _HasloIdent              = 1920;
    _Haslouzytkownicy        = 1923;
    _HasloBrak               = 1924;
    _Haslonieznany           = 1925;
    _haslopelna              = 1926;
    _haslohelp               = 1927;
    _HasloKL                 = 1928;
    _haslokasujhelp          = 1929;
    _HasloKasuj              = 1930;
    _haslowejscie            = 1931;
    _hasloUprawnienia        = 1932;
    _hasloMagazynyAll        = 1933;
    _haslozaksiegowane       = 1934;
    _hasloPotwierdzone       = 1935;
    _hasloZakonczone         = 1936;
    _haslowyjscie            = 1937;
    _hasloemail              = 1938;

    _hasloprzerwa            = 1939;
    _hasloOdbezpiecz         = 1940;
    _hasloZrezygnuj          = 1941;
    _haslobrakuprawnien      = 1942;
    _hasloprofilzmiana       = 1943;
    _hasloWydrukiMagazyn     = 1944;
    _hasloPodgladNiezaplacone= 1945;
    _hasloPonownyWydruk      = 1946;
    _hsDostep                = 1947;
    _hasloZdalnyDostep       = 1948;
    _hasloPowiekszonelitery  = 1949;
    _haslorekorduzywany      = 1950;
    _haslozakresKarty        = 1951;
    _hsMagazyn               = 1952;
    _hasloWydrukizamowienia  = 1953;





    _Tsprzedaz               = 2001;
    _Tmiara                  = 2002;
    _Tzakup                  = 2003;
    _tMinimum                = 2004;
    _topakowanie             = 2005;
    _tKatalog                = 2006;
    _tKurs                   = 2007;
    _tMarza                  = 2008;
    _tNetto                  = 2009;
    _tBrutto                 = 2010;
    _tVAT                    = 2011;
    _tMaximum                = 2012;
    _brutto                  = 2013;
    _tDewizy                 = 2014;
    _tWycena                 = 2015;
    _tMasa                   = 2016;
    _netto                   = 2017;
    _vat                     = 2018;
    _tMasaNT                 = 2019;
    _tMasaBR                 = 2020;
    _toznaczenie             = 2021;
    _tpaczka                 = 2022;
    _nip                     = 2023;
    _wyslanyprzelew          = 2024;
    _jednostka               = 2025;
    _ustawienia              = 2026;
    _odwrotnie               = 2027;
    _zprzeniesienia          = 2028;

{    _tDostawca               = 2022;
    _tOdbiorca               = 2023;
}
    _KHStart                 = 2201;
    _kSzukaj                 = 2202;
    _khSzukaj                = 2203;
    _Kedycja                 = 2204;
    _KHEdycja                = 2205;
    _Kpodglad                = 2206;
    _KHPodglad               = 2207;
    _Kdopisz                 = 2208;
    _KHDopisz                = 2209;
    _kDopiszKon              = 2210;
    _Kpowrot                 = 2211;
    _KHpowrot                = 2212;
    _Kwyjscie                = 2213;
    _khWyjscie               = 2214;
    _Kdrukowanie             = 2215;
    _khDrukowanie            = 2216;
    _kodswiez                = 2217;
    _khOdswiez               = 2218;
//    _kbaza                   = 2219;
    _Klistazlecen            = 2219;
    _Kzestawienia            = 2220;
    _KHlistakontaktow        = 2221;
    _KHlistaspraw            = 2222;
    _Klistakontaktow         = 2223;
    _Klistaspraw             = 2224;
    _KHscenariusze           = 2225;
    _Kscenariusze            = 2226;
    _KHlistaProduktow        = 2227;
    _KlistaProduktow         = 2228;
    _Klistakampanii          = 2230;
    _KHlistakampanii         = 2231;
    _klistaAtrybutow         = 2232;
    _Kodpowiedz              = 2233;
    _khodpowiedz             = 2234;
    _kprzeslij               = 2235;
    _khprzeslij              = 2236;
    _kaprobata               = 2237;
    _khaprobata              = 2238;
    _Kzatwierdz              = 2239;
    _KHzatwierdz             = 2240;
    _Kporzuc                 = 2241;
    _KHporzuc                = 2242;
    _KStart                  = 2243;
    _khpodgladwydruku        = 2244;
    _khwyslijdo              = 2245;
    _kwyslijdo               = 2246;
    _klistaKontrahentow      = 2247;
    _khListakontrahentow     = 2248;
    _kwszystko               = 2249;
    _khwszystko              = 2250;
    _Kzeruj                  = 2251;
    _KHzeruj                 = 2252;
    _Kpoprawa                = 2253;
    _KHPoprawa               = 2254;
    _Kzmiana                 = 2255;
    _kdomyslnie              = 2256;
    _kHdomyslnie             = 2257;
    _KHfakturowanie          = 2258;
    _khistoriaW              = 2259;
    _khhistoriaW             = 2260;
    _Kmenu                   = 2261;
    _KHmenu                  = 2262;
    _kwyslijKancelaria       = 2263;


    _Kwydruki                = 2265;
    _slownie                 = 2266;
    _Ksetup                  = 2267;
    _KHsetup                 = 2268;
//    _KHwydruki               = 2266;


    _pWYKAZPOZYCJINIEZAPLACONYCH = 2299;
    _pstrona                 = 2301;
    _pwydrukowano            = 2302;
    _pkoniecwydruku          = 2303;
    _piloscStron             = 2304;
    _pZaokresoddnia          = 2305;
    _pdodnia                 = 2306;


    _Kzaznacz                = 2341;
    _kHzaznacz               = 2342;
    _kOdznacz                = 2343;
    _KHodznacz               = 2344;
    _Kselekcja               = 2345;
    _KHselekcja              = 2346;
    _kModyfikuj              = 2347;
    _khModyfikuj             = 2348;
    _kSchowek                = 2349;
    _khschowek               = 2350;
    _kKopiuj                 = 2351;
    _kWklej                  = 2352;
    _kWytnij                 = 2353;
    _Kkopiowanie             = 2354;
    _kHemail                 = 2355;
    _khKopiowanie            = 2356;
    _Kwykonaj                = 2357;
    _kznalaz                 = 2358;
    _khZnalaz                = 2359;
    _kszukajdalej            = 2360;
    _khszukajDalej           = 2361;
    _kszukajESC              = 2362;
    _khSzukajESC             = 2363;
    _Kzakonczonozapis        = 2364;
    _Kzakonczone             = 2365;



    _kZapisz                 = 2366;
    _kUwagi                  = 2367;
    _kHuwagi                 = 2368;
    _KHlistaProduktow1       = 2369;
    _kHdziennik              = 2370;
    _kFiltr                  = 2371;
    _kHFiltr                 = 2372;

    _KKontakty               = 2373;
    _Ksprawy                 = 2374;
    _KdPodobny               = 2375;
    _KHdpodobny              = 2376;
    _Kwypelnij               = 2377;
    _KHwypelnij              = 2378;
    _Kagregacja              = 2379;
    _KHagregacja             = 2380;
    _Kadministracja          = 2381;
    _Kzarzadzanie            = 2382;
    _KListaoperatorow        = 2383;
    _kZalacznik              = 2384;
    _KHzalacznik             = 2385;
    _kEdytor                 = 2386;
    _kHEdytor                = 2387;
    _kzadania                = 2388;

    _kkalendarz              = 2390;
    _kHkalendarz             = 2391;
    _kHTowary                = 2392;
    _knowy                   = 2393;
    _kOperacje               = 2394;
    _kAKta                   = 2395;
    _khakta                  = 2396;
    _Kdolaczwczytaj          = 2397;
    _KWyslijdrukuj           = 2398;
    _kanalizy                = 2399;
    _khanalizy               = 2400;
    _kwykresy                = 2401;
    _khwykresy               = 2402;
    _kszablon                = 2403;
    _khszablon               = 2404;
    _kwgszablonu             = 2405;
    _khwgszablonu            = 2406;
    _Kemail                  = 2407;
    _kinstrukcja             = 2408;
    _khinstrukcja            = 2409;
    _krocznik                = 2410;
    _khrocznik               = 2411;
    _kkarta                  = 2412;

    _KlistaTematow           = 2501;
    _KHlistaTematow          = 2502;
    _kPlanKont               = 2503;
    _khplankont              = 2504;
    _KPopraw                 = 2505;
    _Kskanuj                 = 2506;
    _KHSkanuj                = 2507;



    _headZapas               = 2601;
    _headKartotekaMag        = 2602;
    _headTowary              = 2603;
    _headPracownik           = 2604;
    _headNiezaplacone        = 2605;
    _headTowaryPaleta        = 2606;
    _headKontaktyZalezne     = 2607;


    _rstanpoczatkowy         = 2701;
    _rzdnia                  = 2702;
    _rWn                     = 2703;
    _rMa                     = 2704;
    _rnrKonta                = 2705;

    _Fzestawienieobrotow     = 2801;
    _fZestawienieMagazynowe  = 2802;






function pl(nn : integer):ansistring;
begin
  result := '';
  case nn of
    _Ppozycja                : result := 'pozycja';
    _pzaco                   : result := 'za co ';
    _praport                 : result := 'raport';
    _pzalozycnowyraport      : result := 'za³o¿yæ nowy raport ?';
    _pzmienicbiezacyraport   : result := 'zmieniæ bie¿¹cy raport ?';
    _pdokancelarii           : result := 'do kancelarii';
    _poryginal               : result := 'orygina³';
    _pniewaznyraport         : result := 'nie wa¿ny raport';
    _Nazwisko                : Result := 'nazwisko';
    _Imie                    : Result := 'imiê';
    _adresowanie             : Result := 'adresowanie';
    _KHadresowanie             : Result := 'drukowanie kopert';
    _wlasciwosci             : Result := 'w³aciwoci';
    _KopiujAdres             : Result := 'wstaw adres firmy';
    _KopiujAdresH            : Result := 'kopiuj adres z danych firmy';
    _powodDecyzji            : Result := '|cena'+
                                         '|wzornictwo'+
                                         '|jakoæ materia³ów'+
                                         '|jakoæ wykonania'+
                                         '|marka produktu'+
                                         '|dostêpnoæ'+
                                         '|brak innych'+
                                         '|inne'+
                                         '|wszystko'+
                                         '|wszystko oprócz ceny'+
                                         '|warunki gwarancji'+
                                         '|dopasowanie'+
                                         '|dowiadczenie';

    _RodzajZgloszenia        : Result := '|karta zg³oszenia'+
                                         '|Ankieta WWW'+
                                         '|katalog'+
                                         '|targi'+
                                         '|Strona WWW'+
                                         '|inne';

    _FirmeZna                : Result := '|prasa'+
                                         '|radio'+
                                         '|telewizja'+
                                         '|od sprzedawcy'+
                                         '|od znajomych'+
                                         '|z wystawy sklepowej'+
                                         '|nie zna'+
                                         '|Internet'+
                                         '|ksiazka telefoniczna'+
                                         '|ulotka'+
                                         '|bilboard'+
                                         '|targi'+
                                         '|widziane wyroby';

    _polaDl                  : result := 'pola tekstowe d³ugie';
    _polaDanych              : result := 'pola danych';
    _podwojonaPozycja        : result := 'Podwojona pozycja';
    _podwojonaNazwa          : result := 'Podwojona nazwa ';
    _wprowadzony             : result := 'wprowadzony';
    _aktualizowany           : result := 'aktualizowany';
    _ostatniKontakt          : result := 'ostatni Kontakt';
    _baza                    : result := 'baza';
    _selekcja                : result := 'selekcja';
    _przepustka              : result := 'przepustka';
    _odpowiednik             : result := 'odpowiednik';
    _atrybut                 : result := 'atrybut';
    _bazaKontakt             : result := 'baza';
    _rodzajkontakt           : result := 'rodzaj';
    _tytul                   : result := 'tytu³';
    _wyksztalcenietyp        : result := '|podstawowe|srednie|wyzsze|zawodowe|student|gimnazjum';
    _wyksztalcenieSzkola     : result := '|podstawowa|gimnazjum|3L zawodowa|5L rednia zawodowa-technikum|4L rednia ogólnokszta³c¹ca|6L policealna|7L licencjat|8L wy¿sza';


    _prowadzi                : result := 'prowadzi';
    _jezyk                   : result := 'jêzyk';
    _temat                   : result := 'temat';
    _dostawa                 : result := 'dostawa';
    _koszt                   : result := 'koszt';
    _platnosc                : result := 'p³atnoæ';
    _waluta                  : result := 'waluta';
    _cennik                  : result := 'cennik';
    _status                  : result := 'status';
    _kontrahent              : result := 'kontrahent';
    _pracownik               : result := 'pracownik';
    _stanowiskoK             : result := 'stanowisko';
    _handlowiec              : result := 'handlowiec';

    _rozmowca                : result := 'rozmówca';
    _warunkiTransakcji       : result := '--- warunki transakcji ---';
    _przebiegRealizacji      : result := '--- przebieg realizacji ---';
    _zdecydowalo             : result := 'zdecydowa³o :';
    _sukces                  : result := 'sukces';
    _porazka                 : result := 'pora¿ka';
    _jakosc                  : result := 'jakoæ';
//    _cena                    : result := 'cena';
    _kwota                   : result := 'kwota';
    _termin                  : result := 'termin';
    _terminzak               : result := 'termin zakoäczenia';
    _stale                   : result := 'sta³e';
    _Bodlozone               : result := 'od³o¿one ';
    _BKoszu                  : result := 'w koszu';
    _Bcykliczne              : result := 'cykliczne ';
    _bzaniechane             : result := 'zaniechane   ';
    _Bzakonczone             : result := 'zakoñczone   ';
    _Botwarte                : result := 'otwarte      ';
    _Kzakonczone             : result := 'zakoñczone';
    _pWYKAZPOZYCJINIEZAPLACONYCH : result :='WYKAZ POZYCJI NIEZAPACONYCH ';
    _pstrona                 : result := '    strona   ';

    _pwydrukowano            : result := ' wydrukowano ';
    _pzaOkresOddnia          : result := ' za okres od dnia ';
    _pdodnia                 : result := ' do dnia ';
    _pkoniecwydruku          : result := 'koniec wydruku  ';
    _piloscstron             : result := 'iloæ stron';

    _Kwykonaj                : result := 'wykonaj :';
    _kszukajdalej            : result := ' SPACJA: szukaj dalej ';
    _khszukajDalej           : result := ' SPACJA ';
    _kszukajESC              : result := ' ESC: przerwij   ';
    _khSzukajESC             : result := ' ESC';
    _Kzakonczonozapis        : result := 'zakoñczono zapis ';


    _oferta                  : result := 'oferta';
    _zamowienie              : result := 'zamówienie';
    _faktura                 : result := 'faktura';
    _zaplata                 : result := 'zap³ata';
    _wniosek                 : result := 'wniosek';
    _nota                    : result := 'nota';
    _dekretacja              : result := 'dekretacja';
    _aprobata                : result := 'aprobata';
    _diagnoza                : result := 'diagnoza';
    _przyjecie               : result := 'przyjêcie';
    _naprawa                 : result := 'naprawa';
    _odebranie               : result := 'odebranie';
    _zapytanie               : result := 'zapytanie';
    _Rprojekt                : result := 'projekt';
    _zapotrzebowanie         : result := 'zapotrzebowanie';
    _nrMagazynu              : result := 'podaj nr magazynu';
    _Rwykazadresow           : result := 'wykaz adresów';
    _Rharmonogram            : result := 'harmonogram';
    _Rrealizacja             : result := 'realizacja';
    _Rodbior                 : result := 'odbiór';



    _parametrySprawy         : result := 'parametry sprawy';
    _parametryKontaktu       : result := 'parametry kontaktu';
    _PzmienicKontakt         : result := 'Zmieniæ kontakt w etapach?';
    _PAktualizowacKontakt    : result := 'Aktualizowaæ kontakt?';
    _pustawfiltr             : result := 'Ustaw filtr :';

    _Filosc                  : result := 'iloæ';
    _Fcena                   : result := 'cena';
    _Fwartosc                : result := 'wartoæ';
    _Fopis                   : result := 'nazwa  (opis)';
    _Fnumer                  : result := 'numer';
    _Fdatadokumentu          : result := 'data dokumentu';
    _fDataFaktury            : result := 'data faktury';
    _fDataZwrotuTowaru       : result := 'data zwrotu towaru';
    _fwazneOd                : result := 'wa¿ne od';
    _fTypyKasa               : result := ' |Naleznosci z magazynu|Zobowiazania z magazynu|Wpaty|Wypaty'
          +'|zapata z rejestru VAT|umowy zlecenia|Zobowiazania z importu|nowy dokument VAT|Wpaty zaliczek ';
    _Fcennik                 : result := 'cennik';
    _Fnabywca                : result := 'nabywca';
    _Fdatasprzedazy          : result := 'data sprzeda¿y';
    _Fdatawplaty             : result := 'data wp³aty';
    _Fkurs                   : result := 'kurs';
    _FterminP                : result := 'termin p³atnoci';
    _FterminDOS              : result := 'termin dostawy';
    _FterminSprz             : result := 'termin sprzeda¿y';
    _Fdotyczy                : result := 'dotyczy :';
    _FKbrakkontrahenta       : result := 'brak kontrahenta';
    _FKBrakrodzaju           : result := 'brak rodzaju';
    _Fpoprzedniezaliczki     : result := 'poprzednie zaliczki';
    _Fdanezamowienia         : result := 'dane zamówienia lub umowy';
    _Fwartosczaliczki        : result := 'wartoæ zaliczki';
    _Fdatarozliczenia        : result := 'data rozliczenia';
    _fwystawicdo             : result := 'wystawiæ do';
    _fostatniafaktura        : result := 'ostatnia faktura';
    _fOstatniDokument        : result := 'ostatni dokument';
    _fKsPrzychod             : result := ' PRZYCHÓD   ';
    _fKsRozchod              : result := '  ROZCHOD   ';
    _fkskto                  : result := 'kto:';
    _fSaldo                  : result := ' SALDO ';
    _fWystawil               : result := 'wystawi³';
    _fOdebral                : result := 'odebra³';
    _fWplata                 : result := 'wp³ata ';

    _FKbrakpozycji           : result := 'brak pozycji zapisaæ?';
    _FKbrakpozycjiSkasowac   : result := 'brak pozycji skasowaæ?';
    _Fdrukniemiecki          : result := 'druk niemiecki';
    _FdrukFrancuski          : result := 'druk francuski';
    _FdrukHiszpanski         : result := 'druk hiszpañski';
    _FdrukAngielski          : result := 'druk angielski';
    _FdrukPolski             : result := 'druk polski';
    _FMdokumenty             : result := 'dokumenty';
    _FMniezaplacone          : result := 'niezap³acone';
    _FMstopka                : result := 'stopka';
    _FMbazaniezaplaconych    : result := 'baza niezap³aconych';
    _FMbazaOpisow            : result := 'baza opisów';
    _FMbazaMemo              : result := 'baza memo';
    _FMfakturycykliczne      : result := 'faktury cykliczne';
    _FkodPCN                 : result := 'kod PCN';
    _FkodKraju               : result := 'kod kraju';
    _Fjm                     : result := 'j.m.';
    _Fazdnia                 : result := 'fa z dnia';
    _Fkorektadopozycji       : result := 'korekta do pozycji';
    _Fkorektawinnobyc        : result := 'korekta winno byæ';
    _MIcennikPromocyjny      : result := 'cennik promocyjny';


    _FKwczytacdokument       : result := 'wczytaæ dokument ';
    _FKnumerDokumentu        : result := 'numer dokumentu ';
    _FKpozycjenie            : result := 'pozycje nie do odtworzenia';
    _FKniedopisywac          : result := 'nie dopisywaæ';
    _FKfakturazaplacona      : result := 'UWAGA - faktura zap³acona';
    _Fmagazyn                : result := 'magazyn';
    _Fkontrahent             : result := 'kontrahent';
    _Fkonto                  : result := 'konto';
    _Fzestawienieobrotow     : result := 'zestawienie obrotów';
    _fZestawienieMagazynowe  : result := 'zestawienie magazynowe';
    _Fdostawca               : result := 'dostawca';
    _Fodbiorca               : result := 'odbiorca';
    _Fplatnik                : result := 'p³atnik';
    _Fakwizytor              : result := 'akwizytor';
    _FKategoria              : result := 'kategoria';

    _Fdokument               : result := 'dokument';
    _Ftyp                    : result := 'typ';
    _grupytowar              : result := 'grupy towarowe';
    _Fpoletowaru             : result := 'index|pole DOS|pole MAG|pole PKWIU|atrybut 1|atrybut 2| ';
    _Finfo                   : result := ' Info :';
    _FzmianaNumeru           : result := '&zmiana numeru';
    _fpoprawadokumentu       : result := '&poprawa dokumentu';
    _fUwagiPoz               : result := 'UWAGI poz';
    _wyrob                   : result := 'wyrób ';
    _odpad                   : result := 'odpad ';
    _fWytop                  : result := 'wytop ';
    _fAtest                  : result := 'atest ';
    _fPlanProdukcji          : result := 'plan produkcji';
    _fTerminwaznosci         : result := 'termin wa¿noci';
    _fZamowienia             : result := 'zamówienia';
    _fZaliczki               : result := 'zaliczki';
    _fTransakcje             : result := 'transakcje';
    _fTransOpen              : result := 'otwarte ';


    _sprawa                  : result := 'sprawa ';
    _opis                    : result := ' opis      ';
    _przebieg                : result := 'przebieg';
    _kontaktow               : result := 'kontaktów';
    _Kilosc                  : result := 'iloæ';
    _suma                    : result := 'suma';
    _produkty                : result := 'produkty';
    _etap                    : result := 'etap';
    _etapy                   : result := 'etapy';
    _ilosc                   : result := 'iloæ';
    _wartosc                 : result := 'wartoæ';
    _razem                   : result := 'razem';
    _scenariusz              : result := 'procedura';             ///
    _wykonano                : result := 'wykonano                ';
    _tresc                   : result := 'treæ';
//    _ptresc                  : result := 'treæ';
    _zaniechano              : result := 'zaniechano     ';
    _oczekuj                 : result := 'oczekuj';
    _dokument                : result := 'dokument';
    _notatka                 : result := 'notatka ';
    _inicjacja               : result := '---- inicjacja     ----                          wprowadzi³';
    _realizacja              : result := '---- realizacja    ---- ';
    _harmonogram             : result := '---- do wykonania  -------';
    _harmonogramB            : result := '     zadanie                                     dla                          termin';
    _przyjal                 : result := '     przyj¹³/wykona³ : ';
    _powiazanedokumenty      : result := ' powi¹zane dokumenty';
    _sygnatura               : result := 'sygnatura';
    _punktualnosc            : result := 'punktualnoæ';
    _potrzebnyczas           : result := 'potrzebny czas';
    _numer                   : result := 'numer';
    _partia                  : result := 'partia';
    _kaseta                  : result := 'kaseta';
    _regal                   : result := 'rega³';
    _gniazdo                 : result := 'gniazdo';
    _tIndex                  : result := 'index';
    _tnumer                  : result := 'numer';
    _zakupumowa              : result := 'zakup/umowa';
    _stKontrahenci           : result := 'kontrahenci';
    _stPracownicy            : result := 'pracownicy';
    _stNamiary               : result := 'namiary';
    _zapamietajZal           : result := 'zapamietaj w za³¹czniku';
    _dokumentZal             : result := 'dokument jako za³¹cznik';
    _szablon                 : result := 'szablon';
    _zwroty                  : result := 'zwroty';
    _odzew                   : result := 'odzew';
    _zysk                    : result := 'zysk';
    _wyslano                 : result := 'wys³ano';
    _kampania                : result := 'kampania';
    _data                    : result := 'data';
    _strona                  : result := 'strona';
    _arkuszDzielonyNa        : result := 'arkusz dzielony na';
    _podzialArkusza          : result := 'dowolny|3*7|3*8|2*8|3*9|4*11|4*10 ';
    _podzialArkusza2         : result := 'dowolny|4*2|6*3|';
    _odstepPionowy           : result := 'rozstaw pionowy';
    _odstepPoziomy           : result := 'rozstaw poziomy';
    _margineslewy            : result := 'margines lewy';
    _kopertaMargines         : result := 'margines ';
    _kopertaWymiar           : result := '|ma³a : C6 (114 x 162 mm)|d³uga :DL (110 x 220 mm)|rednia : B5 - (176 x 250 mm)|C5  (162 x 229) |paczka|metka|pismo';
    _kopertaPolozenieL       : result := 'lewy rodek|lewy brzeg|prawy rodek|prawy brzeg';
    _kopertaPolozenie        : result :=  'po³o¿enie ';
    _kopertaAdres            : result := 'domylny|pierwszy |drugi|pracownika';
    _koperta                 : result := 'koperta';
    _kopertaDluga            : result := 'd³uga koperta';
    _kopertaNadawca          : result := 'nadawca';
    _poziomy                 : result := 'poziomy';
    _gorny                   : result := 'górny';
    _Kemail                  : result := 'e-mail     ';
    _KKopiowanie             : result := 'kopiowanie ';
    _KHemail                 : result := 'wys³anie e-mail';
    _khKopiowanie            : result := 'kopiowanie na dysk';
    _PowtorneWysylanie       : result := 'Wys³aæ powtórnie?';
    _formatExcel             : result := 'format EXCEL';
    _brakAdresu              : result := 'brak adresu';
    _zlyAdres                : result := 'z³y adres';
    _bylyBledy               : result := 'byly bledy : wysylac  strona';
    _wyeksportowanych        : result := 'wyeksportowanych ';
    _okres                   : result := 'okres ';
    _od                      : result := 'od';
    _do                      : result := 'do';
    _cechy                   : result := 'cechy';
    _rodzaj                  : result := 'rodzaj';
    _postac                  : result := 'postaæ';
    _pokoj                   : result := 'pokój';
    _stanowisko              : result := 'stanowisko';
    _kompetencje             : result := 'kompetencje';
    _obecnosc                : result := 'obecnoæ';
    _przeterminowane         : result := 'przeterminowane';
    _odejmij                 : result := 'odejmij';
    _kategorie               : result := 'kategorie';
    _dodaj                   : result := ' dodaj     ';
    _i                       : result := 'i';
    _sklep                   : result := 'sklep';
    _posiada                 : result := 'posiada';
    _adres                   : result := 'adres';
    _kodPocztowy             : result := 'kod pocztowy';
    _klient                  : result := 'klient';
    _wykonacKonwersjeKamp    : result := 'Wykonaæ konwersjê kampanii?';
    _grupa                   : result := 'grupa' ;
    _kod                     : result := 'kod';
    _operator                : result := 'operator';
    _uwagi                   : result := 'uwagi';
    _pseudonim               : result := 'login';
    _zalacznik               : result := 'za³¹cznik';
    _zastosowanie            : result := 'zastosowanie';
    _akt                     : result := 'akt';
    _warunek                 : result := 'warunek';
    _zaznaczanie             : result := 'zaznaczanie';
    _modyfikacja             : result := 'modyfikacja';
//    _oznaczenie              : result := 'oznaczenie';
    _stypremont              : result := '|proces-zagadnienie|dzia³-miejsce|zadanie-kontrakt';
    _sstatusremont           : result := '|w przygotowaniu|zakoñczone|od³o¿one |realizowane|anulowane|projektowanie  ';
    _sstatuszamowienie       : result := 'realizowane|od³o¿one |zamkniête|anulowane ';
    _sstatusSprzedaz         : result := 'realizowane|wstrzymane |dostarczone|do reklamacji ';
    _sstatusDostawa          : result := 'realizowane|wstrzymane |odebrane|do reklamacji ';
    _sstatusSerwis           : result := 'planowane|realizowane|wstrzymane |zakoñczone|anulowane|do reklamacji ';

    _kankieta                : result := 'ankieta';
    _nalepkiadresowe         : result := 'nalepki adresowe';
    _daneAdresowe            : result := 'dane adresowe';
    _dopisekNaFakturze       : result := 'dopisek na fakturze';
    _dopisekNaFakturzeL      : result := 'brak|orygina³ lub kopia|ORYGINA£-KOPIA';

    _zestawienieskrot        : result := 'skrót';
    _zestawieniepelne        : result := 'pe³ne';
    _formularze              : result := 'formularze';
    _teczka                  : result := 'teczka';
    _kolor                   : result := 'kolor';
    _psekretarz              : result := 'sekretarz';
    _ppocztaWchodzaca        : result := 'poczta przychodz¹ca';
    _ppocztawychodzaca       : result := 'poczta wychodz¹ca';
    _pnadanefaksy            : result := 'nadane faksy';
    _pdokumenty              : result := 'dokumenty';
    _ppocztaDziennik         : result := 'dziennik';
    _pkorespondent           : result := 'korespondent';
    _poplaty                 : result := 'op³aty';
    _pilzal                  : result := 'il.za³';
    _pmasa                   : result := 'masa ';
    _pkomudano               : result := 'komu dano';
    _podebrania              : result := 'odebrania';
    _ppodpisane              : result := 'pokwitowane';
    _pzalatwienia            : result := 'za³atwienia';
//    _pzakonczone             : result := 'zakoäczone';
    _pwyslane                : result := 'wys³ane';
    _psumaoplat              : result := 'suma op³at';
    _pdruklistdopodpisu      : result := 'druk list do pokwitowania';
    _ppodpisanieodbioru      : result := 'podpisanie odbioru';
    _ppotwierdzenieodbioru   : result := 'potwierdzenie odbioru';
    _listakodow              : result := 'lista kodów';
    _kontynuowac             : result := 'kontynuowaæ';
    _ppomijam                : result := 'pomijam';
    _ppodpisuje              : result := 'podpisujê';
    _psumaMas                : result := 'suma mas';
    _pNrNadania              : result := 'nr nadania';
    _ustawKontakt            : result := 'ustaw kontakt';
    _ustawTemat              : result := 'ustaw temat';
    _zaznaczKontakt          : result := 'zaznacz kontakt';
    _nowynamiar              : result := 'nowy namiar';
    _nowykontrahent          : result := 'nowy kontrahent';
    _zmianauzytkownika       : result := 'zmiana u¿ytkownika';
    _Oprogramie              : result := 'O programie';
    _zmienicmagazyn          : result := 'zmieniæ magazyn';
    _anulacjadokumentu       : result := 'anulacja dokumentu';
    _niewlasciwyDokument     : result := 'niew³aciwy dokument';

    _nFakturaVat             : result := 'Faktura VAT';
    _nRachunekUproszczony    : result := 'rachunek uproszczony';
    _nFakturaVatKorekta      : result := 'Faktura VAT korekta';
    _nKorektaFAVAT           : result := 'korekta FA VAT';
    _archiwizacja            : result := 'archiwizacja';
    _transmisje              : result := 'transmisje';
    _haslopelna              : result := 'pe³na nazwa';
    _Haslo                   : result := 'has³o ';
    _haslonazwa              : result := //'u¾ytkownik ';
                                           'login    ';
    _Hasloident              : result := 'identyfikacja';
    _Haslouzytkownicy        : result := 'u¿ytkownicy';
    _haslobiezace            : result := 'poprawa bie¿¹ce';
    _haslofirma              : result := 'zmiana firmy, katalogu';
    _haslobraknazwy          : result := 'brak nazwy';
    _haslobrakhasla          : result := 'brak has³a';
    _hasloPodwojone          : result := 'podwojony u¿ytkownik';
    _haslorekorduzywany      : result := 'rekord u¿ywany';
    _haslozaksiegowane      : result := 'poprawa zaksiêgowanych do KG';
    _hasloPotwierdzone       : result := 'potwierdzone dokumenty';
    _haslozakonczone         : result := 'poprawa zakoñczone (pow 1 mies) ';
    _haslostare              : result := 'poprawa otwarte (stare, do 1 mies)';
    _haslozakres             : result := ' zakres PK:  ';
    _haslozakresKarty        : result := ' zakres pracy: ';
    _Haslobrak               : result := 'brak u¿ytkownika';
    _Haslonieznany           : result := 'nieznany u¿ytkownik';
    _haslohelp               : result := 'F5 zmiana has³a';
    _HasloKL                 : result := ' has³o     ';
    _HasloKasuj              : result := ' kasuj     ';
    _haslokasujhelp          : result := 'kasowanie u¿ytkownika';
    _haslozmiana             : result := 'zmiana has³a';
    _hasloprzepisz           : result := 'powtórz has³o';
    _haslozmiananie          : result := 'has³a nie zmieniono';
    _haslozakaz              : result := 'zakaz zmiany obcego has³a';
    _haslodostep             : result := 'brak dostêpu';
    _haslozapis              : result := 'zakaz zapisu';
    _haslowejscie            : result := 'wejscie';
    _haslowyjscie            : result := 'wyjscie';
    _hasloEmail              : result := 'email';
    _hasloPrzerwa            : result := 'przerwa';
    _hasloUprawnienia        : result := 'uprawnienia';
    _hasloMagazynyAll        : result := 'wszystkie magazyny';
    _hasloZrezygnuj          : result := 'zrezygnuj';
    _haslobrakuprawnien      : result := 'brak uprawnieñ';
    _hasloOdbezpiecz         : result := 'odbezpiecz';
    _hasloProfilzmiana       : result := 'zmiana profilu';
    _hasloWydrukiMagazyn     : result := 'wydruki magazyn';
    _hasloWydrukiZamowienia  : result := 'wydruki zamówienia';
    _hasloPodgladNiezaplacone: result := 'podgl¹d niezap³acone ';
    _hasloPonownyWydruk      : result := 'ponowny wydruk ';
    _hsDostep                : result := 'dostêp';
    _hasloZdalnyDostep       : result := 'zdalny dostêp';
    _haslopowiekszonelitery  : result := 'powiêkszone litery';
    _hsMagazyn               : result := 'magazyn';
    _dyskbladzapisu          : result := 'blad zapisu na dyskietke';
    _dyskzapisaczmiany       : result := 'zapisaæ zmiany na dysk?';
    _dyskzapisaczmianyw      : result := 'Zapisaæ zmiany w %s ?';
    _ustawianiekonfiguracji  : result := 'ustawianie konfiguracji';
    _Btrwadrukowanie         : result := 'trwa drukowanie';
    _Bplikzajety             : result := 'plik zajêty';
    _Bzlezwolniono           : result := 'le zwolniono';
    _bniezgodneindeksy       : result := 'niezgodne indeksy ';
    _bdostepzarezerwowany    : result := 'dostêp zarezerwowany ';
    _bpowtorzonynumer        : result := 'powtórzony numer ';
    _bbrakkontrahenta        : result := 'brak kontrahenta';
    _bzakazzmianytowaru      : result := 'zakaz zmiany asortymentu';
    _bzakazzmianykontrahenta  : result := 'zakaz zmiany kontrahenta';
    _bnieprawidlowyVat       : result := 'nieprawid³owy VAT poz ';
    _bdrugakopiadrukowania   : result := 'druga kopia drukowania';
    _bzakazwydruku           : result := 'zakaz wydruku';
    _Btworzony               : result := 'tworzony plik %s';
    _BBladdysku              : result := 'b³¹d dysku';
    _bbrakpamieci            : result := 'brak pamiêci';
    _bbladotwarcia           : result := ' b³¹d otwarcia pliku';
    _Bwykonajsortowanie      : result := 'wykonaj sortowanie ';
    _Btrybczytania           : result := 'tryb czytania ';
    _bLoginfailure           : result := 'b³¹d logowania';
    _Bwyslanieniemozliwe     : result := 'wys³anie niemo¿liwe';
    _bnadpisac               : result := 'Nadpisaæ %s';
    _bbraktwain              : result := 'brak sterownika TWAIN';
    _bwszystkiepliki         : result := 'wszystkie pliki(*.*)|*.*';
    _bplikitekstowe          : result := 'pliki tekstowe (*.txt,*.doc,*.odt,*.docm, *.pdf)|*.txt;*.doc;*.docx;*.docm;*.txm;*.sxw;*.odt;*.pdf';
    _bplikiacrobat           : result := 'ADOBE ACROBAT (*.PDF)|*.pdf';
    _bplikigraficzne         : result := 'pliki graficzne (*.jpg,*.png,*.tif)|*.bmp;*.jpg;*.png;*.tif';
    _bplikiexcel             : result := 'pliki EXCEL (*.XLS)|*.xls';
    _bplikiDBF               : result := 'pliki DBF (*.DBF)|*.dbf';
    _bplikiCSV               : result := 'pliki CSV (*.csv,*.txt)|*.csv;*.txt;*.txm';
    _bplikiLOG               : result := 'pliki LOG (*.LOG)|*.log';
    _bplikixml               : result := 'pliki xml (*.xml)|*.xml';
    _bplikiEML               : result := 'pliki EML (*.EML)|*.eml';
    _wybranecechy            : result := 'wybrane cechy';
    _bladwyborupozycji       : result := 'bl¹d wyboru  pozycji';
    _wybierzwlasciwapozycje  : result := 'wybierz w³aciw¹ pozycje';
    _bBrakPliku              : result := 'brak pliku ';
    _bbladWysylania          : result := 'b³¹d wysy³ania ';
    _bzakonczonowysylanie    : result := 'zakoñczono wysy³anie';
    _bwpisznazwecechy        : result := 'wpisz nazwê cechy';
    _bniewypelnionepola      : result := 'nie wype³nione pola';
    _bbladzapisywania        : result := 'blad zapisywania';
    _bbrakfunkcji            : result := 'brak funkcji';
    _bbrakNumeru             : result := 'brak numeru';
    _bpozycjaZablokowana     : result := 'pozycja zablokowana';
    _haslopisanie            : result := 'pisanie';
    _hasloczytanie           : result := 'czytanie';
    _sosoba                  : result := 'osoba';
    _sTelefon                : result := 'telefon';
    _sEmail                  : result := 'e-mail';
    _fprodukt                : result := 'produkt ';
    _Sdatazakupu             : result := 'data zakupu';
    _Scenazakupu             : result := 'cena zakupu';
    _Tsprzedaz               : result := 'sprzeda¿';
    _Tmiara                  : result := 'miara';
    _Tzakup                  : result := 'zakup';
    _tKurs                   : result := 'kurs';
    _tMarza                  : result := 'mar¿a';
    _tNetto                  : result := 'NETTO';
    _tBrutto                 : result := 'BRUTTO';
    _brutto                  : result := 'brutto';
    _netto                   : result := 'netto ';
    _tVAT                    : result := 'VAT';
    _VAT                     : result := 'VAT';
    _NIP                     : result := 'NIP';
    _tDewizy                 : result := 'dewizy';
    _tWycena                 : result := 'wycena';
    _tMasa                   : result := 'masa';
    _tMasaNT                 : result := 'masa netto';
    _tMasaBR                 : result := 'masa brutto';
    _tKolejnyWolny           : result := 'kolejny wolny ';
    _tWpisac                 : result := 'wpisaæ';
    _tOdrzucic               : result := 'odrzuciæ';
    _wyslanyprzelew          : result := 'wys³any przelew';
    _jednostka               : result := 'jednostka';
    _Tkatalog                : result := 'katalog';
    _tOpakowanie             : result := 'opakowanie';
    _tOznaczenie             : result := 'oznaczenie';
    _tPaczka                 : result := 'paczka';
    _tminimum                : result := 'minimum';
    _tmaximum                : result := 'maximum';
    _Szbiorwzorca            : result := 'zbiór wzorca';
    _saktualizowac           : result := 'aktualizowaæ ';
    _kdolaczwczytaj          : result := 'do³¹cz/wczytaj ';
    _splanowanytr            : result := 'planowany termin zakoñczenia';
    _sEdytorWbudowany        : result := 'Edytor wbudowany';
    _sIlosckopii             : result := 'ilosc kopii';
    _sNumerDokumentu         : result := 'numer dokumentu ';
    _sNotatkaLista           : result := 'notatka|roboczy| wychodz¹cy| przychodz¹cy| wiedza';
    _kWyslijdrukuj           : result := 'wylij/drukuj  ';
    _sZakonczenie            : result := 'zakoñczenie sprawy  ';
    _Srejestrsprzedazy       : result := 'rejestr sprzeda¿y';
    _sanalizasyntetyczna     : result := 'analiza syntetyczna';
    _Strescispraw            : result := 'treci spraw';
    _szapisprzebiegusprawy   : result := 'zapis przebiegu sprawy';
    _Sutworzycwzorzec        : result := 'utworzyæ wzorzec ';
    _Swprowadza              : result := 'wprowadza';
    _Swykona                 : result := 'wykona';
    _snastepny               : result := 'nast©pny';
    _sdnioczekiwania         : result := 'dni oczekiwania';
    _spsukcesu               : result := '% sukcesu';
    _spytania                : result := 'pytania';
    _sjezykR                 : result := 'polski|english|Deutsch|Español|French|Norway|polski||number';
    _swalutaR                : result := 'z³|USD|EUR|GBP|NOK';
    _sRejestr                : result := 'rejestr';
    _sWydrukRejestru         : result := 'wydruk rejestru';
    _kkartoteka              : result := 'kartoteka  ';
    _kkarta                  : result := 'karta      ';
    _kZatwierdz              : result := 'zatwierd  ';             //klawisz
    _khZatwierdz             : result := 'F9 zapisanie pozycji';
    _Kporzuc                 : result := ' porzuæ    ';             //klawisz - wa¿na d³ugoæ 9 znaków
    _khPorzuc                : result := 'ESC wyjcie bez zapisu';
    _KStart                  : result := ' Start     ';
    _KHStart                 : result := 'F9 wykonanie zestawienia';
    _Kszukaj                 : result := ' szukaj    ';
    _KHSzukaj                : result := 'F7 szukanie wyrazu';
    _KEdycja                 : result := ' edycja    ';
    _KHEdycja                : result := 'F4 poprawa pozycji';
    _KPoprawa                : result := ' poprawa   ';
    _KHPoprawa               : result := 'F4 poprawa pozycji';
    _Kzmiana                 : result := ' zmiana    ';
    _Kpodglad                : result := ' podgl¹d   ';
    _KHpodglad               : result := 'F3 podgl¹d ';
    _KHpodgladwydruku        : result := 'podgl¹d wydruku';
    _khWyslijdo              : result := 'Wylij do ..';
    _kWyslijdo               : result := 'Wylij do  ';
    _kWyslijKancelaria       : result := 'do kancelarii';
    _kwszystko               : result := 'wszystko';
    _khwszystko              : result := 'wszystko razem';
    _kzeruj                  : result := '   zeruj   ';
    _khzeruj                 : result := 'zeruj ustawienia';
    _Kdomyslnie              : result := ' domylnie ';             //klawisz - wa¿na d³ugoæ 9 znaków
    _khdomyslnie             : result := 'F5 przepisz ustawienia domylne';
    _ustawienia              : result := 'ustawienia';
    _odwrotnie               : result := 'odwrotnie ';
    _zprzeniesienia          : result := 'z przeniesienia';
    _Kdopisz                 : result := ' dopisz    ';
    _KdopiszKON              : result := ' dopisz    ';
    _KHdopisz                : result := 'F2 dopisanie nowej pozycji';
    _Kpowrot                 : result := ' powrót    ';
    _KHpowrot                : result := 'zakoñczenie i powrót do poprzedniego okna';
    _Kmenu                   : result := ' menu      ';
    _KHmenu                  : result := ' spis funkcji';
    _kwyjscie                : result := ' wyjcie   ';
    _KHwyjscie               : result := 'zakoñczenie pracy funkcji';
    _Kdrukowanie             : result := ' drukowanie';
    _KHdrukowanie            : result := ' drukowanie';
    _kOdswiez                : result := ' odwie¿   ';
    _khOdswiez               : result := 'odwie¿enie zawartoci';
    _knowy                   : result := ' nowy      ';
    _kSchowek                : result := 'schowek';
    _khschowek               : result := 'schowek';
    _kOperacje               : result := 'operacje';
    _kZaznacz                : result := ' zaznacz   ';
    _khZaznacz               : result := 'zaznaczenie pozycji';
    _kOdznacz                : result := ' odznacz   ';
    _khOdznacz               : result := 'odznaczenie pozycji';
    _kModyfikuj              : result := 'modyfikuj';
    _khModyfikuj             : result := 'zbiorcze modyfikacje';
    _Kzestawienia            : result := 'zestawienia';
    _KWydruki                : result := 'wydruki';
    _slownie                 : result := 's³ownie';
    _KHlistakontaktow        : result := 'lista kontaktów';
    _KHlistaspraw            : result := 'lista spraw';
    _Klistakontaktow         : result := 'lista kontaktów';
    _Klistaspraw             : result := 'lista spraw';
    _KlistaZlecen            : result := 'lista zleceñ';
    _KHscenariusze           : result := 'procedury, wzorce postêpowania';
    _Kscenariusze            : result := 'lista procedur';
    _kListaAtrybutow         : result := 'lista atrybutów';
    _KHlistaProduktow        : result := 'lista produktów,towarów CTRL P';
    _KHlistaProduktow1       : result := 'lista produktów';
    _KlistaProduktow         : result := 'lista towarów';
    _KHsetup                 : result := 'konfiguracja';
    _Ksetup                  : result := 'konfiguracja';
    _KHfakturowanie          : result := 'fakturowanie';
    _kodpowiedz              : result := 'odpowied     ';
    _kprzeslij               : result := 'przeslij dalej';
    _khodpowiedz             : result := 'odpowied     ';
    _khprzeslij              : result := 'przeslij dalej';
    _kaprobata               : result := 'aprobata';
    _kHaprobata              : result := 'aprobata';
    _Klistakampanii          : result := 'kampanie';
    _KHlistakampanii         : result := 'kampanie-przedsiêwziêcia- wysy³ki';
    _kZapisz                 : result := ' zapisz    ';
    _kUwagi                  : result := ' uwagi     ';
    _kHUwagi                 : result := 'F5 dodatkowe uwagi';
    _Kselekcja               : result := ' selekcja  ';
    _KHselekcja              : result := 'ustawianie filtru wywietlania';
    _kKopiuj                 : result := ' kopiuj ';
    _kWklej                  : result := ' wklej  ';
    _kWytnij                 : result := ' wytnij ';
    _KlistaTematow           : result := 'tematy';
    _KHlistaTematow          : result := 'tematy, zadania, obszary';
    _kPlanKont               : result := 'plan kont';
    _kHPlanKont              : result := 'plan kont';
    _Kfiltr                  : result := ' filtrowanie';
    _KHfiltr                 : result := 'ustawianie filtru wywietlania';

    _kKontakty               : result := 'kontakty';
    _kSprawy                 : result := 'sprawy';
    _kHdziennik               : result := 'dziennik ';
    _kZadania                : result := 'zadania';
    _kHKalendarz              : result := 'kalendarz';
    _kKalendarz              : result := 'kalendarz';
    _kHtowary                : result := 'towary   ';
    _KDpodobny               : result := 'd.podobny';
    _KHDpodobny              : result := 'dopisywanie z kopiowaniem CTRL F2';
    _Kwypelnij               : result := 'wype³nij';
    _KHwypelnij              : result := 'do³¹czenie kolejnych zaznaczonych klientow';
    _Kagregacja              : result := 'agregacja';

    _kHwizyty                : result := 'wizyty';
    _kHlisty                 : result := 'listy, przesy³ki';
    _kHtelefony              : result := 'telefony, SMS, fax';
    _Kduplikaty              : result := 'duplikaty';


    _KHagregacja             : result := 'wykonanie zestawienia';
    _Kadministracja          : result := 'administracja';
    _kimagazyn               : result := 'magazyn      ';
    _Kzarzadzanie            : result := 'zarz¹dzanie';
    _kListaoperatorow        : result := 'lista Operatorów';
    _Kzalacznik              : result := 'za³¹cznik  ';
    _kHzalacznik             : result := 'do³¹czanie i podgl¹d za³¹czników';
    _kEdytor                 : result := 'edytor     ';
    _kHEdytor                : result := 'edytor tekstów';
    _Kkliknij2               : result := '  kliknij *2 aby dodaæ nowy';
    _KPopraw                 : result := ' poprawa   ';
    _Kskanuj                 : result := 'skanuj     ';
    _KHskanuj                : result := ' skanuj dokument';
    _kZnalaz                 : result := ' ENTER znalaz³   ';
    _khznalaz                : result := ' <ENTER> ';
    _Kszablon                : result := 'szablon';
    _KHszablon               : result := 'szablon';
    _Kwgszablonu             : result := 'wg szablonu';
    _KHwgszablonu            : result := 'wg szablonu';
    _khistoriaW              : result := 'historia';
    _kHhistoriaW             : result := 'historia';

    _lkontaktRodzaj          : result := 'nie klasyfikowany|namiar|kontrahent|pracownik|oddzia³|nieaktualny|inne';
    _lkontaktSelekcja        : result := '|namiar|kontrahent';
    _lOperatorRodzaj         : result := 'osoba|grupa';
    _lPromocjaRodzaj         : result := '|nowoæ|promocja|obni¿ka|podwy¿ka|wyprzeda¿|nieaktualne';
    _khistoria               : result := ' historia  ';
    _kzapas                  : result := ' zapas     ';
    _lszablonyPlikow         : begin
      result := 'szablony|*.dot;*.dotm;*.dotx;*.rtf;*.txm|szablony MADAR (*.txm)|*.txm|OpenOffice|*.swx;*.odt|pliki RTF (*.rtf)|*.rtf|szablony WORD(*.dot;*.doc;*.docm)|*.dot;*.dotm;*.doc;*.dotx;*.docm|';
         result:=result+'pliki tekstowy (*.txt)|*.txt|wszystkie|*.*';
         end;
    _MIDopisywanie           : result := 'dopisywanie'#9'F2';
    _MIAktualizacja          : result := 'aktualizacja'#9'F4';
    _MISortowanie            : result := 'sortowanie'#9'F6';
    _MISzukanie              : result := 'szukanie'#9'F7';
    _MIKasowanie             : result := 'kasowanie';
    _MIKopiowanie            : result := 'kopiowanie na dysk';
    _MIPodglad               : result := 'podgl¹d'#9'F3';
    _MIZamknij               : result := 'zamknij';
    _MICzytajZDysku          : result := 'czytaj z dysku';
    _MIZapiszNaDysk          : result := 'zapisz na dysk';
    _MIGenerujSzablon        : result := 'generuj szablon';
    _MIEdytujSzablon         : result := 'edytuj szablon';
    _MIczydopisac            : result := 'czy dopisaæ rekord';
    _MISzukanywyraz          : result := 'szukany wyraz';
    _Miskasowacrekord        : result := 'czy skasowaæ rekord';
    _MiBaza                  : result := 'baza';
    _miRodzajKonwersji       : result := 'rodzaj konwersji';

    _miskasowacrekord2       : result := 'odtworzyæ skasowany rekord ';
    _mizakonczyc             : result := 'zakoñczyæ';
    _miskasowac              : result := 'skasowaæ';
    _miPozycjaSkasowana      : result := 'pozycja skasowana';
    _mizakazwtrakcieedycji   : result := 'zakaz w trakcie edycji';
    _mizakazwtrakciedopisywania: result:='zakaz w trakcie dopisywania : zapisz i przejd do edycji';
    _miCzyIstniejeNowa       : result := 'czy istnieje nowa wersja ?';
    _miwyrazunieznaleziono   : result:=  'wyrazu %s nie znaleziono';




    _punktualnoscR           : result := '|punktualnie|<- 1 +> godzina|w dniu|<+ 1 -> dzieñ|<+ 3 -> dni ';
    _potrzebnyczasR          : result := '|do 10 min|do 30 min|1.5 godziny|do 5 godziñ|1 dzieñ|3 dni ';
    _ozakupie                : result := 'o zakupie';
    _zdecydowaloA            : result := 'zdecydowa³o';
    _firmeznaz               : result := 'firmê zna z';



    _sSprawaElement          : result := '|treæ|daty|wykonanie|tytu³|kontakt|dokument ';
    _sSprawaZalacznik        : result := '|nazwa|scie¿ka|czas|wielkoæ|po kompresji|opis|inne |etap|status ';
    _sstatusZalacznik        : result := 'robocze|otrzymane|wys³ane|wewnêtrzne';
    _sStatusOferta           : result := 'wszystkie|bie¿¹ce|nadzorowane|od³o¿one|cykliczne|kosz|zamówienia niepotwierdzone|do fakturowania|nie zap³acone|wg daty ';
    _ROperatopra             : result := '|tylko prowadz¹cy|tylko przyjmuj¹cy|tylko wykonuj¹cy ';


//    10000                    : result := 'aaaaaaaaaaaa';


    1731                     : result := '1731';
    1732                     : result := '1732';



    else Result := '';
  end;
end;   //koniec polskiego




begin
  if pl(1732)<>'1732' then
    halt(1);
end.
i:q
:q
:q!

