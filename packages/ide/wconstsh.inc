{
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 2000 by Berczi Gabor

    Strings for utilities

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ $undef USERESSTRINGS} { this doesn't compile under FP!!! why? }

{$ifdef USERESSTRINGS}
    resourcestring
{$else}
    const
{$endif}

    btn_OK = 'O~K~';
    btn_Cancel = 'MÇgsem';

    dialog_find = 'KeresÇs';
    label_find_options = 'Opci¢k';
    label_find_texttofind = '~K~eresendã';
    label_find_casesensitive = 'K~i~s-/nagybetãk kÅlîn';
    label_find_wholewordsonly = 'Csak teljes sza~v~akat';
    label_find_direction = 'Ir†ny';
    label_find_forward = 'Elãre (~d~)';
    label_find_backward = 'VisszafelÇ (~b~)';
    label_find_scope = 'Szk¢p';
    label_find_global = 'Teljes szîve~g~';
    label_find_selectedtext = 'Kiv†la~s~ztott szîveg';
    label_find_origin = 'Kiindul†si pont';
    label_find_fromcursor = 'A kur~z~ort¢l';
    label_find_entirescope = 'T~e~ljes szk¢p';

    dialog_replace = 'Csere';
    label_replace_texttofind = label_find_texttofind;
    label_replace_newtext = '   ~È~j szîveg';
    label_replace_options = label_find_options;
    label_replace_casesensitive = label_find_casesensitive;
    label_replace_wholewordsonly = label_find_wholewordsonly;
    label_replace_promptonreplace = '~P~rompt on replace';
    label_replace_direction = label_find_direction;
    label_replace_forward = label_find_forward;
    label_replace_backward = label_find_backward;
    label_replace_scope = label_find_scope;
    label_replace_global = label_find_global;
    label_replace_selectedtext = label_find_selectedtext;
    label_replace_origin = label_find_origin;
    label_replace_fromcursor = label_find_fromcursor;
    label_replace_entirescope = label_find_entirescope;
    btn_replace_changeall = '~A~z îsszeset';

    dialog_gotoline = 'Ugr†s sorra';
    label_gotoline_linenumber = 'Az £j sor s~z~†ma';

    msg_invalidmarkindex = 'êrvÇnytelen pont index (%d)';
    msg_marknotset = 'A %d. pont nincs be†ll°tva.';

    msg_foldboundsarenotvalid = 'A hajt†s hat†rai nem ÇrvÇnyesek';

    msg_notenoughmemoryforthisoperation = 'Nincs elÇg mem¢ria a mãvelet elvÇgzÇsÇhez.';
    msg_errorreadingfile = 'Hiba a  %s f†jl olvas†sa kîzben.';
    msg_errorwritingfile = 'Hiba a  %s f†jl °r†sa kîzben.';
    msg_errorsavingfile = 'Hiba a %s f†jl mentÇse kîzben.';
    msg_errorcreatingfile = 'Hiba a %s f†jl lÇtrehoz†sa kîzben.';
    msg_filehasbeenmodifiedsave = 'A %s m¢dosult. Menti?';
    msg_saveuntitledfile = 'Elmenti a c°m nÇlkÅli f†jlt?';
    msg_filehadtoolonglines = #3'A %s f†jlnak t£l hossz£'#13+
                              #3'sora(i) voltak. Az elsã a %d';
    msg_filewasmodified = #3'A %s f†jlt'#13+
                          #3'egy m†sik program m¢dos°totta.'#13+
                          #3'FelÅl°rja az £jabb verzi¢t?';

    dialog_savefileas = 'F†jl mentÇse m†s nÇven';
    dialog_writeblocktofile = 'Blokk ki°r†sa f†jlba';
    dialog_readblockfromfile = 'Blokk beolvas†sa f†jlb¢l';

    label_name = '~N~Çv';

    msg_searchstringnotfound = 'A keresett karaktersor nem tal†lhat¢.';
    msg_replacethisoccourence = 'LecserÇljem ezt az elãfordul†st?';
    msg_fileexistsoverwrite = 'A %s f†jl m†r lÇtezik. FelÅl°rja?';

    { Help system }

    msg_nohelpfilesinstalled = 'Nincsenek s£g¢-f†jlok telep°tve.';
    msg_helpindex = 'S£g¢ index';
    msg_nohelpavailabelforthistopic = 'Ehhez a tÇm†hoz nem tartozik s£g¢.';
    msg_pagenotavailable = 'Az oldal nem elÇrhetã';
    msg_cantaccessurl = 'Sajnos nem tudom megnyitni a %s URL-t...';

    { WUtils }
    msg_bugcheckfailed = 'Belsã hiba: %s'#13+
                         'KÇrem, jelezze a szerzãnek!';
    msg_functionnotimplemented = #3'Ez a funkci¢ mÇg'#13+
                                 #3+'nincs kidolgozva...'#13+
                                 #3+'';

