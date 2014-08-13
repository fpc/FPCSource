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

    btn_OK = 'O~K';
    btn_Cancel = 'Cancel';

    dialog_find = 'Find';
    label_find_options = 'Options';
    label_find_texttofind = '~T~ext to find';
    label_find_casesensitive = '~C~ase sensitive';
    label_find_wholewordsonly = '~W~hole words only';
    label_find_useregexp = '~U~se regular expr.';
    label_find_direction = 'Direction';
    label_find_forward = 'Forwar~d~';
    label_find_backward = '~B~ackward';
    label_find_scope = 'Scope';
    label_find_global = '~G~lobal';
    label_find_selectedtext = '~S~elected text';
    label_find_origin = 'Origin';
    label_find_fromcursor = '~F~rom cursor';
    label_find_entirescope = '~E~ntire scope';

    dialog_replace = 'Replace';
    label_replace_texttofind = '~T~ext to find';
    label_replace_newtext = '    ~N~ew text';
    label_replace_options = 'Options';
    label_replace_casesensitive = '~C~ase sensitive';
    label_replace_useregexp = '~U~se regular expr.';
    label_replace_wholewordsonly = '~W~hole words only';
    label_replace_promptonreplace = '~P~rompt on replace';
    label_replace_direction = 'Direction';
    label_replace_forward = 'Forwar~d~';
    label_replace_backward = '~B~ackward';
    label_replace_scope = 'Scope';
    label_replace_global = '~G~lobal';
    label_replace_selectedtext = '~S~elected text';
    label_replace_origin = 'Origin';
    label_replace_fromcursor = '~F~rom cursor';
    label_replace_entirescope = '~E~ntire scope';
    btn_replace_changeall = 'Change ~a~ll';

    dialog_gotoline = 'Goto line';
    label_gotoline_linenumber = 'Enter new line ~n~umber';

    msg_invalidmarkindex = 'Invalid mark index (%d)';
    msg_marknotset = 'Mark %d not set.';

    msg_foldboundsarenotvalid = 'Fold bounds are not valid';

    msg_notenoughmemoryforthisoperation = 'Not enough memory for this operation.';
    msg_errorreadingfile = 'Error reading file %s.';
    msg_errorwritingfile = 'Error writing file %s.';
    msg_errorsavingfile = 'Error saving file %s.';
    msg_errorcreatingfile = 'Error creating file %s.';
    msg_filehasbeenmodifiedsave = '%s has been modified. Save?';
    msg_saveuntitledfile = 'Save untitled file?';
    msg_filehadtoolonglines = #3'File %s had too long lines'#13+
                              #3'first such line is %d';
    msg_filewasmodified = #3'File %s '#13+
                          #3'was modified by another program.'#13+
                          #3'Overwrite new version?';
    msg_reloaddiskmodifiedfile = #3'File %s '#13+
                          #3'was modified by another program.'#13+
                          #3'Reload new version?';
    msg_reloaddiskandidemodifiedfile = #3'File %s '#13+
                          #3'was modified by another program,'#13+
                          #3'but the IDE version wasn''t saved before.'#13+
                          #3'Reload new on disk version and loose IDE modifications?';
    dialog_savefileas = 'Save File As';
    dialog_writeblocktofile = 'Write Block to File';
    dialog_readblockfromfile = 'Read Block from File';

    label_name = '~N~ame';

    msg_searchstringnotfound = 'Search string not found.';
    msg_replacethisoccourence = 'Replace this occurence?';
    msg_fileexistsoverwrite = 'File %s already exists. Overwrite?';
    msg_readingwinclipboard = 'Reading windows clipboard';
    msg_copyingwinclipboard = 'Copying to windows clipboard';
    msg_pastingclipboard = 'Pasting clipboard';
    msg_copyingclipboard = 'Copying to clipboard';
    msg_cutting = 'Cutting';
    { Help system }

    msg_nohelpfilesinstalled1 = 'To keep the size of the FPC download reasonably low, the CHM help files';
    msg_nohelpfilesinstalled2 = 'which are necessary for the IDE help to function, are omitted.';
    msg_nohelpfilesinstalled3 = 'To get these docs, go to ftp://ftp.freepascal.org/pub/fpc/dist/ and get doc-chm.zip';
    msg_nohelpfilesinstalled4 = 'for your version. Extract the CHM files and add them to the IDE using Help|Files,';
    msg_nohelpfilesinstalled5 = 'add toc.chm first.';
    msg_helpindex = 'Help index';
    msg_nohelpavailabelforthistopic = 'No help available for this topic.';
    msg_pagenotavailable = 'Page not available';
    msg_cantaccessurl = 'Sorry, can''t access the URL: %s ...';

    { WUtils }
    msg_bugcheckfailed = 'Bug check failed: %s'#13+
                         'Please report to author!';
    msg_functionnotimplemented = #3'This function is not'#13+
                                 #3+'yet implemented...'#13+
                                 #3+'Sorry';

