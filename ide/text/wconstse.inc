{
    $Id$
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 2000 by Berczi Gabor

    Strings for utilities

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$undef USERESSTRINGS} { this doesn't compile under FP!!! why? }

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
    label_replace_texttofind = label_find_texttofind;
    label_replace_newtext = '    ~N~ew text';
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
    btn_replace_changeall = 'Change ~a~ll';

    dialog_gotoline = 'Goto line';
    label_gotoline_linenumber = 'Enter new line ~n~umber';

    msg_invalidmarkindex = 'Invalid mark index (%d)';
    msg_marknotset = 'Mark %d not set.';

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

    dialog_savefileas = 'Save File As';
    dialog_writeblocktofile = 'Write Block to File';
    dialog_readblockfromfile = 'Read Block from File';

    label_name = '~N~ame';

    msg_searchstringnotfound = 'Search string not found.';
    msg_replacethisoccourence = 'Replace this occurence?';
    msg_fileexistsoverwrite = 'File %s already exists. Overwrite?';

    { Help system }

    msg_nohelpfilesinstalled = 'No help files installed.';
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

{
  $Log$
  Revision 1.1  2000-07-13 09:48:37  michael
  + Initial import

  Revision 1.1  2000/06/16 09:30:01  pierre
   * new files from Gabor


}
