{$mode objfpc}
{$h+}

unit fpdemsg;

interface

ResourceString

{ ---------------------------------------------------------------------
    Strings which appear in the program
  ---------------------------------------------------------------------}


  SFPDE        = 'Free Pascal documentation editor';
  SName        = 'Name';
  SOK          = ' OK ';
  SCancel      = ' Cancel ';
  SNewDocument = 'New document';
  SNew         = 'New';
  SInsertLink  = 'Insert link';
  SInsertTable = 'Insert table';
  SForFile     = ' in file ';
  SForPackage  = ' in package ';
  SForModule   = ' in module ';
  SForTopic    = ' in topic';
  SLinkTarget  = 'Link target';
  SLinkText    = 'Link text';
  STableRows   = 'Rows';
  STableCols   = 'Columns';
  STableHeader = 'Use header row';
  SFileModified = 'Document "%s" was modified, would you like to save it?';
  SDeletePackage = 'Are you sure you want to delete package "%s" ?';
  SDeleteModule  = 'Are you sure you want to delete module "%s" ?';
  SDeleteTopic  = 'Are you sure you want to delete topic "%s" ?';
  SDeleteElement = 'Are you sure you want to delete element "%s" ?';
  SRenamePackage = 'Rename package';
  SRenameModule  = 'Rename module';
  SRenameTopic   = 'Rename topic';
  SRenameElement = 'Rename element';
  SNoElement = 'No element selected';
  SDataForElement = 'Documentation for element "%s":';
  SShortDescription = 'Short';
  SDescription = 'Description';
  SErrors = 'Errors';
  SSeeAlso = 'See Also';
  SCodeExample = 'Example code File';
  SMakeSkelFromSource = 'Make new document from source file';
  SSkelErrorWithFile = 'makeskel reported an error (%d). Try to load produced file anyway ?';
  SSkelErrorWithoutFile = 'makeskel reported an error (%d) and produced no file.';
  SOptConfirmDelete = 'Confirm node deletion';
  SOptCreateBackup = 'Backup existing files';
  SOptSkipEmptyNodes = 'Do not create empty nodes';
  SOptBackupExtension = 'Backup file extension';
  SOptDefaultExtension = 'Default extension for new files';
  SOptMaxRecentUsed = 'Items in MRU list';
  SAboutText = 'fpdoc editor 1.0'#10'(c) 2002 Michael Van Canneyt'#10+
               'See http://www.freepascal.org/';

{ ---------------------------------------------------------------------
    Menu strings
  ---------------------------------------------------------------------}

  SMenuOpen          = '_Open';
  SMenuNew           = '_New';
  SMenuNewFromSource = 'New from so_urce';
  SMenuSave          = '_Save';
  SMenuSaveAs        = 'Save _as';
  SMenuRecent        = '_Recent';
  SMenuClose         = '_Close';
  SMenuExit          = 'E_xit';
  SMenuFile          = 'File';

  SMenuInsert          = 'Insert';
  SMenuInsertPackage   = '_Package';
  SMenuInsertModule    = '_Module';
  SMenuInsertTopic     = 'T_opic';
  SMenuInsertElement   = '_Element';
  SMenuInsertLink      = '_Link';
  SMenuInsertTable     = '_Table';

  SMenuFormat           = 'Format';
  SMenuFormatBold       = '_Bold';
  SMenuFormatUnderLine  = '_Underline';
  SMenuFormatItalics    = '_Italic';
  SMenuformatVariable   = '_Variable';
  SMenuFormatRemark     = '_Remark';
  SMenuFormatParaGraph  = '_Paragraph';
  SMenuFormatCode       = '_Code';
  SMenuFormatFile       = '_File';

  SMenuRename           = 'Rename';
  SMenuDelete           = 'Delete';

  SMenuExtra            = 'Extra';
  SMenuExtraoptions     = 'Options';

  SMenuHelp             = 'Help';
  SMenuHelpAbout            = 'About';

{ ---------------------------------------------------------------------
    Hint strings
  ---------------------------------------------------------------------}
  SHintFileNew       = 'New file';
  SHintFileOpen      = 'Open file';
  SHintFileSave      = 'Save file';
  SHintFileSaveAs    = 'Save file as';

  SHintInsertPackage = 'New package';
  SHintInsertModule  = 'New module';
  SHintInsertTopic   = 'New topic';
  SHintInsertElement = 'New element';
  SHintInsertLink    = 'Insert link';
  ShintInsertTable   = 'Insert table';

  SMarkSelection     = 'Mark selection %s';

  SHMenuExtraOptions = 'Show options dialog';
  SHMenuHelpAbout        = 'About this program';

{ ---------------------------------------------------------------------
    Error messages.
  ---------------------------------------------------------------------}


  SErrNoPackageForModule = 'No package found to insert module "%s"';
  SErrNoNodeForTopic     = 'No parent node found to insert topic "%s"';
  SErrNoNodeForPackage   = 'No node found for package "%s"';
  SErrNoNodeForModule    = 'No node found for module "%s"';
  SErrNoModuleForElement = 'No module found to insert element "%s"';
  SErrNoNodeForElement   = 'No node found for element "%s"';
  SErrUnknownDomElement  = 'Unknwon DOM element as parent for selected element: "%s"';


  SSaveFileTitle = 'Enter filename to save to';
  SOpenFileTitle = 'Select file to open';

Function FormatHint(S : String) : String;

implementation

uses sysutils;

Function FormatHint(S : String) : String;

begin
  Result:=Format(SMarkSelection,[S]);
end;

end.
