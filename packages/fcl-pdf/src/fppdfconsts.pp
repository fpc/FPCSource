unit fppdfconsts;

{$mode ObjFPC}{$H+}

interface


const
  // Delimiters
  SPDFStartXRef = 'startxref';
  SPDFXRef      = 'xref';
  SPDFObj       = 'obj';
  SPDFEndObj    = 'endobj';
  SPDFStream    = 'stream';
  SPDFEndStream = 'endstream';
  SPDFTrailer   = 'trailer';

  SPDFTokenR = 'R';

  // Object types
  SPDFTypeXref           = 'XRef';
  SPDFTypeObjStm         = 'ObjStm';
  SPDFTypePage           = 'Page';
  SPDFTypePages          = 'Pages';
  SPDFTypeXObject        = 'XObject';
  SPDFTypePattern        = 'Pattern';
  SPDFTypeExtGState      = 'ExtGState';
  SPDFTypeFont           = 'Font';
  SPDFTypeFontDescriptor = 'FontDescriptor';
  SPDFTypeMask           = 'Mask';
  SPDFTypeOCG            = 'OCG';
  SPDFTypeAnnot          = 'Annot';
  SPDFTypeCatalog        = 'Catalog';

  // Known dictionary entry names

  // for streams
  SPDFKeyLength       = 'Length';
  SPDFKeyFilter       = 'Filter';
  SPDFKeyDecodeParms  = 'DecodeParms';
  SPDFKeyF            = 'F';
  SPDFKeyFFilter      = 'FFilter';
  SPDFKeyFDecodeParms = 'FDecodeParms';
  SPDFKeyN            = 'N';

  // Filter names
  SPDFFilterFlateDecode     = 'FlateDecode';
  SPDFFilterASCIIHexDecode  = 'ASCIIHexDecode';
  SPDFFilterASCII85Decode   = 'ASCII85Decode';
  SPDFFilterLZWDecode       = 'LZWDecode';
  SPDFFilterRunLengthDecode = 'RunLengthDecode';
  SPDFFilterCCITTFaxDecode  = 'CCITTFaxDecode';
  SPDFFilterJBIG2Decode     = 'JBIG2Decode';
  SPDFFilterDCTDecode       = 'DCTDecode';
  SPDFFilterJPXDecode       = 'JPXDecode';
  SPDFFilterCrypt           = 'Crypt';
  SPDFKeyPredictor          = 'Predictor';
  SPDFKeyColors             = 'Colors';
  SPDFKeyColumns            = 'Columns';
  SPDFKeyBitsPerComponent   = 'BitsPerComponent';

  // Pages
  SPDFKeyCount              = 'Count';
  SPDFKeyParent             = 'Parent';
  SPDFKeyKids               = 'Kids';

  // Page

  SPDFPageKeyLastModified         = 'LastModified';
  SPDFPageKeyResources            = 'Resources';
  SPDFPageKeyMediaBox             = 'MediaBox';
  SPDFPageKeyCropBox              = 'CropBox';
  SPDFPageKeyBleedBox             = 'BleedBox';
  SPDFPageKeyTrimBox              = 'TrimBox';
  SPDFPageKeyArtBox               = 'ArtBox';
  SPDFPageKeyBoxColorInfo         = 'BoxColorInfo';
  SPDFPageKeyContents             = 'Contents';
  SPDFPageKeyRotate               = 'Rotate';
  SPDFPageKeyGroup                = 'Group';
  SPDFPageKeyThumb                = 'Thumb';
  SPDFPageKeyB                    = 'B';
  SPDFPageKeyDur                  = 'Dur';
  SPDFPageKeyTrans                = 'Trans';
  SPDFPageKeyAnnots               = 'Annots';
  SPDFPageKeyAA                   = 'AA';
  SPDFPageKeyMetaData             = 'Metadata';
  SPDFPageKeyPieceInfo            = 'Pieceinfo';
  SPDFPageKeyStructParents        = 'StructParents';
  SPDFPageKeyID                   = 'ID';
  SPDFPageKeyPZ                   = 'PZ';
  SPDFPageKeySeparationInfo       = 'SeparationInfo';
  SPDFPageKeyTabs                 = 'Tabs';
  SPDFPageKeyTemplateInstantiated = 'TemplateInstantiated';
  SPDFPageKeyPresSteps            = 'PresSteps';
  SPDFPageKeyUserUnit             = 'UserUnit';
  SPDFPageKeyVP                   = 'VP';

  // Resource dictionary
  SPDFResourceKeyFont       = 'Font';
  SPDFResourceKeyExtGState  = 'ExtGState';
  SPDFResourceKeyColorSpace = 'ColorSpace';
  SPDFResourceKeyPattern    = 'Pattern';
  SPDFResourceKeyShading    = 'Shading';
  SPDFResourceKeyXObject    = 'XObject';
  SPDFResourceKeyProcSet    = 'ProcSet';
  SPDFResourceKeyProperties = 'Properties';



  // Object streams
  SPDFKeyFirst  = 'First';
  SPDFKeySize   = 'Size';
  SPDFKeyW      = 'W';
  SPDFKeyIndex  = 'Index';

  // Trailer(Object)
  SPDFKeyRoot = 'Root';
  SPDFKeyInfo = 'Info';
  SPDFKeyPrev = 'Prev';

  // Catalog
  SPDFKeyPages = 'Pages';

  // Document Info
  SPDFKeyTitle         = 'Title';
  SPDFKeyAuthor        = 'Author';
  SPDFKeySubject       = 'Subject';
  SPDFKeyKeywords      = 'Keywords';
  SPDFKeyCreator       = 'Creator';
  SPDFKeyProducer      = 'Producer';
  SPDFKeyCreationDate  = 'CreationDate';
  SPDFKeyModDate       = 'ModDate';
  SPDFKeyTrapped       = 'Trapped';

  // Inline image entries
  SPDFImageKeyBPC    = 'BPC';
  SPDFImageKeyW      = 'W';
  SPDFImageKeyCS     = 'CS';
  SPDFImageKeyD      = 'D';
  SPDFImageKeyDP     = 'D';
  SPDFImageKeyF      = 'F';
  SPDFImageKeyH      = 'H';
  SPDFImageKeyIM     = 'IM';
  SPDFImageKeyIntent = 'Intent';
  SPDFImageKeyI      = 'I';
  SPDFImageKeyG      = 'G';
  SPDFImageKeyRGB    = 'RGB';
  SPDFImageKeyCMYK   = 'CMYK';
  SPDFImageKey       = 'CMYK';
  SPDFImageKeyAHx    = 'AHx';
  SPDFImageKeyA85    = 'A85';
  SPDFImageKeyLZW    = 'LZW';
  SPDFImageKeyFl     =  'Fl';
  SPDFImageKeyRL     = 'RL';
  SPDFImageKeyCCF    = 'CCF';
  SPDFImageKeyDCT    = 'DCT';

  // Font keys
  SPDFFontKeyType         = 'Type';
  SPDFFontKeySubType      = 'Subtype';
  SPDFFontKeyBaseFont     = 'BaseFont';
  SPDFFontKeyEncoding     = 'Encoding';
  SPDFFontKeyDescendantFonts = 'DescendantFonts';
  SPDFFontKeyToUnicode    = 'ToUnicode';

  SPDFFontKeyName         = 'Name';
  SPDFFontKeyFontName     = 'FontName';
  SPDFFontKeyFamily       = 'FontFamily';
  SPDFFontKeyStretch      = 'FontStretch';
  SPDFFontKeyWeight       = 'FontWeight';
  SPDFFontKeyFlags        = 'Flags';
  SPDFFontKeyBBox         = 'FontBBox';
  SPDFFontKeyItalicAngle  = 'ItalicAngle';
  SPDFFontKeyAscent       = 'Ascent';
  SPDFFontKeyDescent      = 'Descent';
  SPDFFontKeyLeading      = 'Leading';
  SPDFFontKeyCapHeight    = 'CapHeight';
  SPDFFontKeyXHeight      = 'XHeight';
  SPDFFontKeyStemV        = 'StemV';
  SPDFFontKeyStemH        = 'StemH';
  SPDFFontKeyAvgWidth     = 'AvgWidth';
  SPDFFontKeyMaxWidth     = 'MaxWidth';
  SPDFFontKeyMissingWidth = 'MissingWidth';
  SPDFFontKeyFontFile     = 'FontFile';
  SPDFFontKeyFontFile2    = 'FontFile2';
  SPDFFontKeyFontFile3    = 'FontFile3';
  SPDFFontKeyCharSet      = 'CharSet';

  SPDFFontKeyStyle        = 'Style';
  SPDFFontKeyLang         = 'Lang';
  SPDFFontKeyFD           = 'FD';
  SPDFFontKeyCIDSet       = 'CIDSet';

  SPDFFontKeyBaseEncoding  = 'BaseEncoding';
  SPDFFontKeyDifferences   = 'Differences';
  SPDFFontKeyCharProcs     = 'CharProcs';
  SPDFFontKeyCIDSystemInfo = 'CIDSystemInfo';


  SPDFCIDSystemInfoKeyRegistry = 'Registry';
  SPDFCIDSystemInfoKeyOrdering = 'Ordering';
  SPDFCIDSystemInfoKeySupplement = 'Supplement';



  // CIDFont keys
  SPDFCIDFontKeyType           = 'Type';
  SPDFCIDFontKeySubtype        = 'Subtype';
  SPDFCIDFontKeyBaseFont       = 'BaseFont';
  SPDFCIDFontKeyCIDSystemInfo  = SPDFFontKeyCIDSystemInfo;
  SPDFCIDFontKeyFontDescriptor = 'FontDescriptor';
  SPDFCIDFontKeyDW             = 'DW';
  SPDFCIDFontKeyW              = 'W';
  SPDFCIDFontKeyDW2            = 'DW2';
  SPDFCIDFontKeyW2             = 'W2';
  SPDFCIDFontKeyCIDToGIDMap    = 'CIDToGIDMap';

  // CMAP keys
  SCMAPKeyType           = 'Type';
  SCMAPKeyCMapName       = 'CMapName';
  SCMAPKeyCIDSystemInfo  = SPDFFontKeyCIDSystemInfo;
  SCMAPKeyWMode          = 'WMode';
  SCMAPKeyUseCMap        = 'UseCMap';

  SPDFExtGStateKeyType  = 'Type';
  SPDFExtGStateKeyLW    = 'LW';
  SPDFExtGStateKeyLC    = 'LC';
  SPDFExtGStateKeyLJ    = 'LJ';
  SPDFExtGStateKeyML    = 'ML';
  SPDFExtGStateKeyD     = 'D';
  SPDFExtGStateKeyRI    = 'RI';
  SPDFExtGStateKeyOP_U  = 'OP';
  SPDFExtGStateKeyop_l  = 'op';
  SPDFExtGStateKeyOPM   = 'OPM';
  SPDFExtGStateKeyFont  = 'Font';
  SPDFExtGStateKeyBG    = 'BG';
  SPDFExtGStateKeyBG2   = 'BG2';
  SPDFExtGStateKeyUCR   = 'UCR';
  SPDFExtGStateKeyUCR2  = 'UCR2';
  SPDFExtGStateKeyTR    = 'TR';
  SPDFExtGStateKeyTR2   = 'TR2';
  SPDFExtGStateKeyHT    = 'HT';
  SPDFExtGStateKeyFL    = 'FL';
  SPDFExtGStateKeySM    = 'SM';
  SPDFExtGStateKeySA    = 'SA';
  SPDFExtGStateKeyBM    = 'BM';
  SPDFExtGStateKeySMask = 'SMask';
  SPDFExtGStateKeyCA_U  = 'CA';
  SPDFExtGStateKeyca_l  = 'ca';
  SPDFExtGStateKeyAIS   = 'AIS';
  SPDFExtGStateKeyTK    = 'TK';


  // CJK Cmaps

  SCMAPGB_EUC_H        = 'GB-EUC-H';
  SCMAPGB_EUC_V        = 'GB-EUC-V';
  SCMAPGBPC_EUC_H      = 'GBpc-EUC-H';
  SCMAPGBPC_EUC_V      = 'GBpc-EUC-V';
  SCMAPGBK_EUC_H       = 'GBK-EUC-H';
  SCMAPGBK_EUC_V       = 'GBK-EUC-V';
  SCMAPGBKP_EUC_H      = 'GBKp-EUC-H';
  SCMAPGBKP_EUC_V      = 'GBKp-EUC-V';
  SCMAPGBK2K_H         = 'GBK2K-H';
  SCMAPGBK2K_V         = 'GBK2K-V';
  SCMAPUniGB_UCS2_H    = 'UniGB-UCS2-H';
  SCMAPUniGB_UCS2_V    = 'UniGB-UCS2-V';
  SCMAPUniGB_UTF16_H   = 'UniGB-UTF16-H';
  SCMAPUniGB_UTF16_V   = 'UniGB-UTF16-V';
  SCMAPB5pc_H          = 'B5pc-H';
  SCMAPB5pc_V          = 'B5pc-V';
  SCMAPHKscs_B5_H      = 'HKscs-B5-H';
  SCMAPHKscs_B5_V      = 'HKscs-B5-V';
  SCMAPETen_B5_H       = 'ETen-B5-H';
  SCMAPETen_B5_V       = 'ETen-B5-V';
  SCMAPETenms_B5_H     = 'ETenms-B5-H';
  CMAPETenms_B5_V      = 'ETenms-B5-V';
  CMAPCNS_EUC_H        = 'CNS-EUC-H';
  CMAPCNS_EUC_V        = 'CNS-EUC-V';
  CMAPUniCNS_UCS2_H    = 'UniCNS-UCS2-H';
  CMAPUniCNS_UCS2_V    = 'UniCNS-UCS2-V';
  CMAPUniCNS_UTF16_H   = 'UniCNS-UTF16-H';
  CMAPUniCNS_UTF16_V   = 'UniCNS-UTF16-V';
  CMAP83pv_RKSJ_H      = '83pv-RKSJ-H';
  CMAP90ms_RKSJ_H      = '90ms-RKSJ-H';
  CMAP90ms_RKSJ_V      = '90ms-RKSJ-V';
  CMAP90msp_RKSJ_H     = '90msp-RKSJ-H';
  CMAP90msp_RKSJ_V     = '90msp-RKSJ-V';
  CMAP90pv_RKSJ_H      = '90pv-RKSJ-H';
  CMAPAdd_RKSJ_H       = 'Add-RKSJ-H';
  CMAPAdd_RKSJ_V       = 'Add-RKSJ-V';
  CMAPEUC_H            = 'EUC-H';
  CMAPEUC_V            = 'EUC-V';
  CMAPExt_RKSJ_H       = 'Ext-RKSJ-H';
  CMAPExt_RKSJ_V       = 'Ext-RKSJ-V';
  CMAPH                = 'H';
  CMAPV                = 'V';
  CMAPUniJIS_UCS2_H    = 'UniJIS-UCS2-H';
  CMAPUniJIS_UCS2_V    = 'UniJIS-UCS2-V';
  CMAPUniJIS_UCS2_HW_H = 'UniJIS-UCS2-HW-H';
  CMAPUniJIS_UCS2_HW_V = 'UniJIS-UCS2-HW-V';
  CMAPUniJIS_UTF16_H   = 'UniJIS-UTF16-H';
  CMAPUniJIS_UTF16_V   = 'UniJIS-UTF16-V';
  CMAPKSC_EUC_H = 'KSC-EUC-H';
  CMAPKSC_EUC_V = 'KSC-EUC-V';
  CMAPKSCms_UHC_H = 'KSCms-UHC-H';
  CMAPKSCms_UHC_V = 'KSCms-UHC-V';
  CMAPKSCms_UHC_HW_H = 'KSCms-UHC-HW-H';
  CMAPKSCms_UHC_HW_V = 'KSCms-UHC-HW-V';
  CMAPKSCpc_EUC_H = 'KSCpc-EUC-H';
  CMAPUniKS_UCS2_H = 'UniKS-UCS2-H';
  CMAPUniKS_UCS2_V = 'UniKS-UCS2-V';
  CMAPUniKS_UTF16_H = 'UniKS-UTF16-H';
  CMAPUniKS_UTF16_V = 'UniKS-UTF16-V';
  CMAPIdentity_H = 'Identity-H';
  CMAPIdentity_V = 'Identity-V';

  SPDFColorSpaceDeviceGray  = 'DeviceGray';
  SPDFColorSpaceDeviceRGB   = 'DeviceRGB';
  SPDFColorSpaceDeviceCMYK  = 'DeviceCMYK';
  SPDFColorSpacePattern     = 'Pattern';
  SPDFColorSpaceCalGray     = 'CalGray';
  SPDFColorSpaceCalRGB      = 'CalRGB';

implementation

end.

