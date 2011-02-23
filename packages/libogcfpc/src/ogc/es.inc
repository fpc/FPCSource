
{$IFDEF OGC_INTERFACE}
{$ifdef HW_RVL}

const
  ES_EINVAL = - $1004;
  ES_ENOMEM = - $100C;
  ES_ENOTINIT = - $1100;
  ES_EALIGN = - $1101;
  ES_SIG_RSA4096 = $10000;
  ES_SIG_RSA2048 = $10001;
  ES_SIG_ECDSA = $10002;
  ES_CERT_RSA4096 = 0;
  ES_CERT_RSA2048 = 1;
  ES_CERT_ECDSA = 2;
  ES_KEY_COMMON = 4;
  ES_KEY_SDCARD = 6;

type
  sigtype = cuint32;
  sig_header = sigtype;
  signed_blob = sig_header;
  psigned_blob = ^signed_blob;

  sha1 = array [0..19] of cuint8;
  psha1 = ^sha1;
  aeskey = array [0..15] of cuint8;


  _sig_rsa2048 = packed record
    type_ : sigtype;
    sig : array [0..255] of cuint8;
    fill : array [0..59] of cuint8;
  end;
  sig_rsa2048 = _sig_rsa2048;


type
  _sig_rsa4096 = packed record
    type_ : sigtype;
    sig : array [0..511] of cuint8;
    fill : array [0..59] of cuint8;
  end;
  sig_rsa4096 = _sig_rsa4096;


type
  _sig_ecdsa = packed record
    type_ : sigtype;
    sig : array [0..59] of cuint8;
    fill : array [0..63] of cuint8;
  end;
  sig_ecdsa = _sig_ecdsa;


type
  sig_issuer = array [0..63] of cchar;


type
  _tiklimit = packed record
    tag : cuint32;
    value : cuint32;
  end;
  tiklimit = _tiklimit;


type
  _tikview = packed record
    view : cuint32;
    ticketid : cuint64;
    devicetype : cuint32;
    titleid : cuint64;
    access_mask : cuint16;
    reserved : array [0..59] of cuint8;
    cidx_mask : array [0..63] of cuint8;
    padding : cuint16;
    limits : array [0..7] of tiklimit;
  end;
  tikview = _tikview;
  ptikview = ^_tikview;


type
  _tik = packed record
    issuer : sig_issuer;
    fill : array [0..62] of cuint8;  //TODO: not really fill
    cipher_title_key : aeskey;
    fill2 : cuint8;
    ticketid : cuint64;
    devicetype : cuint32;
    titleid : cuint64;
    access_mask : cuint16;
    reserved : array [0..59] of cuint8;
    cidx_mask : array [0..63] of cuint8;
    padding : cuint16;
    limits : array [0..7] of tiklimit;
  end;
  tik = _tik;


type
  _tmd_content = packed record
    cid : cuint32;
    index_ : cuint16;
    type_ : cuint16;
    size : cuint64;
    hash : sha1;
  end;
  tmd_content = _tmd_content;
  ptmd_content = ^tmd_content;


  _tmd = packed record
    issuer : sig_issuer;  //0x140
    version : cuint8;  //0x180
    ca_crl_version : cuint8;  //0x181
    signer_crl_version : cuint8;  //0x182
    fill2 : cuint8;  //0x183
    sys_version : cuint64;  //0x184
    title_id : cuint64;  //0x18c
    title_type : cuint32;  //0x194
    group_id : cuint16;  //0x198
    zero : cuint16;  //0x19a
    region : cuint16;  //0x19c
    ratings : array [0..15] of cuint8;  //0x19e
    reserved : array [0..11] of cuint8;  //0x1ae
    ipc_mask : array [0..11] of cuint8;
    reserved2 : array [0..17] of cuint8;
    access_rights : cuint32;
    title_version : cuint16;
    num_contents : cuint16;
    boot_index : cuint16;
    fill3 : cuint16;
    // content records follow
    // C99 flexible array
    contents: Ptmd_content;
  end;
  tmd = _tmd;
  ptmd = ^tmd;


type
  _tmd_view_content = packed record
    cid : cuint32;
    index_ : cuint16;
    type_ : cuint16;
    size : cuint64;
  end;
  tmd_view_content = _tmd_view_content;
  ptmd_view_content = ^tmd_view_content;


  _tmdview = packed record
    version : cuint8;  // 0x0000;
    filler : array [0..2] of cuint8;
    sys_version : cuint64;  //0x0004
    title_id : cuint64;  // 0x00c
    title_type : cuint32;  //0x0014
    group_id : cuint16;  //0x0018
    reserved : array [0..61] of cuint8;  //0x001a this is the same reserved 0x3e bytes from the tmd
    title_version : cuint16;  //0x0058
    num_contents : cuint16;  //0x005a
    contents : ptmd_view_content;
  end;
  tmd_view = _tmdview;
  ptmd_view = ^tmd_view;


type
  _cert_header = packed record
    issuer : sig_issuer;
    cert_type : cuint32;
    cert_name : array [0..63] of cchar;
    cert_id : cuint32;  //???
  end;
  cert_header = _cert_header;
  pcert_header = ^_cert_header;


type
  _cert_rsa2048 = packed record
    issuer : sig_issuer;
    cert_type : cuint32;
    cert_name : array [0..63] of cchar;
    cert_id : cuint32;
    modulus : array [0..255] of cuint8;
    exponent : cuint32;
    pad : array [0..51] of cuint8;
  end;
  cert_rsa2048 = _cert_rsa2048;


type
  _cert_rsa4096 = packed record
    issuer : sig_issuer;
    cert_type : cuint32;
    cert_name : array [0..63] of cchar;
    cert_id : cuint32;
    modulus : array [0..511] of cuint8;
    exponent : cuint32;
    pad : array [0..51] of cuint8;
  end;
  cert_rsa4096 = _cert_rsa4096;


type
  _cert_ecdsa = packed record
    issuer : sig_issuer;
    cert_type : cuint32;
    cert_name : array [0..63] of cchar;
    cert_id : cuint32;  // ng key id
    r : array [0..29] of cuint8;
    s : array [0..29] of cuint8;
    pad : array [0..59] of cuint8;
  end;
  cert_ecdsa = _cert_ecdsa;


function TMD_SIZE(x: ptmd): cint; inline;

// backwards compatibility
function TMD_CONTENTS(x: ptmd): tmd_content; inline;

//TODO: add ECC stuff
function IS_VALID_SIGNATURE(x: psigned_blob): boolean; inline;

function SIGNATURE_SIZE(x: psigned_blob): cint; inline;

function SIGNATURE_SIG(x: psigned_blob): cint; inline;

function IS_VALID_CERT(x: cert_header): boolean; inline;

function CERTIFICATE_SIZE(x: cert_header): cint; inline;

function SIGNATURE_PAYLOAD(x: psigned_blob): pointer; inline;

function SIGNED_TMD_SIZE(x: psigned_blob): cint; inline;

function SIGNED_TIK_SIZE(x: psigned_blob): cint; inline;

function SIGNED_CERT_SIZE(x: psigned_blob): cint; inline;

function STD_SIGNED_TIK_SIZE(): cint; inline;

const
  MAX_NUM_TMD_CONTENTS = 512;

function MAX_TMD_SIZE(): cint; inline;

function MAX_SIGNED_TMD_SIZE(): cint; inline;

function __ES_Init: cint32; cdecl; external;

function __ES_Close: cint32; cdecl; external;

function __ES_Reset: cint32; cdecl; external;

function ES_GetTitleID(titleID: pcuint64): cint32; cdecl; external;

function ES_SetUID(uid: cuint64): cint32; cdecl; external;

function ES_GetDataDir(titleID: cuint64; filepath: pcchar): cint32; cdecl; external;

function ES_GetNumTicketViews(titleID: cuint64; cnt: pcuint32): cint32; cdecl; external;

function ES_GetTicketViews(titleID: cuint64; views: Ptikview; cnt: cuint32)
  : cint32; cdecl; external;

function ES_GetNumOwnedTitles(cnt: pcuint32): cint32; cdecl; external;

function ES_GetOwnedTitles(titles: pcuint64; cnt: cuint32): cint32; cdecl; external;

function ES_GetNumTitles(cnt: pcuint32): cint32; cdecl; external;

function ES_GetTitles(titles: pcuint64; cnt: cuint32): cint32; cdecl; external;

function ES_GetNumStoredTMDContents(stmd: Psigned_blob; tmd_size: cuint32;
  cnt: pcuint32): cint32; cdecl; external;

function ES_GetStoredTMDContents(stmd: Psigned_blob; tmd_size: cuint32;
  contents: pcuint32; cnt: cuint32): cint32; cdecl; external;

function ES_GetStoredTMDSize(titleID: cuint64; size: pcuint32): cint32; cdecl; external;

function ES_GetStoredTMD(titleID: cuint64; stmd: Psigned_blob; size: cuint32)
  : cint32; cdecl; external;

function ES_GetTitleContentsCount(titleID: cuint64; num: pcuint32): cint32; cdecl; external;

function ES_GetTitleContents(titleID: cuint64; data: pcuint8; size: cuint32)
  : cint32; cdecl; external;

function ES_GetTMDViewSize(titleID: cuint64; size: pcuint32): cint32; cdecl; external;

function ES_GetTMDView(titleID: cuint64; data: pcuint8; size: cuint32): cint32; cdecl; external;

function ES_GetNumSharedContents(cnt: pcuint32): cint32; cdecl; external;

function ES_GetSharedContents(contents: Psha1; cnt: cuint32): cint32; cdecl; external;

function ES_LaunchTitle(titleID: cuint64; view: Ptikview): cint32; cdecl; external;

function ES_LaunchTitleBackground(titleID: cuint64; view: Ptikview): cint32; cdecl; external;

function ES_Identify(certificates: Psigned_blob; certificates_size: cuint32;
  tmd: Psigned_blob; tmd_size: cuint32; ticket: Psigned_blob;
  ticket_size: cuint32; keyid: pcuint32): cint32; cdecl; external;

function ES_AddTicket(tik: Psigned_blob; tik_size: cuint32;
  certificates: Psigned_blob; certificates_size: cuint32; crl: Psigned_blob;
  crl_size: cuint32): cint32; cdecl; external;

function ES_DeleteTicket(view: Ptikview): cint32; cdecl; external;

function ES_AddTitleTMD(tmd: Psigned_blob; tmd_size: cuint32): cint32; cdecl; external;

function ES_AddTitleStart(tmd: Psigned_blob; tmd_size: cuint32;
  certificatess: Psigned_blob; certificatess_size: cuint32; crl: Psigned_blob;
  crl_size: cuint32): cint32; cdecl; external;

function ES_AddContentStart(titleID: cuint64; cid: cuint32): cint32; cdecl; external;

function ES_AddContentData(cid: cint32; data: pcuint8; data_size: cuint32)
  : cint32; cdecl; external;

function ES_AddContentFinish(cid: cuint32): cint32; cdecl; external;

function ES_AddTitleFinish: cint32; cdecl; external;

function ES_AddTitleCancel: cint32; cdecl; external;

function ES_ImportBoot(tik: Psigned_blob; tik_size: cuint32;
  tik_certs: Psigned_blob; tik_certs_size: cuint32; tmd: Psigned_blob;
  tmd_size: cuint32; tmd_certs: Psigned_blob; tmd_certs_size: cuint32;
  content: pcuint8; content_size: cuint32): cint32; cdecl; external;

function ES_OpenContent(index_: cuint16): cint32; cdecl; external;

function ES_OpenTitleContent(titleID: cuint64; views: Ptikview; index_: cuint16)
  : cint32; cdecl; external;

function ES_ReadContent(cfd: cint32; data: pcuint8; data_size: cuint32): cint32; cdecl; external;

function ES_SeekContent(cfd, where, whence: cint32): cint32; cdecl; external;

function ES_CloseContent(cfd: cint32): cint32; cdecl; external;

function ES_DeleteTitle(titleID: cuint64): cint32; cdecl; external;

function ES_DeleteTitleContent(titleID: cuint64): cint32; cdecl; external;

function ES_Encrypt(keynum: cuint32; iv, source: pcuint8; size: cuint32;
  dest: pcuint8): cint32; cdecl; external;

function ES_Decrypt(keynum: cuint32; iv, source: pcuint8; size: cuint32;
  dest: pcuint8): cint32; cdecl; external;

function ES_Sign(source: pcuint8; size: cuint32; sig, certs: pcuint8): cint32; cdecl; external;

function ES_GetDeviceCert(outbuf: pcuint8): cint32; cdecl; external;

function ES_GetDeviceID(device_id: pcuint32): cint32; cdecl; external;

function ES_GetBoot2Version(version: pcuint32): cint32; cdecl; external;

function ES_NextCert(certs: Psigned_blob): Psigned_blob; cdecl; external;

{$endif HW_RVL}

{$ENDIF}

{$IFDEF OGC_IMPLEMENTATION}
{$ifdef HW_RVL}

function TMD_SIZE(x: ptmd): cint; inline;
begin
  TMD_SIZE := (x^.num_contents) * sizeof(tmd_content) + sizeof(tmd);
end;
  // backwards compatibility

function TMD_CONTENTS(x: ptmd): tmd_content; inline;
begin
  TMD_CONTENTS := x^.contents^;
end;

//TODO: add ECC stuff

function IS_VALID_SIGNATURE(x: psigned_blob): boolean; inline;
begin
  IS_VALID_SIGNATURE :=	(x^ = ES_SIG_RSA2048) or (x^ = ES_SIG_RSA4096) or (x^ = ES_SIG_ECDSA);
end;

function SIGNATURE_SIZE(x: psigned_blob): cint; inline;
begin
	case x^ of
    ES_SIG_RSA2048: SIGNATURE_SIZE := sizeof(sig_rsa2048);
    ES_SIG_RSA4096: SIGNATURE_SIZE := sizeof(sig_rsa4096);
    ES_SIG_ECDSA: SIGNATURE_SIZE := sizeof(sig_ecdsa);
  else
    SIGNATURE_SIZE := 0;
  end;
end;

function SIGNATURE_SIG(x: psigned_blob): cint; inline;
begin
  SIGNATURE_SIG := cuint8(x^) + 4;
end;

function IS_VALID_CERT(x: cert_header): boolean; inline;
begin
	IS_VALID_CERT := (x.cert_type = ES_CERT_RSA2048) or
                   (x.cert_type = ES_CERT_RSA4096) or
	                 (x.cert_type = ES_CERT_ECDSA);
end;

function CERTIFICATE_SIZE(x: cert_header): cint; inline;
begin
  case x.cert_type of
	  ES_CERT_RSA2048: CERTIFICATE_SIZE := sizeof(cert_rsa2048);
	  ES_CERT_RSA4096: CERTIFICATE_SIZE := sizeof(cert_rsa4096);
	  ES_CERT_ECDSA: CERTIFICATE_SIZE := sizeof(cert_ecdsa);
  else
    CERTIFICATE_SIZE := 0;
  end;
end;

function SIGNATURE_PAYLOAD(x: psigned_blob): pointer; inline;
begin
  SIGNATURE_PAYLOAD := pointer(x^ + SIGNATURE_SIZE(x));
end;

function SIGNED_TMD_SIZE(x: psigned_blob): cint; inline;
begin
  SIGNED_TMD_SIZE := TMD_SIZE(ptmd(SIGNATURE_PAYLOAD(x))) + SIGNATURE_SIZE(x);
end;

function SIGNED_TIK_SIZE(x: psigned_blob): cint; inline;
begin
  SIGNED_TIK_SIZE := sizeof(tik) + SIGNATURE_SIZE(x);
end;

function SIGNED_CERT_SIZE(x: psigned_blob): cint; inline;
begin
  SIGNED_CERT_SIZE := CERTIFICATE_SIZE(pcert_header(SIGNATURE_PAYLOAD(x))^) + SIGNATURE_SIZE(x);
end;

function STD_SIGNED_TIK_SIZE(): cint; inline;
begin
  STD_SIGNED_TIK_SIZE := sizeof(tik) + sizeof(sig_rsa2048);
end;

function MAX_TMD_SIZE(): cint; inline;
begin
  MAX_TMD_SIZE := sizeof(tmd) + MAX_NUM_TMD_CONTENTS * sizeof(tmd_content);
end;

function MAX_SIGNED_TMD_SIZE(): cint; inline;
begin
  MAX_SIGNED_TMD_SIZE := MAX_TMD_SIZE + sizeof(sig_rsa2048);
end;


{$endif HW_RVL}
{$ENDIF}
