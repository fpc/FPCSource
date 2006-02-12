
{
  Automatically converted by H2Pas 1.0.0 from nzt.h
  The following command line parameters were used:
    -p
    -D
    -l
    jojo.so
    nzt.h
}

{$PACKRECORDS C}

type
  Pnzctx         = pointer;
  PPub1          = pointer;
  PPnzttIdentity = pointer;
  PPnzttPersona  = pointer;
  PPtext         = pointer;
  Pnzstrc        = pointer;

  { DISABLE check_long_lines  }
  { Copyright (c) 1996, 2004, Oracle. All rights reserved.   }
  { Copyright (c) 1996, 2004, Oracle. All rights reserved.   }
  {
   * $Header: nzt.h 10-aug-2004.11:55:06 rchahal Exp $ 
    }
  { 
   * NAME
   *    nzt.h
   * 
   * DESCRIPTION
   *    Toolkit public declarations.
   *    
   * PUBLIC FUNCTIONS
   *    nztwOpenWallet           - Open a wallet based on a WRL and pwd.
   *    nztwCloseWallet          - Close a wallet.
   * +  nztwCreateWallet         - Create a new wallet.
   * +  nztwDestroyWallet        - Destroy an existing wallet.
   *    nztwRetrievePersonaCopy  - Retieve a copy of a particular persona.
   * +  nzteStorePersona         - Store a persona in the wallet.
   *    nzteOpenPersona          - Open a persona.
   *    nzteClosePersona         - Close a persona.
   * +  nzteRemovePersona        - Remove a persona from a wallet.
   * +  nzteCreatePersona        - Create a persona.
   *    nzteDestroyPersona       - Destroy a persona.
   *    nztiStoreTrustedIdentity - Store an identity with associated trust.
   *    nzteRetrieveTrustedIdentCopy - Retrieves a trusted identity from persona
   * +  nzteSetProtection        - Modify the protection set in a persona.
   * +  nzteGetProtection        - Get the protection set in a persona
   *    nztePriKey               - Get the Private Key (X509 Only)
   *    nzteMyCert               - Get the Certificate (X509 only)
   *    nzteX509CreatePersona    - Create a persona given an X509 Certificate.
   * +  nztiRemoveIdentity       - Remove an identity from a persona.
   *    nztiCreateIdentity       - Create an identity.
   *    nztiDuplicateIdentity    - Create a complete copy of an identity.
   *    nztiAbortIdentity        - Discard an unstored identity.
   *    nztidGetIdentityDesc     - Gets Identity Description from Identity.
   *    nztidFreeIdentityDesc    - Frees memory for Identity Desc object.
   *    nztSign                  - Generate an attached signature.
   * +  nztxSignExpansion        - Determine size of signature.
   *    nztVerify                - Verify an attached signature.               
   *    nztValidate              - Validate an identity.                       
   *    nztsd_SignDetached       - Generate a detached signature.
   * +  nztxsd_SignDetachedExpansion - Determine size of detached signature.
   *    nztved_VerifyDetached    - Verify a detached signature.                 
   * +  nztEncrypt               - Symmetric key encryption.   
   * +  nztxEncryptExpansion     - Determine the tdu length for encryption.
   * +  nztDecrypt               - Symmetric key decryption.                   
   * +  nztEnvelope              - Sign then encrypt data for recipient(s).    
   * +  nztDeEnvelope            - Reverse nztEnvelope.                        
   * +  nztKeyedHash             - Generate keyed hash.                        
   * +  nztxKeyedHashExpansion   - Determine size of TDU for keyed hash.
   *    nztHash                  - Generate hash.   
   * +  nztxHashExpansion        - Determine the size of the TDU for a hash.
   *    nztSeedRandom            - See the random number generator.
   *    nztrb_RandomBytes        - Generate a series of random bytes.          
   *    nztrn_RandomNumber       - Generate a random number.                   
   *    nztbbInitBlock           - Initialize a buffer block.                  
   *    nztbbReuseBlock          - Reuse a buffer block.           
   *    nztbbSizeBlock           - Find the size of the buffer block.
   *    nztbbGrowBlock           - Grow initialized buffer block by 'inc' bytes.
   *    nztbbPurgeBlock          - Purge the memory used within a buffer block.
   *    nztbbSetBlock            - Set block to known state.
   *    nztkec_PKEncrypt         - Encrypt data then encrypt key for recipient.
   *    nztkdc_PKDecrypt         - Decrypt PKEncrypt'ed data.                  
   *    nztific_FreeIdentityContent - Free the contents of an identity.
   *    nztifdn                  - Create an identity from a distinguished name
   *    nztcts_CipherSpecToStr   - Converts the Cipher Spec Code To String
   *    nztiae_IsAuthEnabled     - Checks to see if Authentication is Enabled
   *                               in the current Cipher Spec.
   *    nztiae_IsEncrEnabled     - Checks to see if Encryption is Enabled
   *                               in the current Cipher Spec.
   *    nztiae_IsHashEnabled     - Checks to see if Hashing is Enabled
   *                               in the current Cipher Spec.
   *    nztwGetCertInfo          - Get peer certificate info
   *
   * NOTE: the '+' indicates that these functions are UNSUPPORTED at this time.
   * 
   * NOTES
   *    
   * MODIFIED
   *    rchahal    07/27/04 - add keyusage 
   *    srtata     11/10/03 - fix nztSetAppDefaultLocation header 
   *    rchahal    10/15/03 - bug 2513821 
   *    rchahal    11/11/02 - pkcs11 support
   *    akoyfman   07/05/02 - adding secret store to persona
   *    supriya    10/11/01 - Fix for bug # 2015732
   *    ajacobs    04/04/01 - make NZT_REGISTRY_WRL always available
   *    ajacobs    03/06/01 - olint fix
   *    ajacobs    03/02/01 - Add GetCertInfo
   *    supriya    02/23/01 - Move nzttKPUsage from nzt0.h
   *    rchahal    01/26/01 - olint fixes
   *    supriya    12/07/00 - Change fn name
   *    supriya    12/01/00 - Certificate API's needed for iAS
   *    supriya    06/19/00 - Adding definitions for MCS and ENTR
   *    lkethana   05/31/00 - multiple cert support
   *    skanjila   06/25/99 - Remove nztcts_CipherSpecToStr() to NZOS.
   *    skanjila   06/23/99 - Change API of nztcts_CipherSpecToStr.
   *    lkethana   06/18/99 - rem nztIPrivateAlloc, etc
   *    lkethana   06/10/99 - changing size_t to ub4
   *    lkethana   06/02/99 - add api for getting auth/encry/hash capability of c
   *    arswamin   12/28/98 - add NZT_MAX_MD5.
   *    arswamin   12/21/98 - change signature of compareDN
   *    qdinh      12/21/98 - change size_t to ub4.
   *    inetwork   11/22/98 - Removing NZDEPRECATED definition
   *    amthakur   09/14/98 - deprecating and updating the c-structures.
   *    arswamin   09/24/98 - adding NZTTWRL_NULL for SSO support.
   *    amthakur   07/30/98 - changing the prototype of nztGetCertChain.
   *    qdinh      05/01/98 - add NZTTIDENTTYPE_INVALID_TYPE
   *    qdinh      04/17/98 - add NZTTWRL_ORACLE.
   *    ascott     10/08/97 - implement nztiStoreTrustedIdentity
   *    ascott     10/07/97 - add nztiGetIdentityDesc
   *    ascott     09/28/97 - clarify prototype comments and error codes
   *    ascott     09/05/97 - update identity: create, destroy, duplicate
   *    ascott     08/21/97 - add GetCert and GetPriKey
   *    ascott     08/07/97 - add other WRL settings
   *    asriniva   03/25/97 - Add ANSI prototypes
   *    rwessman   03/19/97 - Added prototypes for nztific_FreeIdentityContent()
   *    asriniva   03/11/97 - Fix olint errors
   *    sdange     02/28/97 - Removed inclusion of nz0decl.h
   *    sdange     02/18/97 - Moved nzt specific declarations from nz0decl.h
   *    asriniva   01/21/97 - Remove prototypes.
   *    asriniva   10/31/96 - Include oratypes.h
   *    asriniva   10/15/96 - Declare buffer block helper functions
   *    asriniva   10/08/96 - First pass at wallet open/close
   *    asriniva   10/04/96 - Add random number seed function
   *    asriniva   10/03/96 - Reorder parameters in nztbbSetBlock
   *    asriniva   10/03/96 - Keep editing.
   *    asriniva   10/03/96 - Continued edits.
   *    asriniva   10/02/96 - Continue editing.
   *    asriniva   09/26/96 -
    }
  { ENABLE check_long_lines  }
  { ORATYPES  }
{$include nzerror.inc}         // NZ error type
  { NZERROR_ORACLE  }

  const
     NZT_MAX_SHA1 = 20;     
     NZT_MAX_MD5 = 16;     
  {************************************* }
  { PUBLIC CONSTANTS, MACROS, AND TYPES  }
  {************************************* }
  {
   * Wallet Resource Locator Type Strings
   *
   * WRL TYPE        PARAMETERS      BEHAVIOR
   * ========        ==========      =====================================
   * default:          <none>        Uses directory defined by the parameter
   *                                 SNZD_DEFAULT_FILE_DIRECTORY which in 
   *                                 unix is "$HOME/oracle/oss"
   * 
   * file:            file path      Find the Oracle wallet in this directory.
   *                                 example: file:<dir-path>
   * 
   * sqlnet:           <none>        In this case, the directory path will be 
   *                                 retrieved from the sqlnet.ora file under
   *                                 the oss.source.my_wallet parameter.
   *
   * mcs:              <none>        Microsoft WRL.
   *
   * entr:             dir path      Entrust WRL. eg: ENTR:<dir-path>   
   * 
    }
  { Note that there is no NZT_NULL_WRL.  Instead look in snzd.h for DEFAULT_WRP
   * which is used in our new defaulting mechanism.  The NZT_DEFAULT_WRL
   * should be deprecated.
    }

  const
         NZT_DEFAULT_WRL:Ptext = 'default:';
         NZT_SQLNET_WRL:Ptext ='sqlnet:';
         NZT_FILE_WRL:Ptext='file:';
         NZT_ENTR_WRL:Ptext='entr:';
         NZT_MCS_WRL:Ptext='mcs:';
         NZT_ORACLE_WRL:Ptext='oracle:';
         NZT_REGISTRY_WRL:Ptext='reg:';

  { Default, use SNZD_DEFAULT_FILE_DIRECTORY  }
  { Use oss.source.my_wallet in sqlnet.ora file  }
  { Find the oracle wallet in this directory  }
  { Find the entrust profile in this directory  }
  { WRL for Microsoft  }
  { Get the wallet from OSS db  }
  { New SSO defaulting mechanism  }
  { Find the wallet in Windows Registry  }

  type
     nzttwrl = (NZTTWRL_DEFAULT := 1,NZTTWRL_SQLNET,
       NZTTWRL_FILE,NZTTWRL_ENTR,NZTTWRL_MCS,
       NZTTWRL_ORACLE,NZTTWRL_NULL,NZTTWRL_REGISTRY
       );


     Pnzttwrl = ^nzttwrl;
{$ifndef NZ0DECL_ORACLE}
  {
      * With the elimination of nz0decl.h from public, we need this
      * redundant typedef.
       }

{$endif}
  { NZ0DECL_ORACLE  }
  { Moved from nz0decl.h  }

  type
    PnzttIdentityPrivate = pointer;
    PnzttPersonaPrivate = pointer;
    PnzttWalletPrivate = pointer;
    PnzttWalletObj = pointer;        // For wallet object
    PnzssEntry = pointer;            // For secretstore
    Pnzpkcs11_Info = pointer;

  {
   * Crypto Engine State
   *
   * Once the crypto engine (CE) has been initialized for a particular
   * cipher, it is either at the initial state, or it is continuing to
   * use the cipher.  NZTCES_END is used to change the state back to
   * initialized and flush any remaining output.  NZTTCES_RESET can be
   * used to change the state back to initialized and throw away any
   * remaining output.
    }
  { Continue processing input  }
  { End processing input  }
  { Reset processing and skip generating output  }
     nzttces = (NZTTCES_CONTINUE := 1,NZTTCES_END,NZTTCES_RESET
       );


     Pnzttces = ^nzttces;
  {
   * Crypto Engine Functions
   *
   * List of crypto engine categories; used to index into protection
   * vector.
    }
  { Signature, detached from content  }
  { Signature combined with content  }
  { Signature and encryption with content  }
  { Encryption for one or more recipients  }
  { Symmetric encryption  }
  { Keyed hash/checkusm  }
  { Hash/checsum  }
  { Random byte generation  }
  { Used for array size  }
     nzttcef = (NZTTCEF_DETACHEDSIGNATURE := 1,NZTTCEF_SIGNATURE,
       NZTTCEF_ENVELOPING,NZTTCEF_PKENCRYPTION,
       NZTTCEF_ENCRYPTION,NZTTCEF_KEYEDHASH,
       NZTTCEF_HASH,NZTTCEF_RANDOM,NZTTCEF_LAST
       );


     Pnzttcef = ^nzttcef;
  {
   * State of the persona.
    }
  { is not in any state(senseless???)  }
  { cert-request  }
  { certificate  }
  { certificate  }
  { renewal-requested  }
     nzttState = (NZTTSTATE_EMPTY := 0,NZTTSTATE_REQUESTED,
       NZTTSTATE_READY,NZTTSTATE_INVALID,NZTTSTATE_RENEWAL
       );


     PnzttState = ^nzttState;
  {
   * Cert-version types
   * 
   * This is used to quickly look-up the cert-type
    }
  { X.509v1  }
  { X.509v3  }
{$ifdef NZDEPRECATED}
  { Symmetric  }
{$endif}
  { For Initialization  }

  type
     nzttVersion = (NZTTVERSION_X509v1 := 1,NZTTVERSION_X509v3,
       NZTTVERSION_SYMMETRIC,NZTTVERSION_INVALID_TYPE
       );


     PnzttVersion = ^nzttVersion;
  {
   * Cipher Types
   *
   * List of all cryptographic algorithms, some of which may not be
   * available.
    }
  { RSA public key  }
  { DES  }
  { RC4  }
  { DES encrypted MD5 with salt (PBE)  }
  { RC2 encrypted MD5 with salt (PBE)  }
  { MD5  }
  { SHA  }
     nzttCipherType = (NZTTCIPHERTYPE_RSA := 1,NZTTCIPHERTYPE_DES,
       NZTTCIPHERTYPE_RC4,NZTTCIPHERTYPE_MD5DES,
       NZTTCIPHERTYPE_MD5RC2,NZTTCIPHERTYPE_MD5,
       NZTTCIPHERTYPE_SHA);


     PnzttCipherType = ^nzttCipherType;
  {
   * TDU Formats
   *
   * List of possible toolkit data unit (TDU) formats.  Depending on the
   * function and cipher used some may be not be available.
    }
  { PKCS7 format  }
  { RSA padded format  }
  { Oracle v1 format  }
  { Used for array size  }
     nztttdufmt = (NZTTTDUFMT_PKCS7 := 1,NZTTTDUFMT_RSAPAD,
       NZTTTDUFMT_ORACLEv1,NZTTTDUFMT_LAST
       );


     Pnztttdufmt = ^nztttdufmt;
  {
   * Validate State
   *
   * Possible validation states an identity can be in.
    }
  { Needs to be validated  }
  { Validated  }
  { Failed to validate  }
     nzttValState = (NZTTVALSTATE_NONE := 1,NZTTVALSTATE_GOOD,
       NZTTVALSTATE_REVOKED);


     PnzttValState = ^nzttValState;
  {
   * Policy Fields <----NEW (09/14/98)
   *
   * Policies enforced
    }
  { number of retries for decryption = 1  }
  { number of retries for decryption = 2  }
  { number of retries for decryption = 3  }
     nzttPolicy = (NZTTPOLICY_NONE := 0,NZTTPOLICY_RETRY_1,
       NZTTPOLICY_RETRY_2,NZTTPOLICY_RETRY_3
       );


     PnzttPolicy = ^nzttPolicy;
  {
   * Persona Usage <----NEW (09/14/98)
   *
   * what a persona will be used for?
    }
{ $ifdef NZDEPRECATED_MULTIPLECERTS}
  { persona for SSL usage  }

  type
     nzttUsage = (NZTTUSAGE_NONE := 0,NZTTUSAGE_SSL);


     PnzttUsage = ^nzttUsage;
//     nzttUsage = nzttUsage;
{ $endif}
  {
   * Personas and identities have unique id's that are represented with
   * 128 bits.
    }

  type

     PnzttID = ^nzttID;
     nzttID = ub1;
  {
   * Identity Types
   *
   * List of all Identity types..
    }
     nzttIdentType = (NZTTIDENTITYTYPE_INVALID_TYPE := 0,
       NZTTIDENTITYTYPE_CERTIFICTAE,NZTTIDENTITYTYPE_CERT_REQ,
       NZTTIDENTITYTYPE_RENEW_CERT_REQ,NZTTIDENTITYTYPE_CLEAR_ETP,
       NZTTIDENTITYTYPE_CLEAR_UTP,NZTTIDENTITYTYPE_CLEAR_PTP
       );


     PnzttIdentType = ^nzttIdentType;

     PnzttKPUsage = ^nzttKPUsage;
     nzttKPUsage = ub4;
  { IF new types are added nztiMUS should be changed  }

  const
     NZTTKPUSAGE_NONE = 0;
  { SSL Server  }
     NZTTKPUSAGE_SSL = 1;
     NZTTKPUSAGE_SMIME_ENCR = 2;
     NZTTKPUSAGE_SMIME_SIGN = 4;
     NZTTKPUSAGE_CODE_SIGN = 8;
     NZTTKPUSAGE_CERT_SIGN = 16;
  { SSL Client  }
     NZTTKPUSAGE_SSL_CLIENT = 32;
     NZTTKPUSAGE_INVALID_USE = $ffff;
  {
   * Timestamp as 32 bit quantity in UTC.
    }

  type

     PnzttTStamp = ^nzttTStamp;
     nzttTStamp = ub1;
  {
   * Buffer Block
   *
   * A function that needs to fill (and possibly grow) an output buffer
   * uses an output parameter block to describe each buffer.
   *
   * The flags_nzttBufferBlock member tells the function whether the
   * buffer can be grown or not.  If flags_nzttBufferBlock is 0, then
   * the buffer will be realloc'ed automatically.  
   *
   * The buflen_nzttBufferBLock member is set to the length of the
   * buffer before the function is called and will be the length of the
   * buffer when the function is finished.  If buflen_nzttBufferBlock is
   * 0, then the initial pointer stored in pobj_nzttBufferBlock is
   * ignored.
   *
   * The objlen_nzttBufferBlock member is set to the length of the
   * object stored in the buffer when the function is finished.  If the
   * initial buffer had a non-0 length, then it is possible that the
   * object length is shorter than the buffer length.
   *
   * The pobj_nzttBufferBlock member is a pointer to the output object.
    }
  { # define NZT_NO_AUTO_REALLOC     0x1  }
  { Flags  }
  { Total length of buffer  }
  { Length of used buffer part  }
  { Pointer to buffer  }
     PnzttBufferBlock = ^nzttBufferBlock;
     nzttBufferBlock = record
          flags_nzttBufferBlock : uword;
          buflen_nzttBufferBlock : ub4;
          usedlen_nzttBufferBlock : ub4;
          buffer_nzttBufferBlock : Pub1;
       end;

  {
   * Wallet.
    }
  { user's LDAP Name  }
  { len of user's LDAP Name  }
  { secured-policy of the wallet  }
  { open-policy of the wallet  }
  { List of personas in wallet  }
  { Private wallet information  }
{$ifdef NZDEPRECATED}
  { Number of personas  }
{$endif}

  type
     PnzttPersona = ^nzttPersona;
     PnzttWallet = ^nzttWallet;
     PnzttIdentity = ^nzttIdentity;
     nzttWallet = record
          ldapName_nzttWallet : Pub1;
          ldapNamelen_nzttWallet : ub4;
          securePolicy_nzttWallet : nzttPolicy;
          openPolicy_nzttWallet : nzttPolicy;
          persona_nzttWallet : PnzttPersona;
          private_nzttWallet : PnzttWalletPrivate;
          npersona_nzttWallet : ub4;
       end;

  {
   * The wallet contains, one or more personas.  A persona always
   * contains its private key and its identity.  It may also contain
   * other 3rd party identites.  All identities qualified with trust
   * where the qualifier can indicate anything from untrusted to trusted
   * for specific operations.
    }
  {
   * Persona
   *
   * Structure containing information about a persona.
    }
  { user-friendly persona name   }
  { persona-name length  }
  { Opaque part of persona  }
  { My cert-requests  }
  { My certificates  }
  { List of trusted identities  }
  { List of secrets  }
  { PKCS11 token info  }
  { Next persona  }
{$ifdef NZDEPRECATED_MULTIPLECERTS}
  { As Persona has multiple certs for different
       usages, Persona Usage does not mean anything. Similarly
       each key pair has its own state and Persona state itself
       does not mean anything. - lk 5/31/00
     }
  { persona usage; SSL/SET/..  }
  { persona state-requested/ready  }
  { Num of trusted identities  }
{$endif}

     nzttPersona = record
          genericName_nzttPersona : Pub1;
          genericNamelen_nzttPersona : ub4;
          private_nzttPersona : PnzttPersonaPrivate;
          mycertreqs_nzttPersona : PnzttIdentity;
          mycerts_nzttPersona : PnzttIdentity;
          mytps_nzttPersona : PnzttIdentity;
          mystore_nzttPersona : PnzssEntry;
          mypkcs11Info_nzttPersona : Pnzpkcs11_Info;
          next_nzttPersona : PnzttPersona;
          usage_nzttPersona : nzttUsage;
          state_nzttPersona : nzttState;
          ntps_nzttPersona : ub4;
       end;

  {
   * Identity
   *
   * Structure containing information about an identity.
   *
   * NOTE
   *  -- the next_trustpoint field only applies to trusted identities and
   *     has no meaning (i.e. is NULL) for self identities.
    }
  { Alias  }
  { Length of alias  }
  { Comment   }
  { Length of comment  }
  { Opaque part of identity  }
  { next identity in list  }
     nzttIdentity = record
          dn_nzttIdentity : Ptext;
          dnlen_nzttIdentity : ub4;
          comment_nzttIdentity : Ptext;
          commentlen_nzttIdentity : ub4;
          private_nzttIdentity : PnzttIdentityPrivate;
          next_nzttIdentity : PnzttIdentity;
       end;

     PnzttB64Cert = ^nzttB64Cert;
     nzttB64Cert = record
          b64Cert_nzttB64Cert : Pub1;
          b64Certlen_nzttB64Cert : ub4;
          next_nzttB64Cert : PnzttB64Cert;
       end;

  { Hash cipher  }
  { Symmetric cipher  }
  { Length of key to use  }
     PnzttPKCS7ProtInfo = ^nzttPKCS7ProtInfo;
     nzttPKCS7ProtInfo = record
          mictype_nzttPKCS7ProtInfo : nzttCipherType;
          symmtype_nzttPKCS7ProtInfo : nzttCipherType;
          keylen_nzttPKCS7ProtInfo : ub4;
       end;

  {
   * Protection Information.
   *
   * Information specific to a type of protection.
    }
     PnzttProtInfo = ^nzttProtInfo;
     nzttProtInfo = record
         case longint of
            0 : ( pkcs7_nzttProtInfo : nzttPKCS7ProtInfo );
         end;


  {
   * A description of a persona so that the toolkit can create one.  A
   * persona can be symmetric or asymmetric and both contain an
   * identity.  The identity for an asymmetric persona will be the
   * certificate and the identity for the symmetric persona will be
   * descriptive information about the persona.  In either case, an
   * identity will have been created before the persona is created.
   *
   * A persona can be stored separately from the wallet that references
   * it.  By default, a persona is stored with the wallet (it inherits
   * with WRL used to open the wallet).  If a WRL is specified, then it
   * is used to store the actuall persona and the wallet will have a
   * reference to it.
    }
  { Length of private info (key) }
  { Private information  }
  { Length of PRL  }
  { PRL for storage  }
  { Length of alias  }
  { Alias  }
  { Length of longer description }
  { Longer persona description  }
     PnzttPersonaDesc = ^nzttPersonaDesc;
     nzttPersonaDesc = record
          privlen_nzttPersonaDesc : ub4;
          priv_nzttPersonaDesc : Pub1;
          prllen_nzttPersonaDesc : ub4;
          prl_nzttPersonaDesc : Ptext;
          aliaslen_nzttPersonaDesc : ub4;
          alias_nzttPersonaDesc : Ptext;
          longlen_nzttPersonaDesc : ub4;
          long_nzttPersonaDesc : Ptext;
       end;

  {
   * A description of an identity so that the toolkit can create one.
   * Since an identity can be symmetric or asymmetric, the asymmetric
   * identity information will not be used when a symmetric identity is
   * created.  This means the publen_nzttIdentityDesc and
   * pub_nzttIdentityDesc members will not be used when creating a
   * symmetric identity.
    }
  { Length of identity  }
  { Type specific identity  }
  { Length of alias  }
  { Alias  }
  { Length of longer description  }
  { Longer description  }
  { Length of trust qualifier  }
  { Trust qualifier  }
     PnzttIdentityDesc = ^nzttIdentityDesc;
     nzttIdentityDesc = record
          publen_nzttIdentityDesc : ub4;
          pub_nzttIdentityDesc : Pub1;
          dnlen_nzttIdentityDesc : ub4;
          dn_nzttIdentityDesc : Ptext;
          longlen_nzttIdentityDesc : ub4;
          long_nzttIdentityDesc : Ptext;
          quallen_nzttIdentityDesc : ub4;
          trustqual_nzttIdentityDesc : Ptext;
       end;

  {****************************** }
  { PUBLIC FUNCTION DECLARATIONS  }
  {****************************** }
  {---------------------- nztwOpenWallet ---------------------- }
  {
   * NAME
   *    nztwOpenWallet - Open a wallet based on a wallet Resource Locator (WRL).
   * 
   * PARAMETERS
   *    osscntxt IN     OSS context. 
   *    wrllen   IN     Length of WRL.
   *    wrl      IN     WRL.
   *    pwdlen   IN     Length of password.
   *    pwd      IN     Password.
   *    wallet   IN/OUT Initialized wallet structure.   
   * 
   * NOTES
   *    The syntax for a WRL is <Wallet Type>:<Wallet Type Parameters>.
   *
   *    Wallet Type       Wallet Type Parameters.
   *    -----------       ----------------------
   *    File              Pathname (e.g. "file:/home/asriniva")
   *    Oracle            Connect string (e.g. "oracle:scott/tiger@oss")
   *
   *    There are also defaults.  If the WRL is NZT_DEFAULT_WRL, then
   *    the platform specific WRL default is used.  If only the wallet
   *    type is specified, then the WRL type specific default is used
   *    (e.g. "oracle:")
   *
   *    There is an implication with Oracle that should be stated: An
   *    Oracle based wallet can be implemented in a user's private space
   *    or in world readable space.
   *
   *    When the wallet is opened, the password is verified by hashing
   *    it and comparing against the password hash stored with the
   *    wallet.  The list of personas (and their associated identities)
   *    is built and stored into the wallet structure.
   *    
   * RETURNS
   *    NZERROR_OK           Success.
   *    NZERROR_RIO_OPEN     RIO could not open wallet (see network trace file).
   *    NZERROR_TK_PASSWORD  Password verification failed.
   *    NZERROR_TK_WRLTYPE   WRL type is not known.
   *    NZERROR_TK_WRLPARM   WRL parm does not match type.
    }
{$IFNDEF LinkDynamically}
  function nztwOpenWallet(_para1:Pnzctx; _para2:ub4; _para3:Ptext; _para4:ub4; _para5:Ptext;
             _para6:PnzttWallet):nzerror;cdecl;external ocilib name 'nztwOpenWallet';
{$ELSE}
var nztwOpenWallet : function (_para1:Pnzctx; _para2:ub4; _para3:Ptext; _para4:ub4; _para5:Ptext;
             _para6:PnzttWallet):nzerror;cdecl;
{$ENDIF}


  {---------------------- nztwCloseWallet ---------------------- }
  {
   * NAME
   *    nztwCloseWallet - Close a wallet
   * 
   * PARAMETERS
   *    osscntxt IN     OSS context.
   *    wallet   IN/OUT Wallet.
   * 
   * NOTES
   *    Closing a wallet also closes all personas associated with that
   *    wallet.  It does not cause a persona to automatically be saved
   *    if it has changed.  The implication is that a persona can be
   *    modified by an application but if it is not explicitly saved it
   *    reverts back to what was in the wallet.
   *
   * RETURNS
   *    NZERROR_OK           Success.
   *    NZERROR_RIO_CLOSE    RIO could not close wallet (see network trace file).
    }
{$IFNDEF LinkDynamically}
  function nztwCloseWallet(_para1:Pnzctx; _para2:PnzttWallet):nzerror;cdecl;external ocilib name 'nztwCloseWallet';
{$ELSE}
  nztwCloseWallet : function (_para1:Pnzctx; _para2:PnzttWallet):nzerror;cdecl;
{$ENDIF}
  {--------------------nztwGetCertInfo---------------------------- }
  {***NOTE: This function is a temporary hack.*** }
  {***DO NOT CALL.  It will soon disappear.*** }
  {_ nzctx *nz_context,
                              nzosContext *nzosCtx,
                              nzttWallet *walletRef,
                              void *peerCert _ }(* error 
                            void *peerCert _*/);
 in declarator_list *)
  {------------------------ nztwConstructWallet ----------------------- }
  {
   * 
   * nzerror nztwConstructWallet( nzctx *oss_context,    
   *              nzttPolicy openPolicy,
   *              nzttPolicy securePolicy,
   *              ub1 *ldapName,
   *              ub4 ldapNamelen,
   *              nzstrc *wrl,
   *              nzttPersona *personas,
   *              nzttWallet **wallet );
    }
  {---------------------- nztwRetrievePersonaCopy ---------------------- }
  {
   * NAME
   *    nztwRetrievePersonaCopy - Retrieves a persona based from wallet
   * 
   * PARAMETERS
   *    osscntxt IN     OSS context. 
   *    wallet   IN     Wallet.
   *    index    IN     Which wallet index to remove (first persona is zero).
   *    persona  OUT    Persona found.
   * 
   * NOTES
   *    Retrieves a persona from the wallet based on the index number passed
   *    in.  This persona is a COPY of the one stored in the wallet, therefore
   *    it is perfectly fine for the wallet to be closed after this call is 
   *    made.
   *
   *    The caller is responsible for disposing of the persona when completed.
   *
   * RETURNS
   *    NZERROR_OK           Success.
    }

{$IFNDEF LinkDynamically}
  function nztwRetrievePersonaCopy(_para1:Pnzctx; _para2:PnzttWallet; _para3:ub4; _para4:PPnzttPersona):nzerror;cdecl;external ocilib name 'nztwRetrievePersonaCopy';
{$ELSE}
  nztwRetrievePersonaCopy : function (_para1:Pnzctx; _para2:PnzttWallet; _para3:ub4; _para4:PPnzttPersona):nzerror;cdecl;
{$ENDIF}

  {---------------------- nztwRetrievePersonaCopyByName ---------------------- }
  {
   * NAME
   *    nztwRetrievePersonaCopyByName - Retrieves a persona based on its name.
   *
   * PARAMETERS
   *    osscntxt IN     OSS context.
   *    wallet   IN     Wallet.
   *    name    IN     Name of the persona 
   *    persona  OUT    Persona found.
   *
   * NOTES
   *    Retrieves a persona from the wallet based on the name of the persona. 
   *    This persona is a COPY of the one stored in the wallet, therefore
   *    it is perfectly fine for the wallet to be closed after this call is
   *    made.
   *
   *    The caller is responsible for disposing of the persona when completed.
   *
   * RETURNS
   *    NZERROR_OK           Success.
    }

{$IFNDEF LinkDynamically}
  function nztwRetrievePersonaCopyByName(_para1:Pnzctx; _para2:PnzttWallet; _para3:Pchar; _para4:PPnzttPersona):nzerror;cdecl;external ocilib name 'nztwRetrievePersonaCopyByName';
{$ELSE}
  nztwRetrievePersonaCopyByName : function (_para1:Pnzctx; _para2:PnzttWallet; _para3:Pchar; _para4:PPnzttPersona):nzerror;cdecl;
{$ENDIF}

  {---------------------- nzteOpenPersona ---------------------- }
  {
   * NAME
   *    nzteOpenPersona - Open a persona.
   * 
   * PARAMETERS
   *    osscntxt IN     OSS context. 
   *    persona  IN/OUT Persona.
   * 
   * NOTES
   *    
   * RETURNS
   *    NZERROR_OK           Success.
   *    NZERROR_TK_PASSWORD  Password failed to decrypt persona.
   *    NZERROR_TK_BADPRL    Persona resource locator did not work.
   *    NZERROR_RIO_OPEN     Could not open persona (see network trace file).
    }

{$IFNDEF LinkDynamically}
  function nzteOpenPersona(_para1:Pnzctx; _para2:PnzttPersona):nzerror;cdecl;external ocilib name 'nzteOpenPersona';
{$ELSE}
  nzteOpenPersona : function (_para1:Pnzctx; _para2:PnzttPersona):nzerror;cdecl;
{$ENDIF}

  {--------------------- nzteClosePersona --------------------- }
  {
   * NAME
   *    nzteClosePersona - Close a persona.
   * 
   * PARAMETERS
   *    osscntxt IN     OSS context.
   *    persona  IN/OUT Persona.
   * 
   * NOTES
   *    Closing a persona does not store the persona, it simply releases
   *    the memory associated with the crypto engine.
   *    
   * RETURNS
   *    NZERROR_OK        Success.
    }

{$IFNDEF LinkDynamically}
  function nzteClosePersona(_para1:Pnzctx; _para2:PnzttPersona):nzerror;cdecl;external ocilib name 'nzteClosePersona';
{$ELSE}
  nzteClosePersona : function (_para1:Pnzctx; _para2:PnzttPersona):nzerror;cdecl;
{$ENDIF}

  {--------------------- nzteDestroyPersona --------------------- }
  {
   * NAME
   *    nzteDestroyPersona - Destroy a persona.
   * 
   * PARAMETERS
   *    osscntxt IN        OSS context.
   *    persona  IN/OUT    Persona.
   * 
   * NOTES
   *    The persona is destroyd in the open state, but it will
   *    not be associated with a wallet.
   *    
   *    The persona parameter is doubly indirect so that at the
   *    conclusion of the function, the pointer can be set to NULL.
   *
   *
   * RETURNS
   *    NZERROR_OK        Success.
   *    NZERROR_TK_TYPE   Unsupported itype/ctype combination.
   *    NZERROR_TK_PARMS  Error in persona description.
    }

{$IFNDEF LinkDynamically}
  function nzteDestroyPersona(_para1:Pnzctx; _para2:PPnzttPersona):nzerror;cdecl;external ocilib name 'nzteDestroyPersona';
{$ELSE}
  nzteDestroyPersona : function (_para1:Pnzctx; _para2:PPnzttPersona):nzerror;cdecl;
{$ENDIF}

  {---------------------- nzteRetrieveTrustedIdentCopy ---------------------- }
  {
   * NAME
   *    nzteRetrieveTrustedIdentCopy - Retrieves a trusted identity from persona
   * 
   * PARAMETERS
   *    osscntxt IN     OSS context. 
   *    persona  IN     Persona.
   *    index    IN     Which wallet index to remove (first element is zero).
   *    identity OUT    Trusted Identity from this persona.
   * 
   * NOTES
   *    Retrieves a trusted identity from the persona based on the index 
   *    number passed in.  This identity is a COPY of the one stored in 
   *    the persona, therefore it is perfectly fine to close the persona
   *    after this call is made.
   *
   *    The caller is responsible for freeing the memory of this object 
   *    by calling nztiAbortIdentity it is no longer needed
   *
   * RETURNS
   *    NZERROR_OK           Success.
    }

{$IFNDEF LinkDynamically}
  function nzteRetrieveTrustedIdentCopy(_para1:Pnzctx; _para2:PnzttPersona; _para3:ub4; _para4:PPnzttIdentity):nzerror;cdecl;external ocilib name 'nzteRetrieveTrustedIdentCopy';
{$ELSE}
  nzteRetrieveTrustedIdentCopy : function (_para1:Pnzctx; _para2:PnzttPersona; _para3:ub4; _para4:PPnzttIdentity):nzerror;cdecl;
{$ENDIF}

  {--------------------- nztePriKey --------------------- }
  {
   * NAME
   *    nztePriKey - Get the decrypted Private Key for the Persona
   * 
   * PARAMETERS
   *    osscntxt IN     OSS context.
   *    persona  IN     Persona.
   *    vkey     OUT    Private Key [B_KEY_OBJ]
   *    vkey_len OUT    Private Key Length
   * 
   * NOTES
   *    This funiction will only work for X.509 based persona which contain
   *    a private key.  
   *    A copy of the private key is returned to the caller so that they do not 
   *    have to worry about the key changeing "underneath them".
   *    Memory will be allocated for the vkey and therefore, the CALLER
   *    will be responsible for freeing this memory.
   *
   * RETURNS
   *    NZERROR_OK           Success.
   *    NZERROR_NO_MEMORY    ossctx is null.
   *    NZERROR_TK_BADPRL    Persona resource locator did not work.
    }

{$IFNDEF LinkDynamically}
  function nztePriKey(_para1:Pnzctx; _para2:PnzttPersona; _para3:PPub1; _para4:Pub4):nzerror;cdecl;external ocilib name 'nztePriKey';
{$ELSE}
  nztePriKey : function (_para1:Pnzctx; _para2:PnzttPersona; _para3:PPub1; _para4:Pub4):nzerror;cdecl;
{$ENDIF}

  {--------------------- nzteMyCert --------------------- }
  {
   * NAME
   *    nzteMyCert - Get the X.509 Certificate for a persona
   * 
   * PARAMETERS
   *    osscntxt IN     OSS context.
   *    persona  IN     Persona.
   *    cert     OUT    X.509 Certificate [BER encoded]
   *    cert_len OUT    Certificate length
   * 
   * NOTES
   *    This funiction will only work for X.509 based persona which contain
   *    a certificate for the self identity. 
   *    A copy of the certificate is returned to the caller so that they do not 
   *    have to worry about the certificate changeing "underneath them".
   *    Memory will be allocated for the cert and therefore, the CALLER
   *    will be responsible for freeing this memory.
   *
   * RETURNS
   *    NZERROR_OK           Success.
   *    NZERROR_NO_MEMORY    ossctx is null.
    }

{$IFNDEF LinkDynamically}
  function nzteMyCert(_para1:Pnzctx; _para2:PnzttPersona; _para3:PPub1; _para4:Pub4):nzerror;cdecl;external ocilib name 'nzteMyCert';
{$ELSE}
  nzteMyCert : function (_para1:Pnzctx; _para2:PnzttPersona; _para3:PPub1; _para4:Pub4):nzerror;cdecl;
{$ENDIF}

  {--------------------- nzteX509CreatePersona --------------------- }
  {
   * NAME
   *    nzteX509CreatePersona - Given a BER X.509 cert, create a persona
   * 
   * PARAMETERS
   *    osscntxt IN     OSS context.
   *    cert     IN     X.509 Certificate [BER encoded]
   *    cert_len IN     Certificate length
   *    persona  OUT    Persona.
   * 
   * NOTES
   *    Memory will be allocated for the persona and therefore, the CALLER
   *    will be responsible for freeing this memory.
   *
   * RETURNS
   *    NZERROR_OK           Success.
   *    NZERROR_NO_MEMORY    ossctx is null.
    }

{$IFNDEF LinkDynamically}
  function nzteX509CreatePersona(_para1:Pnzctx; _para2:Pub1; _para3:ub4; _para4:PPnzttPersona):nzerror;cdecl;external ocilib name 'nzteX509CreatePersona';
{$ELSE}
  nzteX509CreatePersona : function (_para1:Pnzctx; _para2:Pub1; _para3:ub4; _para4:PPnzttPersona):nzerror;cdecl;
{$ENDIF}

  {-------------------- nztiCreateIdentity -------------------- }
  {
   * NAME
   *    nztiCreateIdentity - Create an identity.
   * 
   * PARAMETERS
   *    osscntxt IN     OSS context.
   *    itype    IN     Identity type.
   *    desc     IN     Description of identity.
   *    identity IN/OUT Identity.
   * 
   * NOTES
   *    Memory is only allocated for the identity structure.  The elements in
   *    the description struct are not copied.  Rather their pointers are copied
   *    into the identity structure.  Therefore, the caller should not free
   *    the elements referenced by the desc.  These elements will be freed
   *    when the nztiDestroyIdentity is called.
   *    
   * RETURNS
   *    NZERROR_OK        Success.
   *    NZERROR_PARMS     Error in description.
    }

{$IFNDEF LinkDynamically}
  function nztiCreateIdentity(_para1:Pnzctx; _para2:nzttVersion; _para3:PnzttIdentityDesc; _para4:PPnzttIdentity):nzerror;cdecl;external ocilib name 'nztiCreateIdentity';
{$ELSE}
  nztiCreateIdentity : function (_para1:Pnzctx; _para2:nzttVersion; _para3:PnzttIdentityDesc; _para4:PPnzttIdentity):nzerror;cdecl;
{$ENDIF}

{$ifdef NZ_OLD_TOOLS}
  {-------------------- nztiDuplicateIdentity -------------------- }
  {
   * NAME
   *    nztiDuplicateIdentity - Duplicate an identity.
   * 
   * PARAMETERS
   *    osscntxt     IN     OSS context.
   *    identity     IN     Target Identity.
   *    new_identity IN     New Identity.
   * 
   * NOTES
   *    Memory for the identity is allocated inside the function, and all
   *    internal identity elements as well.
   *    
   * RETURNS
   *    NZERROR_OK        Success.
   *    NZERROR_TK_NOTFOUND  Identity not found.
   *    NZERROR_PARMS     Error in description.
    }

{$IFNDEF LinkDynamically}
  function nztiDuplicateIdentity(_para1:Pnzctx; _para2:PnzttIdentity; _para3:PPnzttIdentity):nzerror;cdecl;external ocilib name 'nztiDuplicateIdentity';
{$ELSE}
  nztiDuplicateIdentity : function (_para1:Pnzctx; _para2:PnzttIdentity; _para3:PPnzttIdentity):nzerror;cdecl;
{$ENDIF}

{$endif}
  { NZ_OLD_TOOLS  }
  {--------------------- nztiAbortIdentity --------------------- }
  {
   * NAME
   *    nztiAbortIdentity - Abort an unassociated identity.
   * 
   * PARAMETERS
   *    osscntxt IN     OSS context.
   *    identity IN/OUT Identity.
   * 
   * NOTES
   *    It is an error to try to abort an identity that can be
   *    referenced through a persona.
   *    
   *    The identity pointer is set to NULL at the conclusion.
   * 
   * RETURNS
   *    NZERROR_OK           Success.
   *    NZERROR_CANTABORT    Identity is associated with persona.
    }

{$IFNDEF LinkDynamically}
  function nztiAbortIdentity(_para1:Pnzctx; _para2:PPnzttIdentity):nzerror;cdecl;external ocilib name 'nztiAbortIdentity';
{$ELSE}
  nztiAbortIdentity : function (_para1:Pnzctx; _para2:PPnzttIdentity):nzerror;cdecl;
{$ENDIF}

{$ifdef NZ_OLD_TOOLS}
  {----------------- nztidGetIdentityDesc ----------------- }
  {
   * NAME
   *    nztidGetIdentityDesc - Gets an Identity Description from the identity
   * 
   * PARAMETERS
   *    osscntxt    IN     Success.
   *    identity    IN     Identity.
   *    description IN/OUT Identity Description.
   * 
   * NOTES
   *    Memory is allocated for the Identity Description. It
   *    is the callers responsibility to free this memory by calling 
   *    nztiFreeIdentityDesc.
   *
   * RETURNS
   *    NZERROR_OK     Success.
    }

{$IFNDEF LinkDynamically}
  function nztidGetIdentityDesc(_para1:Pnzctx; _para2:PnzttIdentity; _para3:PPnzttIdentityDesc):nzerror;cdecl;external ocilib name 'nztidGetIdentityDesc';
{$ELSE}
  nztidGetIdentityDesc : function (_para1:Pnzctx; _para2:PnzttIdentity; _para3:PPnzttIdentityDesc):nzerror;cdecl;
{$ENDIF}

  {----------------- nztidFreeIdentityDesc ----------------- }
  {
   * NAME
   *    nztidFreeIdentityDesc - Frees memory for Identity Desc object.
   * 
   * PARAMETERS
   *    osscntxt    IN     oss context.
   *    description IN/OUT Identity Description.
   * 
   * NOTES
   *    Memory is freed for all Identity description elements.  Pointer is 
   *    then set to null.
   *
   * RETURNS
   *    NZERROR_OK     Success.
    }

{$IFNDEF LinkDynamically}
  function nztidFreeIdentityDesc(_para1:Pnzctx; _para2:PPnzttIdentityDesc):nzerror;cdecl;external ocilib name 'nztidFreeIdentityDesc';
{$ELSE}
  nztidFreeIdentityDesc : fucntion (_para1:Pnzctx; _para2:PPnzttIdentityDesc):nzerror;cdecl;
{$ENDIF}

{$endif}
  { NZ_OLD_TOOLS  }
  {---------------- nztific_FreeIdentityContent ---------------- }
  {
   * NAME
   *     nztific_FreeIdentityContent - free the contents of an identity.
   * 
   * PARAMETERS
   *    osscntxt         IN     OSS context.
   *    identity         IN/OUT freed identity
   * 
   * NOTES
   *    Free a created identity.
   *
   * RETURNS
   *    NZERROR_OK     Success.
    }
  {
   * Free the identity content.
    }

{$IFNDEF LinkDynamically}
  function nztific_FreeIdentityContent(ossctx:Pnzctx; identity:PnzttIdentity):nzerror;cdecl;external ocilib name 'nztific_FreeIdentityContent';
{$ELSE}
  nztific_FreeIdentityContent : function (ossctx:Pnzctx; identity:PnzttIdentity):nzerror;cdecl;
{$ENDIF}

  {-------------------------- nztSign -------------------------- }
  {
   * NAME
   *    nztSign - Create an attached signature.
   * 
   * PARAMETERS
   *    osscntxt IN     OSS context.
   *    persona  IN     Open persona acting as signer.
   *    state    IN     State of signature.
   *    inlen    IN     Length of this input part.
   *    in       IN     This input part.
   *    tdubuf   IN/OUT TDU buffer.
   * 
   * NOTES
   *    
   * RETURNS
   *    NZERROR_OK           Success.
   *    NZERROR_TK_CANTGROW  Needed to grow output buffer but could not.
   *    NZERROR_TK_NOTOPEN   Persona is not open.
   *    NZERROR_TK_NOTSUPP   Function not supported with persona.
    }

{$IFNDEF LinkDynamically}
  function nztSign(_para1:Pnzctx; _para2:PnzttPersona; _para3:nzttces; _para4:ub4; _para5:Pub1;
             _para6:PnzttBufferBlock):nzerror;cdecl;external ocilib name 'nztSign';
{$ELSE}
  nztSign : function(_para1:Pnzctx; _para2:PnzttPersona; _para3:nzttces; _para4:ub4; _para5:Pub1;
             _para6:PnzttBufferBlock):nzerror;cdecl;
{$ENDIF}

  {------------------------- nztVerify ------------------------- }
  {
   * NAME
   *    nztVerify - Verify an attached signature.
   * 
   * PARAMETERS
   *    osscntxt IN     OSS context.
   *    persona  IN     Persona.
   *    state    IN     State of verification.
   *    intdulen IN     TDU length.
   *    intdu    IN     TDU.
   *    out      IN/OUT Extracted message.
   *    verified OUT    TRUE if signature verified.
   *    validatedOUT    TRUE if signing identity validated.
   *    identity OUT Identity of signing party.
   * 
   * NOTES
   *    
   * RETURNS
   *    NZERROR_OK           Success.
   *    NZERROR_TK_CANTGROW  Needed to grow outptu buffer but could not.
   *    NZERROR_TK_NOTOPEN   Persona is not open.
   *    NZERROR_TK_NOTSUPP   Function not supported with persona.
    }

{$IFNDEF LinkDynamically}
  function nztVerify(_para1:Pnzctx; _para2:PnzttPersona; _para3:nzttces; _para4:ub4; _para5:Pub1;
             _para6:PnzttBufferBlock; _para7:Pboolean; _para8:Pboolean; _para9:PPnzttIdentity):nzerror;cdecl;external ocilib name 'nztVerify';
{$ELSE}
  nztVerify : function (_para1:Pnzctx; _para2:PnzttPersona; _para3:nzttces; _para4:ub4; _para5:Pub1;
             _para6:PnzttBufferBlock; _para7:Pboolean; _para8:Pboolean; _para9:PPnzttIdentity):nzerror;cdecl;
{$ENDIF}

  {------------------------ nztValidate ------------------------ }
  {
   * NAME
   *    nztValidate - Validate an identity.
   * 
   * PARAMETERS
   *    osscntxt IN  OSS context.
   *    persona  IN  Persona.
   *    identity IN  Identity.
   *    validatedOUT TRUE if identity was validated.  
   * 
   * NOTES
   *    
   * RETURNS
   *    NZERROR_OK           Success.
   *    NZERROR_TK_NOTOPEN   Persona is not open.
   *    NZERROR_TK_NOTSUPP   Function not supported with persona.
    }

{$IFNDEF LinkDynamically}
  function nztValidate(_para1:Pnzctx; _para2:PnzttPersona; _para3:PnzttIdentity; _para4:Pboolean):nzerror;cdecl;external ocilib name 'nztValidate';
{$ELSE}
  nztValidate : function (_para1:Pnzctx; _para2:PnzttPersona; _para3:PnzttIdentity; _para4:Pboolean):nzerror;cdecl;
{$ENDIF}

  {-------------------- nztsd_SignDetached -------------------- }
  {
   * NAME
   *    nztsd_SignDetached - Generate a detached signature.
   * 
   * PARAMETERS
   *    osscntxt IN     OSS context.
   *    persona  IN     Persona.
   *    state    IN     State of signature.
   *    inlen    IN     Length of this input part.
   *    in       IN     This input part.
   *    tdubuf   IN/OUT TDU buffer.
   * 
   * NOTES
   *    
   * RETURNS
   *    NZERROR_OK           Success.
   *    NZERROR_TK_CANTGROW  Needed to grow output buffer but could not.
   *    NZERROR_TK_NOTOPEN   Persona is not open.
   *    NZERROR_TK_NOTSUPP   Function not supported with persona.
    }

{$IFNDEF LinkDynamically}
  function nztsd_SignDetached(_para1:Pnzctx; _para2:PnzttPersona; _para3:nzttces; _para4:ub4; _para5:Pub1;
             _para6:PnzttBufferBlock):nzerror;cdecl;external ocilib name 'nztsd_SignDetached';
{$ELSE}
  nztsd_SignDetached : function (_para1:Pnzctx; _para2:PnzttPersona; _para3:nzttces; _para4:ub4; _para5:Pub1;
             _para6:PnzttBufferBlock):nzerror;cdecl;
{$ENDIF}

  {------------------- nztved_VerifyDetached ------------------- }
  {
   * NAME
   *    nztved_VerifyDetached - Verify a detached signature.
   * 
   * PARAMETERS
   *    osscntxt IN     OSS context.
   *    persona  IN     Persona.
   *    state    IN     State of verification.
   *    inlen    IN     Length of data.
   *    in       IN     Data.
   *    intdulen IN     Input TDU length.
   *    tdu      IN     Input TDU.
   *    verified OUT    TRUE if signature verified.
   *    validatedOUT    TRUE if signing identity validated.
   *    identity OUT Identity of signing party.
   * 
   * NOTES
   *    
   * RETURNS
   *    NZERROR_OK           Success.
   *    NZERROR_TK_NOTOPEN   Persona is not open.
   *    NZERROR_TK_NOTSUPP   Function not supported with persona.
    }

{$IFNDEF LinkDynamically}
  function nztved_VerifyDetached(_para1:Pnzctx; _para2:PnzttPersona; _para3:nzttces; _para4:ub4; _para5:Pub1;
             _para6:ub4; _para7:Pub1; _para8:Pboolean; _para9:Pboolean; _para10:PPnzttIdentity):nzerror;cdecl;external ocilib name 'nztved_VerifyDetached';
{$ELSE}
  nztved_VerifyDetached : function (_para1:Pnzctx; _para2:PnzttPersona; _para3:nzttces; _para4:ub4; _para5:Pub1;
             _para6:ub4; _para7:Pub1; _para8:Pboolean; _para9:Pboolean; _para10:PPnzttIdentity):nzerror;cdecl;
{$ENDIF}

  {--------------------- nztkec_PKEncrypt --------------------- }
  {
   * NAME
   *    nztkec_PKEncrypt - Encrypt data symmetrically, encrypt key asymmetrically
   * 
   * PARAMETERS
   *    osscntxt    IN     OSS context.
   *    persona     IN     Persona.
   *    nrecipients IN     Number of recipients for this encryption.
   *    recipients  IN     List of recipients.
   *    state       IN     State of encryption.
   *    inlen       IN     Length of this input part.
   *    in          IN     This input part.
   *    tdubuf      IN/OUT TDU buffer.
   * 
   * NOTES
   *    There is a limitation of 1 recipient (nrecipients = 1) at this
   *    time.
   *    
   * RETURNS
   *    NZERROR_OK           Success.
   *    NZERROR_TK_CANTGROW  Needed to grow output buffer but could not.
   *    NZERROR_TK_NOTOPEN   Persona is not open.
   *    NZERROR_TK_NOTSUPP   Function not supported with persona.
    }

{$IFNDEF LinkDynamically}
  function nztkec_PKEncrypt(_para1:Pnzctx; _para2:PnzttPersona; _para3:ub4; _para4:PnzttIdentity; _para5:nzttces;
             _para6:ub4; _para7:Pub1; _para8:PnzttBufferBlock):nzerror;cdecl;external ocilib name 'nztkec_PKEncrypt';
{$ELSE}
  nztkec_PKEncrypt : function (_para1:Pnzctx; _para2:PnzttPersona; _para3:ub4; _para4:PnzttIdentity; _para5:nzttces;
             _para6:ub4; _para7:Pub1; _para8:PnzttBufferBlock):nzerror;cdecl;
{$ENDIF}

  {---------------- nztxkec_PKEncryptExpansion ---------------- }
  {
   * NAME
   *    nztxkec_PKEncryptExpansion - Determine the buffer needed for PKEncrypt
   * 
   * PARAMETERS
   *    osscntxt    IN     OSS context. 
   *    persona     IN     Persona.
   *    nrecipients IN     Number of recipients.
   *    inlen       IN     Length of input.
   *    tdulen      out    Length of buffer need.
   * 
   * NOTES
   *    
   * RETURNS
   *    NZERROR_OK           Success.
   *    NZERROR_TK_NOTOPEN   Persona is not open.
   *    NZERROR_TK_NOTSUPP   Function not supported with persona.
    }

{$IFNDEF LinkDynamically}
  function nztxkec_PKEncryptExpansion(_para1:Pnzctx; _para2:PnzttPersona; _para3:ub4; _para4:ub4; _para5:Pub4):nzerror;cdecl;external ocilib name 'nztxkec_PKEncryptExpansion';
{$ELSE}
  nztxkec_PKEncryptExpansion : function (_para1:Pnzctx; _para2:PnzttPersona; _para3:ub4; _para4:ub4; _para5:Pub4):nzerror;cdecl;
{$ENDIF}

  {--------------------- nztkdc_PKDecrypt --------------------- }
  {
   * NAME
   *    nztkdc_PKDecrypt - Decrypt a PKEncrypted message.
   * 
   * PARAMETERS
   *    osscntxt    IN     OSS context.
   *    persona     IN     Persona.
   *    state       IN     State of encryption.
   *    inlen       IN     Length of this input part.
   *    in          IN     This input part.
   *    tdubuf      IN/OUT TDU buffer.
   * 
   * NOTES
   *    
   * RETURNS
   *    NZERROR_OK           Success.
   *    NZERROR_TK_CANTGROW  Needed to grow output buffer but could not.
   *    NZERROR_TK_NOTOPEN   Persona is not open.
   *    NZERROR_TK_NOTSUPP   Function not supported with persona.
    }

{$IFNDEF LinkDynamically}
  function nztkdc_PKDecrypt(_para1:Pnzctx; _para2:PnzttPersona; _para3:nzttces; _para4:ub4; _para5:Pub1;
             _para6:PnzttBufferBlock):nzerror;cdecl;external ocilib name 'nztkdc_PKDecrypt';
{$ELSE}
  nztkdc_PKDecrypt : function (_para1:Pnzctx; _para2:PnzttPersona; _para3:nzttces; _para4:ub4; _para5:Pub1;
             _para6:PnzttBufferBlock):nzerror;cdecl;
{$ENDIF}

  {-------------------------- nztHash -------------------------- }
  {
   * NAME
   *    nztHash - Generate a hash.
   * 
   * PARAMETERS
   *    osscntxt IN     OSS context.
   *    persona  IN     Persona.
   *    state    IN     State of hash.
   *    inlen    IN     Length of this input.
   *    in       IN     This input.
   *    tdu      IN/OUT Output tdu.
   * 
   * NOTES
   *    
   * RETURNS
   *    NZERROR_OK           Success.
   *    NZERROR_TK_CANTGROW  Needed to grow TDU buffer but could not.
   *    NZERROR_TK_NOTOPEN   Persona is not open.
   *    NZERROR_TK_NOTSUPP   Function not supported with persona.
    }

{$IFNDEF LinkDynamically}
  function nztHash(_para1:Pnzctx; _para2:PnzttPersona; _para3:nzttces; _para4:ub4; _para5:Pub1;
             _para6:PnzttBufferBlock):nzerror;cdecl;external ocilib name 'nztHash';
{$ELSE}
  nztHash : function (_para1:Pnzctx; _para2:PnzttPersona; _para3:nzttces; _para4:ub4; _para5:Pub1;
             _para6:PnzttBufferBlock):nzerror;cdecl;
{$ENDIF}

  {----------------------- nztSeedRandom ----------------------- }
  {
   * NAME
   *    nztSeedRandom - Seed the random function
   * 
   * PARAMETERS
   *    osscntxt IN  OSS context.
   *    persona  IN  Persona.
   *    seedlen  IN  Length of seed.
   *    seed     IN  Seed.
   * 
   * NOTES
   *    
   * RETURNS
   *    NZERROR_OK           Success.
   *    NZERROR_TK_NOTOPEN   Persona is not open.
   *    NZERROR_TK_NOTSUPP   Function not supported with persona.
    }

{$IFNDEF LinkDynamically}
  function nztSeedRandom(_para1:Pnzctx; _para2:PnzttPersona; _para3:ub4; _para4:Pub1):nzerror;cdecl;external ocilib name 'nztSeedRandom';
{$ELSE}
  nztSeedRandom : function (_para1:Pnzctx; _para2:PnzttPersona; _para3:ub4; _para4:Pub1):nzerror;cdecl;
{$ENDIF}

  {--------------------- nztrb_RandomBytes --------------------- }
  {
   * NAME
   *    nztrb_RandomBytes - Generate a buffer random bytes.
   * 
   * PARAMETERS
   *    osscntxt IN     OSS context.
   *    persona  IN     Persona.
   *    nbytes   IN     Number of bytes desired.
   *    out      IN/OUT Buffer block for bytes.
   * 
   * NOTES
   *    
   * RETURNS
   *    NZERROR_OK           Success.
   *    NZERROR_TK_CANTGROW  Needed to grow TDU buffer but could not.
   *    NZERROR_TK_NOTOPEN   Persona is not open.
   *    NZERROR_TK_NOTSUPP   Function not supported with persona.
    }

{$IFNDEF LinkDynamically}
  function nztrb_RandomBytes(_para1:Pnzctx; _para2:PnzttPersona; _para3:ub4; _para4:PnzttBufferBlock):nzerror;cdecl;external ocilib name 'nztrb_RandomBytes';
{$ELSE}
  nztrb_RandomBytes : function (_para1:Pnzctx; _para2:PnzttPersona; _para3:ub4; _para4:PnzttBufferBlock):nzerror;cdecl;
{$ENDIF}

  {-------------------- nztrn_RandomNumber -------------------- }
  {
   * NAME
   *    nztrn_RandomNumber - Generate a random number
   * 
   * PARAMETERS
   *    osscntxt IN     OSS context.
   *    persona  IN     Persona.
   *    num      OUT    Number.
   * 
   * NOTES
   *    
   * RETURNS
   *    NZERROR_OK           Success.
   *    NZERROR_TK_NOTOPEN   Persona is not open.
   *    NZERROR_TK_NOTSUPP   Function not supported with persona.
    }

{$IFNDEF LinkDynamically}
  function nztrn_RandomNumber(_para1:Pnzctx; _para2:PnzttPersona; _para3:Puword):nzerror;cdecl;external ocilib name 'nztrn_RandomNumber';
{$ELSE}
  nztrn_RandomNumber : function (_para1:Pnzctx; _para2:PnzttPersona; _para3:Puword):nzerror;cdecl;
{$ENDIF}

  {---------------------- nztbbInitBlock ---------------------- }
  {
   * NAME
   *    nztbbInitBlock - Initialize a buffer block.
   * 
   * PARAMETERS
   *    osscntxt IN     OSS context.
   *    block    IN/OUT Buffer block.
   * 
   * NOTES
   *    The buffer block is initialized to be empty (all members are set
   *    to 0/NULL).  Such a block will be allocated memory as needed.
   *    
   * RETURNS
   *    NZERROR_OK        Success.
    }

{$IFNDEF LinkDynamically}
  function nztbbInitBlock(_para1:Pnzctx; _para2:PnzttBufferBlock):nzerror;cdecl;external ocilib name 'nztbbInitBlock';
{$ELSE}
  nztbbInitBlock : function (_para1:Pnzctx; _para2:PnzttBufferBlock):nzerror;cdecl;
{$ENDIF}

  {---------------------- nztbbReuseBlock ---------------------- }
  {
   * NAME
   *    nztbbReuseBlock - Reuse an already initialized and possibly used block.
   * 
   * PARAMETERS
   *    osscntxt IN     OSS context.
   *    block    IN/OUT Buffer block.
   * 
   * NOTES
   *    This function simply sets the used length member of the buffer
   *    block to 0.  If the block already has memory allocated to it,
   *    this will cause it to be reused.
   *    
   * RETURNS
   *    NZERROR_OK        Success.
    }

{$IFNDEF LinkDynamically}
  function nztbbReuseBlock(_para1:Pnzctx; _para2:PnzttBufferBlock):nzerror;cdecl;external ocilib name 'nztbbReuseBlock';
{$ELSE}
  nztbbReuseBlock : function (_para1:Pnzctx; _para2:PnzttBufferBlock):nzerror;cdecl;
{$ENDIF}

  {---------------------- nztbbSizeBlock ---------------------- }
  {
   * NAME
   *    nztbbSizeBlock - Size an initialized block to a particular size.
   * 
   * PARAMETERS
   *    osscntxt IN     OSS context.
   *    len      IN     Minimum number of unused bytes desired.
   *    block    IN/OUT Buffer block.
   *
   * NOTES
   *    
   * RETURNS
   *    NZERROR_OK        Success.
    }

{$IFNDEF LinkDynamically}
  function nztbbSizeBlock(_para1:Pnzctx; _para2:ub4; _para3:PnzttBufferBlock):nzerror;cdecl;external ocilib name 'nztbbSizeBlock';
{$ELSE}
  nztbbSizeBlock : function (_para1:Pnzctx; _para2:ub4; _para3:PnzttBufferBlock):nzerror;cdecl;
{$ENDIF}

  {----------------------- nztbbGrowBlock ----------------------- }
  {
   * NAME
   *    nzbbGrowBlock - Increase the size of the buffer block.
   * 
   * PARAMETERS
   *    osscntxt IN     OSS context.
   *    inc      IN     Number of bytes to increase.
   *    block    IN/OUT Buffer block.
   *
   * NOTES
   *    
   * RETURNS
   *    NZERROR_OK        Success.
    }

{$IFNDEF LinkDynamically}
  function nztbbGrowBlock(_para1:Pnzctx; _para2:ub4; _para3:PnzttBufferBlock):nzerror;cdecl;external ocilib name 'nztbbGrowBlock';
{$ELSE}
  nztbbGrowBlock : function (_para1:Pnzctx; _para2:ub4; _para3:PnzttBufferBlock):nzerror;cdecl;
{$ENDIF}

  {---------------------- nztbbPurgeBlock ---------------------- }
  {
   * NAME
   *    nztbbPurgeBlock - Purge a buffer block of its memory.
   * 
   * PARAMETERS
   *    osscntxt IN     OSS context.
   *    block    IN/OUT Buffer block.
   * 
   * NOTES
   *    The memory used by the buffer block as the buffer is released.
   *    The buffer block itself is not affected.
   *    
   * RETURNS
   *    NZERROR_OK        Success.
    }

{$IFNDEF LinkDynamically}
  function nztbbPurgeBlock(_para1:Pnzctx; _para2:PnzttBufferBlock):nzerror;cdecl;external ocilib name 'nztbbPurgeBlock';
{$ELSE}
  nztbbPurgeBlock : function (_para1:Pnzctx; _para2:PnzttBufferBlock):nzerror;cdecl;
{$ENDIF}

  {----------------------- nztbbSetBlock ----------------------- }
  {
   * NAME
   *    nztbbSetBlock - Set a buffer block to a known state.
   * 
   * PARAMETERS
   *    osscntxt IN     OSS context.
   *    flags    IN     Flags to set.
   *    buflen   IN     Length of buffer.
   *    usedlen  IN     Used length.
   *    buffer   IN     Buffer.
   *    block    IN/OUT Buffer block
   * 
   * NOTES
   *    If buflen > 0, objlen == 0, and obj == NULL, then buflen bytes
   *    of memory is allocated and a pointer is stored in the buffer
   *    block.
   *
   *    The buffer parameter remains unchanged.
   *    
   * RETURNS
   *    NZERROR_OK        Success.
    }

{$IFNDEF LinkDynamically}
  function nztbbSetBlock(_para1:Pnzctx; _para2:uword; _para3:ub4; _para4:ub4; _para5:Pub1;
             _para6:PnzttBufferBlock):nzerror;cdecl;external ocilib name 'nztbbSetBlock';
{$ELSE}
  nztbbSetBlock : function (_para1:Pnzctx; _para2:uword; _para3:ub4; _para4:ub4; _para5:Pub1;
             _para6:PnzttBufferBlock):nzerror;cdecl;
{$ENDIF}

  {--------------------- nztiGetSecInfo --------------------- }
  {
   * NAME
   *    nztiGetSecInfo - Get some security information for SSL
   * 
   * PARAMETERS
   *    Name   IN/OUT   Description
   *       osscntxt IN        OSS context.
   *       persona  IN      persona
   *       dname    OUT       distinguished name of the certificate
   *       dnamelen OUT       length of the distinguished name 
   *       issuername OUT     issuer name of the certificate
   *       certhash OUT      SHA1 hash of the certificate
   *   certhashlenOUT   length of the hash
   * NOTES
   *    This function allocate memories for issuername, certhash, and dname.
   *   To deallocate memory for those params, you should call nztdbuf_DestroyBuf.
   * RETURNS
   *    
    }

{$IFNDEF LinkDynamically}
  function nztiGetSecInfo(_para1:Pnzctx; _para2:PnzttPersona; _para3:PPtext; _para4:Pub4; _para5:PPtext;
             _para6:Pub4; _para7:PPub1; _para8:Pub4):nzerror;cdecl;external ocilib name 'nztiGetSecInfo';
{$ELSE}
  nztiGetSecInfo : function (_para1:Pnzctx; _para2:PnzttPersona; _para3:PPtext; _para4:Pub4; _para5:PPtext;
             _para6:Pub4; _para7:PPub1; _para8:Pub4):nzerror;cdecl;
{$ENDIF}

  {---------------------- nztiGetDName ---------------------- }
  {
   * NAME
   *    nztiGetDName - Get the distinguished name for the given identity
   * 
   * PARAMETERS
   *    Name   IN/OUT   Description
   *       osscntxt IN        OSS context.
   *   identity IN      identity need to get dname from
   *   dn   OUT      distinguished name
   *   dnlen   OUT      length of the dname
   *
   * NOTES
   *    
   * RETURNS
   *    
    }

{$IFNDEF LinkDynamically}
  function nztiGetDName(_para1:Pnzctx; _para2:PnzttIdentity; _para3:PPtext; _para4:Pub4):nzerror;cdecl;external ocilib name 'nztiGetDName';
{$ELSE}
  nztiGetDName : function (_para1:Pnzctx; _para2:PnzttIdentity; _para3:PPtext; _para4:Pub4):nzerror;cdecl;
{$ENDIF}

  {------------------- nztiGetIssuerName ------------------- }
  {
   * NAME
   *    nztiGetIssuerName -  Get IssuerName for the given identity 
   * 
   * PARAMETERS
   *    Name   IN/OUT   Description
   *       osscntxt IN        OSS context.
   *   identity IN      identity need to get issuername from
   *   issuername OUT          issuer's name
   *   issuernamelen OUT   length of the issuer's name
   * 
   * NOTES
   *    
   * RETURNS
   *    
    }

{$IFNDEF LinkDynamically}
  function nztiGetIssuerName(_para1:Pnzctx; _para2:PnzttIdentity; _para3:PPtext; _para4:Pub4):nzerror;cdecl;external ocilib name 'nztiGetIssuerName';
{$ELSE}
  nztiGetIssuerName : function (_para1:Pnzctx; _para2:PnzttIdentity; _para3:PPtext; _para4:Pub4):nzerror;cdecl;
{$ENDIF}

  {-------------------- nztgch_GetCertHash -------------------- }
  {
   * NAME
   *    nztgch_GetCertHash -  Get SHA1 hash for the certificate of the identity 
   * 
   * PARAMETERS
   *    Name   IN/OUT   Description
   *       osscntxt IN        OSS context.
   *   identity IN      identity need to get issuername from
   *   certHash OUT          certHash buffer 
   *   hashLen OUT      length of the certHash 
   * 
   * NOTES
   *    Need to call nztdbuf_DestroyBuf to deallocate memory for certHash.   
   * RETURNS
   *    
    }

{$IFNDEF LinkDynamically}
  function nztgch_GetCertHash(_para1:Pnzctx; _para2:PnzttIdentity; _para3:PPub1; _para4:Pub4):nzerror;cdecl;external ocilib name 'nztgch_GetCertHash';
{$ELSE}
  nztgch_GetCertHash : function (_para1:Pnzctx; _para2:PnzttIdentity; _para3:PPub1; _para4:Pub4):nzerror;cdecl;
{$ENDIF}

  {-------------------- nztdbuf_DestroyBuf -------------------- }
  {
   * NAME
   *    nztdbuf_DestroyBuf - Deallocation funtions for ub1 and text buffer 
   * 
   * PARAMETERS
   *    Name   IN/OUT   Description
   *       osscntxt IN        OSS context.
   *   buf      IN      Allocated buffer to be destroyed.
   * 
   * NOTES
   *    
   * RETURNS
   *    
    }

{$IFNDEF LinkDynamically}
  function nztdbuf_DestroyBuf(_para1:Pnzctx; _para2:PPdvoid):nzerror;cdecl;external ocilib name 'nztdbuf_DestroyBuf';
{$ELSE}
  nztdbuf_DestroyBuf : function (_para1:Pnzctx; _para2:PPdvoid):nzerror;cdecl;
{$ENDIF}

  {----------------------- nztGetCertChain ----------------------- }
  {
   * NAME
   *    nztGetCertChain - 
   * 
   * PARAMETERS
   *    Name   IN/OUT   Description
   *       osscntxt IN        OSS context.
   * 
   * NOTES
   *    
   * RETURNS
   *    
    }

{$IFNDEF LinkDynamically}
  function nztGetCertChain(_para1:Pnzctx; _para2:PnzttWallet):nzerror;cdecl;external ocilib name 'nztGetCertChain';
{$ELSE}
  nztGetCertChain : function (_para1:Pnzctx; _para2:PnzttWallet):nzerror;cdecl;
{$ENDIF}

  {----------------------- nztCompareDN ----------------------- }
  {
   * NAME
   *    nztCompareDN - 
   * 
   * PARAMETERS
   *    Name   IN/OUT   Description
   *       osscntxt IN        OSS context.
   *   dn1   IN      distinguished name 1
   *   dn2     IN      distinguished name 2
   * 
   * NOTES
   *    
   * RETURNS
   *    NZERROR_OK       succeeded
   *   others         failed
   *    
    }

{$IFNDEF LinkDynamically}
  function nztCompareDN(_para1:Pnzctx; _para2:Pub1; _para3:ub4; _para4:Pub1; _para5:ub4;
             _para6:Pboolean):nzerror;cdecl;external ocilib name 'nztCompareDN';
{$ELSE}
  nztCompareDN : function (_para1:Pnzctx; _para2:Pub1; _para3:ub4; _para4:Pub1; _para5:ub4;
             _para6:Pboolean):nzerror;cdecl;
{$ENDIF}

{$ifdef NZ_OLD_TOOLS}
  {--------------------- nztIdentityAlloc --------------------- }
  {
   * NAME
   *    nztIdentityAlloc - Allocate memory for nzttIdentity context
   * 
   * PARAMETERS
   *    Name   IN/OUT   Description
   *       osscntxt IN        OSS context.
   *   identity OUT      nzttIdentity context
   * 
   * NOTES
   *    
   * RETURNS
   *    NZERROR_OK       succeeded
   *   others         failed
   *    
    }

{$IFNDEF LinkDynamically}
  function nztIdentityAlloc(_para1:Pnzctx; _para2:PPnzttIdentity):nzerror;cdecl;external ocilib name 'nztIdentityAlloc';
{$ELSE}
  nztIdentityAlloc : function (_para1:Pnzctx; _para2:PPnzttIdentity):nzerror;cdecl;
{$ENDIF}

  {--------------------- nztIPrivateAlloc --------------------- }
  {
   * NAME
   *    nztIPrivateAlloc - Allocate memory for nzttIdentityPrivate
   * 
   * PARAMETERS
   *    Name   IN/OUT   Description
   * 
   *       osscntxt IN        OSS context.
   *   ipriv   OUT      identityPrivate structure
   * NOTES
   *    
   * RETURNS
   *    NZERROR_OK       succeeded
   *   others         failed
   *    
    }

{$IFNDEF LinkDynamically}
  function nztIPrivateAlloc(_para1:Pnzctx; _para2:PPnzttIdentityPrivate):nzerror;cdecl;external ocilib name 'nztIPrivateAlloc';
{$ELSE}
  nztIPrivateAlloc : function (_para1:Pnzctx; _para2:PPnzttIdentityPrivate):nzerror;cdecl;
{$ENDIF}

  {---------------------- nztIDupContent ---------------------- }
  {
   * NAME
   *    nztIDupContent - 
   * 
   * PARAMETERS
   *    Name   IN/OUT   Description
   *       osscntxt IN        OSS context.
   *    targetIdentityOUT   target identity
   *   sourceIdentity IN   source identity
   * NOTES
   *    
   * RETURNS
   *    NZERROR_OK       succeeded
   *   others         failed
   *    
    }

{$IFNDEF LinkDynamically}
  function nztIDupContent(_para1:Pnzctx; _para2:PnzttIdentity; _para3:PnzttIdentity):nzerror;cdecl;external ocilib name 'nztIDupContent';
{$ELSE}
  nztIDupContent : function (_para1:Pnzctx; _para2:PnzttIdentity; _para3:PnzttIdentity):nzerror;cdecl;
{$ENDIF}

  {---------------------- nztIPDuplicate ---------------------- }
  {
   * NAME
   *    nztIPDuplicate - 
   * 
   * PARAMETERS
   *    Name   IN/OUT   Description
   *       osscntxt IN        OSS context.
   *   target_ipriv OUT   target identityPrivate
   *   source_ipriv IN   source identityPrivate
   * 
   * NOTES
   *    
   * RETURNS
   *    NZERROR_OK       succeeded
   *   others         failed
   *    
    }

{$IFNDEF LinkDynamically}
  function nztIPDuplicate(_para1:Pnzctx; _para2:PPnzttIdentityPrivate; _para3:PnzttIdentityPrivate):nzerror;cdecl;external ocilib name 'nztIPDuplicate';
{$ELSE}
  nztIPDuplicate : function (_para1:Pnzctx; _para2:PPnzttIdentityPrivate; _para3:PnzttIdentityPrivate):nzerror;cdecl;
{$ENDIF}

  {--------------------- nztiDupIdentList --------------------- }
  {
   * NAME
   *    nztiDupIdentList - 
   * 
   * PARAMETERS
   *    Name   IN/OUT   Description
   *       osscntxt IN        OSS context.
   *   source_identities IN   source identity list
   *   numIdent OUT      number of identity in the list
   *   ppidentity OUT   Target of identity   
   *   
   * NOTES
   *    
   * RETURNS
   *    NZERROR_OK       succeeded
   *   others         failed
   *    
    }

{$IFNDEF LinkDynamically}
  function nztiDupIdentList(_para1:Pnzctx; _para2:PnzttIdentity; _para3:Pub4; _para4:PPnzttIdentity):nzerror;cdecl;external ocilib name 'nztiDupIdentList';
{$ELSE}
  nztiDupIdentList : function (_para1:Pnzctx; _para2:PnzttIdentity; _para3:Pub4; _para4:PPnzttIdentity):nzerror;cdecl;
{$ENDIF}

  {--------------------- nztFreeIdentList --------------------- }
  {
   * NAME
   *    nztFreeIdentList - Free memory for a list of Identities
   * 
   * PARAMETERS
   *    Name   IN/OUT   Description
   *       osscntxt IN        OSS context.
   *   identity IN      identity context
   *
   * NOTES
   *    
   * RETURNS
   *    NZERROR_OK       succeeded
   *   others         failed
   *    
    }

{$IFNDEF LinkDynamically}
  function nztFreeIdentList(_para1:Pnzctx; _para2:PPnzttIdentity):nzerror;cdecl;external ocilib name 'nztFreeIdentList';
{$ELSE}
  nztFreeIdentList : function (_para1:Pnzctx; _para2:PPnzttIdentity):nzerror;cdecl;
{$ENDIF}

{$endif}
  { NZ_OLD_TOOLS  }
  {--------------------- nztCheckVaLidity --------------------- }
  {
   * NAME
   *    nztCheckVaLidity -  Check the validity of certificate
   * 
   * PARAMETERS
   *    Name   IN/OUT   Description
   *       osscntxt IN        OSS context.
   *   start_time      Start time of the certificate
   *   end_time      End time of the certificate
   * 
   * NOTES
   *    
   * RETURNS
   *    NZERROR_OK       succeeded
   *   others         failed
   *    
    }

{$IFNDEF LinkDynamically}
  function nztCheckValidity(_para1:Pnzctx; _para2:ub4; _para3:ub4):nzerror;cdecl;external ocilib name 'nztCheckValidity';
{$ELSE}
  nztCheckValidity : function (_para1:Pnzctx; _para2:ub4; _para3:ub4):nzerror;cdecl;
{$ENDIF}

  {--------------------- nztwCreateWallet --------------------- }
  {
   * NAME
   *    nztwCreateWallet - Create a new wallet.
   * 
   * PARAMETERS
   *    osscntxt IN     OSS context.
   *    wrllen   IN     Length of wallet resource locator.
   *    wrl      IN     WRL.
   *    pwdlen   IN     Length of password (see notes below).
   *    pwd      IN     Password.
   *    wallet   IN/OUT Wallet. 
   * 
   * NOTES
   *    It is an error to try to create a wallet that already exists.
   *    The previously existing wallet must be destroyed first.
   *
   *    The wallet itself is not encrypted.  Rather, all the personas in
   *    the wallet are encrypted under the same password.  A hash of the
   *    password is stored in the wallet.
   *
   *    Upon success, an empty open wallet is stored in the wallet
   *    parameter.
   *    
   * RETURNS
   *    NZERROR_OK                 Sucess.
   *    NZERROR_TK_WALLET_EXISTS   Wallet already exists.
   *    NZERROR_RIO_OPEN           RIO could not create wallet (see trace file).
    }

{$IFNDEF LinkDynamically}
  function nztwCreateWallet(_para1:Pnzctx; _para2:ub4; _para3:Ptext; _para4:ub4; _para5:Ptext;
             _para6:PnzttWallet):nzerror;cdecl;external ocilib name 'nztwCreateWallet';
{$ELSE}
  nztwCreateWallet : function (_para1:Pnzctx; _para2:ub4; _para3:Ptext; _para4:ub4; _para5:Ptext;
             _para6:PnzttWallet):nzerror;cdecl;
{$ENDIF}

  {--------------------- nztwDestroyWallet --------------------- }
  {
   * NAME
   *    nztwDestroyWallet - Destroy an existing wallet.
   * 
   * PARAMETERS
   *    osscntxt IN     OSS context.
   *    wrllen   IN     Length of wallet resource locator.
   *    wrl      IN     WRL.
   *    pwdlen   IN     Length of password.
   *    pwd      IN     Password.
   * NOTES
   *    
   * RETURNS
   *    NZERROR_OK           Success.
   *    NZERROR_TK_PASSWORD  Password verification failed.
   *    NZERROR_RIO_OPEN     RIO could not open wallet (see trace file).
   *    NZERROR_RIO_DELETE   Delete failed (see trace file).
    }

{$IFNDEF LinkDynamically}
  function nztwDestroyWallet(_para1:Pnzctx; _para2:ub4; _para3:Ptext; _para4:ub4; _para5:Ptext):nzerror;cdecl;external ocilib name 'nztwDestroyWallet';
{$ELSE}
  nztwDestroyWallet : function (_para1:Pnzctx; _para2:ub4; _para3:Ptext; _para4:ub4; _para5:Ptext):nzerror;cdecl;
{$ENDIF}

  {--------------------- nzteStorePersona --------------------- }
  {
   * NAME
   *    nzteStorePersona - Store an open persona in a wallet.
   * 
   * PARAMETERS
   *    osscntxt IN     OSS context. 
   *    persona  IN/OUT Persona.
   *    wallet   IN/OUT Wallet.
   * 
   * NOTES
   *    If the open persona is not associated with any wallet (it was
   *    created via the nzteClosePersona function), then storing the
   *    persona creates that association.  The wallet will also have an
   *    updated persona list that reflects this association.
   *
   *    If the open persona was associated with wallet 'A' (it was
   *    opened via the nztwOpenWallet function), and is stored back into
   *    wallet 'A', then then the old persona is overwritten by the new
   *    persona if the password can be verified.  Recall that all
   *    personas have a unique identity id.  If that id changes then
   *    storing the persona will put a new persona in the wallet.
   *
   *    If the open persona was associated with wallet 'A' and is stored
   *    into wallet 'B', and if wallet 'B' does not contain a persona
   *    with that unique identity id, then the persona will be copied
   *    into wallet 'B', wallet 'B''s persona list will be updated, and
   *    the persona structure will be updated to be associated with
   *    wallet 'B'.  If wallet 'B' already contained the persona, it
   *    would be overwritten by the new persona.
   *
   *    The persona parameter is doubly indirect so that at the
   *    conclusion of the function call, the pointer can be directed to
   *    the persona in the wallet.
   *
   * RETURNS
   *    NZERROR_OK           Success.
   *    NZERROR_TK_PASSWORD  Password verification failed.
   *    NZERROR_RIO_STORE    Store failed (see network trace file).
    }

{$IFNDEF LinkDynamically}
  function nzteStorePersona(_para1:Pnzctx; _para2:PPnzttPersona; _para3:PnzttWallet):nzerror;cdecl;external ocilib name 'nzteStorePersona';
{$ELSE}
  nzteStorePersona : function (_para1:Pnzctx; _para2:PPnzttPersona; _para3:PnzttWallet):nzerror;cdecl;
{$ENDIF}

  {--------------------- nzteRemovePersona --------------------- }
  {
   * NAME
   *    nzteRemovePersona - Remove a persona from the wallet.
   * 
   * PARAMETERS
   *    osscntxt IN     OSS context.
   *    persona  IN/OUT Persona.
   * 
   * NOTES
   *    The password is verified before trying to remove the persona.
   *    
   *    If the persona is open, it is closed.  The persona is removed
   *    from the wallet list and the persona pointer is set to NULL.
   *    
   *    A double indirect pointer to the persona is required so that the
   *    persona pointer can be set to NULL upon completion.
   *
   * RETURNS
   *    NZERROR_OK           Success.
   *    NZERROR_TK_PASSWORD  Password verification failed.
   *    NZERROR_RIO_DELETE   Delete failed.
    }

{$IFNDEF LinkDynamically}
  function nzteRemovePersona(_para1:Pnzctx; _para2:PPnzttPersona):nzerror;cdecl;external ocilib name 'nzteRemovePersona';
{$ELSE}
  nzteRemovePersona : function (_para1:Pnzctx; _para2:PPnzttPersona):nzerror;cdecl;
{$ENDIF}

  {--------------------- nzteCreatePersona --------------------- }
  {
   * NAME
   *    nzteCreatePersona - Create a persona.
   * 
   * PARAMETERS
   *    osscntxt IN     OSS context.
   *    itype    IN     Identity type.
   *    ctype    IN     Cipher type.
   *    desc     IN     Persona description.
   *    persona  OUT    Persona.
   * 
   * NOTES
   *    The resulting persona is created in the open state, but it will
   *    not be associated with a wallet.
   *    
   *    The memory for the persona is allocated by the function.
   *
   * RETURNS
   *    NZERROR_OK        Success.
   *    NZERROR_TK_TYPE   Unsupported itype/ctype combination.
   *    NZERROR_TK_PARMS  Error in persona description.
    }

{$IFNDEF LinkDynamically}
  function nzteCreatePersona(_para1:Pnzctx; _para2:nzttVersion; _para3:nzttCipherType; _para4:PnzttPersonaDesc; _para5:PPnzttPersona):nzerror;cdecl;external ocilib name 'nzteCreatePersona';
{$ELSE}
  nzteCreatePersona : function (_para1:Pnzctx; _para2:nzttVersion; _para3:nzttCipherType; _para4:PnzttPersonaDesc; _para5:PPnzttPersona):nzerror;cdecl;
{$ENDIF}

  {----------------- nztiStoreTrustedIdentity ----------------- }
  {
   * NAME
   *    nztiStoreTrustedIdentity - Store an identity into a persona.
   * 
   * PARAMETERS
   *    osscntxt IN     Success.
   *    identity IN/OUT Trusted Identity.
   *    persona  IN/OUT Persona.    
   * 
   * NOTES
   *    The identity is not saved with the persona in the wallet until
   *    the persona is stored.
   *    
   *    The identity parameter is double indirect so that it can point
   *    into the persona at the conclusion of the call.
   *
   * RETURNS
   *    NZERROR_OK     Success.
    }

{$IFNDEF LinkDynamically}
  function nztiStoreTrustedIdentity(_para1:Pnzctx; _para2:PPnzttIdentity; _para3:PnzttPersona):nzerror;cdecl;external ocilib name 'nztiStoreTrustedIdentity';
{$ELSE}
  nztiStoreTrustedIdentity : function (_para1:Pnzctx; _para2:PPnzttIdentity; _para3:PnzttPersona):nzerror;cdecl;
{$ENDIF}

  {--------------------- nzteSetProtection --------------------- }
  {
   * NAME
   *    nzteSetProtection - Set the protection type for a CE function.
   * 
   * PARAMETERS
   *    osscntxt IN     OSS context.
   *    persona  IN/OUT Persona.
   *    func     IN     CE function.
   *    tdufmt   IN     TDU Format.
   *    protinfo IN     Protection information specific to this format.
   * 
   * NOTES
   *    
   * RETURNS
   *    NZERROR_OK              Success.
   *    NZERROR_TK_PROTECTION   Unsupported protection.
   *    NZERROR_TK_PARMS        Error in protection info.
    }

{$IFNDEF LinkDynamically}
  function nzteSetProtection(_para1:Pnzctx; _para2:PnzttPersona; _para3:nzttcef; _para4:nztttdufmt; _para5:PnzttProtInfo):nzerror;cdecl;external ocilib name 'nzteSetProtection';
{$ELSE}
  nzteSetProtection : function (_para1:Pnzctx; _para2:PnzttPersona; _para3:nzttcef; _para4:nztttdufmt; _para5:PnzttProtInfo):nzerror;cdecl;
{$ENDIF}

  {--------------------- nzteGetProtection --------------------- }
  {
   * NAME
   *    nzteGetProtection - Get the protection type for a CE function.
   * 
   * PARAMETERS
   *    osscntxt IN     OSS context.
   *    persona  IN     Persona.
   *    func     IN     CE function.
   *    tdufmt   OUT    TDU format.
   *    protinfo OUT    Protection information.
   * 
   * NOTES
   *    
   * RETURNS
   *    NZERROR_OK        Success.
    }

{$IFNDEF LinkDynamically}
  function nzteGetProtection(_para1:Pnzctx; _para2:PnzttPersona; _para3:nzttcef; _para4:Pnztttdufmt; _para5:PnzttProtInfo):nzerror;cdecl;external ocilib name 'nzteGetProtection';
{$ELSE}
  nzteGetProtection : function (_para1:Pnzctx; _para2:PnzttPersona; _para3:nzttcef; _para4:Pnztttdufmt; _para5:PnzttProtInfo):nzerror;cdecl;
{$ENDIF}

  {-------------------- nztiRemoveIdentity -------------------- }
  {
   * NAME
   *    nztiRemoveIdentity - Remove an identity from an open persona.
   * 
   * PARAMETERS
   *    osscntxt IN     OSS context. 
   *    identity IN/OUT     Identity.
   * 
   * NOTES
   *    If the persona is not stored, this identity will still be in the
   *    persona stored in the wallet.
   *    
   *    The identity parameter is doubly indirect so that at the
   *    conclusion of the function, the pointer can be set to NULL.
   *
   * RETURNS
   *    NZERROR_OK           Success.
   *    NZERROR_TK_NOTFOUND  Identity not found.
   *    NZERROR_TK_NOTOPEN   Persona is not open.
    }

{$IFNDEF LinkDynamically}
  function nztiRemoveIdentity(_para1:Pnzctx; _para2:PPnzttIdentity):nzerror;cdecl;external ocilib name 'nztiRemoveIdentity';
{$ELSE}
  nztiRemoveIdentity : function (_para1:Pnzctx; _para2:PPnzttIdentity):nzerror;cdecl;
{$ENDIF}

  {----------------- nztifdn ----------------- }
  {
   * NAME
   *    nztifdn - create an Identity From a Distinguished Name
   * 
   * PARAMETERS
   *    osscntxt           IN  OSS context.
   *    length             IN  Length of the distinguished name
   *    distinguished_name IN  distinguished name string
   *    ppidentity         OUT created identity
   * 
   * NOTES
   *    Given a distinguished name, return the identity that corresponds to it.
   *
   * RETURNS
   *    NZERROR_OK     Success.
    }

{$IFNDEF LinkDynamically}
  function nztifdn(ossctx:Pnzctx; length:ub4; distinguished_name:Ptext; ppidentity:PPnzttIdentity):nzerror;cdecl;external ocilib name 'nztifdn';
{$ELSE}
  nztifdn : function (ossctx:Pnzctx; length:ub4; distinguished_name:Ptext; ppidentity:PPnzttIdentity):nzerror;cdecl;
{$ENDIF}

  {--------------------- nztxSignExpansion --------------------- }
  {
   * NAME
   *    nztxSignExpansion - Determine the size of the attached signature buffer.
   * 
   * PARAMETERS
   *    osscntxt IN  OSS context.
   *    persona  IN  Persona.
   *    inlen    IN  Length of input.
   *    tdulen   OUT Buffer needed for signature.
   *
   * NOTES
   *    
   * RETURNS
   *    NZERROR_OK           Success.
   *    NZERROR_TK_NOTOPEN   Persona is not open.
   *    NZERROR_TK_NOTSUPP   Function not supported with persona.
    }

{$IFNDEF LinkDynamically}
  function nztxSignExpansion(_para1:Pnzctx; _para2:PnzttPersona; _para3:ub4; _para4:Pub4):nzerror;cdecl;external ocilib name 'nztxSignExpansion';
{$ELSE}
  nztxSignExpansion : function (_para1:Pnzctx; _para2:PnzttPersona; _para3:ub4; _para4:Pub4):nzerror;cdecl;
{$ENDIF}

  {--------------- nztxsd_SignDetachedExpansion --------------- }
  {
   * NAME
   *    nztxsd_SignDetachedExpansion - Determine the size of buffer needed.
   * 
   * PARAMETERS
   *    osscntxt IN  OSS context.
   *    persona  IN  Persona.
   *    inlen    IN  Length of input.
   *    tdulen   OUT Buffer needed for signature.
   * 
   * NOTES
   *    
   * RETURNS
   *    NZERROR_OK           Success.
   *    NZERROR_TK_NOTOPEN   Persona is not open.
   *    NZERROR_TK_NOTSUPP   Function not supported with persona.
    }

{$IFNDEF LinkDynamically}
  function nztxsd_SignDetachedExpansion(_para1:Pnzctx; _para2:PnzttPersona; _para3:ub4; _para4:Pub4):nzerror;cdecl;external ocilib name 'nztxsd_SignDetachedExpansion';
{$ELSE}
  nztxsd_SignDetachedExpansion : function (_para1:Pnzctx; _para2:PnzttPersona; _para3:ub4; _para4:Pub4):nzerror;cdecl;
{$ENDIF}

  {------------------------ nztEncrypt ------------------------ }
  {
   * NAME
   *    nztEncrypt - Symmetrically encrypt
   * 
   * PARAMETERS
   *    osscntxt    IN     OSS context.
   *    persona     IN     Persona.
   *    inlen       IN     Length of this input part.
   *    in          IN     This input part.
   *    tdubuf      IN/OUT TDU buffer.
   * 
   * NOTES
   *    
   * RETURNS
   *    NZERROR_OK           Success.
   *    NZERROR_TK_CANTGROW  Needed to grow TDU buffer but could not.
   *    NZERROR_TK_NOTOPEN   Persona is not open.
   *    NZERROR_TK_NOTSUPP   Function not supported with persona.
    }

{$IFNDEF LinkDynamically}
  function nztEncrypt(_para1:Pnzctx; _para2:PnzttPersona; _para3:nzttces; _para4:ub4; _para5:Pub1;
             _para6:PnzttBufferBlock):nzerror;cdecl;external ocilib name 'nztEncrypt';
{$ELSE}
  nztEncrypt : function (_para1:Pnzctx; _para2:PnzttPersona; _para3:nzttces; _para4:ub4; _para5:Pub1;
             _para6:PnzttBufferBlock):nzerror;cdecl;
{$ENDIF}

  {------------------- nztxEncryptExpansion ------------------- }
  {
   * NAME
   *    nztxEncryptExpansion - Determine the size of the TDU to encrypt.
   * 
   * PARAMETERS
   *    osscntxt    IN  OSS context.
   *    persona     IN  Persona.
   *    inlen       IN  Length of this input part.
   *    tdulen      OUT Length of TDU.
   * 
   * NOTES
   *    
   * RETURNS
   *    NZERROR_OK           Success.
   *    NZERROR_TK_NOTOPEN   Persona is not open.
   *    NZERROR_TK_NOTSUPP   Function not supported with persona.
    }

{$IFNDEF LinkDynamically}
  function nztxEncryptExpansion(_para1:Pnzctx; _para2:PnzttPersona; _para3:ub4; _para4:Pub4):nzerror;cdecl;external ocilib name 'nztxEncryptExpansion';
{$ELSE}
  nztxEncryptExpansion : function (_para1:Pnzctx; _para2:PnzttPersona; _para3:ub4; _para4:Pub4):nzerror;cdecl;
{$ENDIF}

  {------------------------ nztDecrypt ------------------------ }
  {
   * NAME
   *    nztDecrypt - Decrypt an Encrypted message.
   * 
   * PARAMETERS
   *    osscntxt IN     OSS context.
   *    persona  IN     Persona.
   *    state    IN     State of decryption.
   *    inlen    IN     Length of this input part.
   *    in       IN     This input part.
   *    out      IN/OUT Cleartext message.
   * 
   * NOTES
   *    
   * RETURNS
   *    NZERROR_OK           Success.
   *    NZERROR_TK_CANTGROW  Needed to grow TDU buffer but could not.
   *    NZERROR_TK_NOTOPEN   Persona is not open.
   *    NZERROR_TK_NOTSUPP   Function not supported with persona.
    }

{$IFNDEF LinkDynamically}
  function nztDecrypt(_para1:Pnzctx; _para2:PnzttPersona; _para3:nzttces; _para4:ub4; _para5:Pub1;
             _para6:PnzttBufferBlock):nzerror;cdecl;external ocilib name 'nztDecrypt';
{$ELSE}
  nztDecrypt : function (_para1:Pnzctx; _para2:PnzttPersona; _para3:nzttces; _para4:ub4; _para5:Pub1;
             _para6:PnzttBufferBlock):nzerror;cdecl;
{$ENDIF}

  {------------------------ nztEnvelope ------------------------ }
  {
   * NAME
   *    nztEnvelope - Sign and PKEncrypt a message.
   * 
   * PARAMETERS
   *    osscntxt    IN     OSS context.
   *    persona     IN     Persona.
   *    nrecipients IN     Number of recipients for this encryption.
   *    recipients  IN     List of recipients.
   *    state       IN     State of encryption.
   *    inlen       IN     Length of this input part.
   *    in          IN     This input part.
   *    tdubuf      IN/OUT TDU buffer.
   *
   * NOTES
   *    
   * RETURNS
   *    NZERROR_OK           Success.
   *    NZERROR_TK_CANTGROW  Needed to grow output buffer but could not.
   *    NZERROR_TK_NOTOPEN   Persona is not open.
   *    NZERROR_TK_NOTSUPP   Function not supported with persona.
    }

{$IFNDEF LinkDynamically}
  function nztEnvelope(_para1:Pnzctx; _para2:PnzttPersona; _para3:ub4; _para4:PnzttIdentity; _para5:nzttces;
             _para6:ub4; _para7:Pub1; _para8:PnzttBufferBlock):nzerror;cdecl;external ocilib name 'nztEnvelope';
{$ELSE}
  nztEnvelope : function (_para1:Pnzctx; _para2:PnzttPersona; _para3:ub4; _para4:PnzttIdentity; _para5:nzttces;
             _para6:ub4; _para7:Pub1; _para8:PnzttBufferBlock):nzerror;cdecl;
{$ENDIF}

  {----------------------- nztDeEnvelope ----------------------- }
  {
   * NAME
   *    nztDeEnvelope - PKDecrypt and verify a message.
   * 
   * PARAMETERS
   *    osscntxt    IN     OSS context.
   *    persona     IN     Persona.
   *    state       IN     State of encryption.
   *    inlen       IN     Length of this input part.
   *    in          IN     This input part.
   *    out         OUT    Message from TDU.
   *    verified    OUT    TRUE if verified.
   *    validated   OUT    TRUE if validated.
   *    sender      OUT    Identity of sender.
   *
   * NOTES
   *    
   * RETURNS
   *    NZERROR_OK           Success.
   *    NZERROR_TK_CANTGROW  Needed to grow TDU buffer but could not.
   *    NZERROR_TK_NOTOPEN   Persona is not open.
   *    NZERROR_TK_NOTSUPP   Function not supported with persona.
    }

{$IFNDEF LinkDynamically}
  function nztDeEnvelope(_para1:Pnzctx; _para2:PnzttPersona; _para3:nzttces; _para4:ub4; _para5:Pub1;
             _para6:PnzttBufferBlock; _para7:Pboolean; _para8:Pboolean; _para9:PPnzttIdentity):nzerror;cdecl;external ocilib name 'nztDeEnvelope';
{$ELSE}
  nztDeEnvelope : function (_para1:Pnzctx; _para2:PnzttPersona; _para3:nzttces; _para4:ub4; _para5:Pub1;
             _para6:PnzttBufferBlock; _para7:Pboolean; _para8:Pboolean; _para9:PPnzttIdentity):nzerror;cdecl;
{$ENDIF}

  {----------------------- nztKeyedHash ----------------------- }
  {
   * NAME
   *    nztKeyedHash - Generate a keyed hash.
   * 
   * PARAMETERS
   *    osscntxt IN     OSS context.
   *    persona  IN     Persona.
   *    state    IN     State of hash.
   *    inlen    IN     Length of this input.
   *    in       IN     This input.
   *    tdu      IN/OUT Output tdu.
   * 
   * NOTES
   *    
   * RETURNS
   *    NZERROR_OK           Success.
   *    NZERROR_TK_CANTGROW  Needed to grow TDU buffer but could not.
   *    NZERROR_TK_NOTOPEN   Persona is not open.
   *    NZERROR_TK_NOTSUPP   Function not supported with persona.
    }

{$IFNDEF LinkDynamically}
  function nztKeyedHash(_para1:Pnzctx; _para2:PnzttPersona; _para3:nzttces; _para4:ub4; _para5:Pub1;
             _para6:PnzttBufferBlock):nzerror;cdecl;external ocilib name 'nztKeyedHash';
{$ELSE}
  nztKeyedHash : function (_para1:Pnzctx; _para2:PnzttPersona; _para3:nzttces; _para4:ub4; _para5:Pub1;
             _para6:PnzttBufferBlock):nzerror;cdecl;
{$ENDIF}

  {------------------ nztxKeyedHashExpansion ------------------ }
  {
   * NAME
   *    nztxKeyedHashExpansion - Determine the space needed for a keyed hash.
   * 
   * PARAMETERS
   *    osscntxt IN  OSS context.
   *    persona  IN  Persona.
   *    inlen    IN  Length of this input.
   *    tdulen   OUT TDU length.
   * 
   * NOTES
   *    
   * RETURNS
   *    NZERROR_OK           Success.
   *    NZERROR_TK_NOTOPEN   Persona is not open.
   *    NZERROR_TK_NOTSUPP   Function not supported with persona.
    }

{$IFNDEF LinkDynamically}
  function nztxKeyedHashExpansion(_para1:Pnzctx; _para2:PnzttPersona; _para3:ub4; _para4:Pub4):nzerror;cdecl;external ocilib name 'nztxKeyedHashExpansion';
{$ELSE}
  nztxKeyedHashExpansion : function (_para1:Pnzctx; _para2:PnzttPersona; _para3:ub4; _para4:Pub4):nzerror;cdecl;
{$ENDIF}

  {--------------------- nztxHashExpansion --------------------- }
  {
   * NAME
   *    nztxHashExpansion - Determine the size of the TDU for a hash.
   * 
   * PARAMETERS
   *    osscntxt IN  OSS context.
   *    persona  IN  Persona.
   *    inlen    IN  Length of this input.
   *    tdulen   OUT TDU length.
   * 
   * NOTES
   *    
   * RETURNS
   *    NZERROR_OK           Success.
   *    NZERROR_TK_NOTOPEN   Persona is not open.
   *    NZERROR_TK_NOTSUPP   Function not supported with persona.
    }

{$IFNDEF LinkDynamically}
  function nztxHashExpansion(_para1:Pnzctx; _para2:PnzttPersona; _para3:ub4; _para4:Pub4):nzerror;cdecl;external ocilib name 'nztxHashExpansion';
{$ELSE}
  nztxHashExpansion : function(_para1:Pnzctx; _para2:PnzttPersona; _para3:ub4; _para4:Pub4):nzerror;cdecl;
{$ENDIF}

  {---------------- nztiae_IsAuthEnabled ---------------- }
  {
   * NAME
   *    nztiae_IsAuthEnabled -  Checks to see if Authentication is Enabled
   *                               in the current Cipher Spec.
   * 
   * PARAMETERS
   *    ctx   IN   Oracle SSL Context
   *      ncipher IN    CipherSuite
   *      authEnabled OUT Boolean for is Auth Enabled?
   * 
   * NOTES
   *    
   * RETURNS
   *      NZERROR_OK on success.
   *      NZERROR_TK_INV_CIPHR_TYPE if Cipher Spec is not Recognized.
    }

{$IFNDEF LinkDynamically}
  function nztiae_IsAuthEnabled(_para1:Pnzctx; _para2:ub2; _para3:Pboolean):nzerror;cdecl;external ocilib name 'nztiae_IsAuthEnabled';
{$ELSE}
  nztiae_IsAuthEnabled : function (_para1:Pnzctx; _para2:ub2; _para3:Pboolean):nzerror;cdecl;
{$ENDIF}

  {---------------- nztiee_IsEncrEnabled ---------------- }
  {
   * NAME
   *    nztiee_IsEncrEnabled -  Checks to see if Encryption is Enabled
   *                               in the current Cipher Spec.
   * 
   * PARAMETERS
   *    ctx   IN   Oracle SSL Context
   *      ncipher IN    CipherSuite
   *      EncrEnabled OUT Boolean for is Auth Enabled?
   * 
   * NOTES
   *    
   * RETURNS
   *      NZERROR_OK on success.
   *      NZERROR_TK_INV_CIPHR_TYPE if Cipher Spec is not Recognized.
    }

{$IFNDEF LinkDynamically}
  function nztiee_IsEncrEnabled(_para1:Pnzctx; _para2:ub2; _para3:Pboolean):nzerror;cdecl;external ocilib name 'nztiee_IsEncrEnabled';
{$ELSE}
  nztiee_IsEncrEnabled : function (_para1:Pnzctx; _para2:ub2; _para3:Pboolean):nzerror;cdecl;
{$ENDIF}

  {---------------- nztihe_IsHashEnabled ---------------- }
  {
   * NAME
   *    nztihe_IsHashEnabled -  Checks to see if HAshing is Enabled
   *                               in the current Cipher Spec.
   * 
   * PARAMETERS
   *    ctx   IN   Oracle SSL Context
   *      ncipher IN    CipherSuite
   *      hashEnabled OUT Boolean for is Auth Enabled?
   * 
   * NOTES
   *    
   * RETURNS
   *      NZERROR_OK on success.
   *      NZERROR_TK_INV_CIPHR_TYPE if Cipher Spec is not Recognized.
    }

{$IFNDEF LinkDynamically}
  function nztihe_IsHashEnabled(_para1:Pnzctx; _para2:ub2; _para3:Pboolean):nzerror;cdecl;external ocilib name 'nztihe_IsHashEnabled';
{$ELSE}
  nztihe_IsHashEnabled : function (_para1:Pnzctx; _para2:ub2; _para3:Pboolean):nzerror;cdecl;
{$ENDIF}

  {
   *
    }
{$IFNDEF LinkDynamically}

  function nztGetIssuerName(_para1:Pnzctx; _para2:PnzttIdentity; _para3:PPub1; _para4:Pub4):nzerror;cdecl;external ocilib name 'nztGetIssuerName';


  function nztGetSubjectName(_para1:Pnzctx; _para2:PnzttIdentity; _para3:PPub1; _para4:Pub4):nzerror;cdecl;external ocilib name 'nztGetSubjectName';


  function nztGetBase64Cert(_para1:Pnzctx; _para2:PnzttIdentity; _para3:PPub1; _para4:Pub4):nzerror;cdecl;external ocilib name 'nztGetBase64Cert';

  function nztGetSerialNumber(_para1:Pnzctx; _para2:PnzttIdentity; _para3:PPub1; _para4:Pub4):nzerror;cdecl;external ocilib name 'nztGetSerialNumber';

  function nztGetValidDate(_para1:Pnzctx; _para2:PnzttIdentity; _para3:Pub4; _para4:Pub4):nzerror;cdecl;external ocilib name 'nztGetValidDate';

  function nztGetVersion(_para1:Pnzctx; _para2:PnzttIdentity; _para3:Pnzstrc):nzerror;cdecl;external ocilib name 'nztGetVersion';

  function nztGetPublicKey(_para1:Pnzctx; _para2:PnzttIdentity; _para3:PPub1; _para4:Pub4):nzerror;cdecl;external ocilib name 'nztGetPublicKey';

  function nztGenericDestroy(_para1:Pnzctx; _para2:PPub1):nzerror;cdecl;external ocilib name 'nztGenericDestroy';

  function nztSetAppDefaultLocation(_para1:Pnzctx; _para2:Ptext; _para3:size_t):nzerror;cdecl;external ocilib name 'nztSetAppDefaultLocation';

  function nztSearchNZDefault(_para1:Pnzctx; _para2:Pboolean):nzerror;cdecl;external ocilib name 'nztSearchNZDefault';
{$ELSE}
  nztGetIssuerName : function (_para1:Pnzctx; _para2:PnzttIdentity; _para3:PPub1; _para4:Pub4):nzerror;cdecl;


  nztGetSubjectName: function(_para1:Pnzctx; _para2:PnzttIdentity; _para3:PPub1; _para4:Pub4):nzerror;cdecl;


  nztGetBase64Cert : function(_para1:Pnzctx; _para2:PnzttIdentity; _para3:PPub1; _para4:Pub4):nzerror;cdecl;

  nztGetSerialNumber : function(_para1:Pnzctx; _para2:PnzttIdentity; _para3:PPub1; _para4:Pub4):nzerror;cdecl;

  nztGetValidDate : function(_para1:Pnzctx; _para2:PnzttIdentity; _para3:Pub4; _para4:Pub4):nzerror;cdecl;

  nztGetVersion : function(_para1:Pnzctx; _para2:PnzttIdentity; _para3:Pnzstrc):nzerror;cdecl;

  nztGetPublicKey : function(_para1:Pnzctx; _para2:PnzttIdentity; _para3:PPub1; _para4:Pub4):nzerror;cdecl;

  nztGenericDestroy : function(_para1:Pnzctx; _para2:PPub1):nzerror;cdecl;

  nztSetAppDefaultLocation : function(_para1:Pnzctx; _para2:Ptext; _para3:size_t):nzerror;cdecl;

  nztSearchNZDefault : function(_para1:Pnzctx; _para2:Pboolean):nzerror;cdecl;
{$ENDIF}

