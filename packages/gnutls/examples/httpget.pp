program httpget;

uses ssockets,gnutls;

Const
  MAX_BUF = 1024*256;
  MSG = 'GET / HTTP/1.0'#13#10#13#10;
  
  GNUTLS_SERVER                = 1;
  GNUTLS_CLIENT                = (1 shl 1);
  GNUTLS_DATAGRAM              = (1 shl 2);
  GNUTLS_NONBLOCK              = (1 shl 3);
  GNUTLS_NO_EXTENSIONS         = (1 shl 4);
  GNUTLS_NO_REPLAY_PROTECTION  = (1 shl 5);
  GNUTLS_NO_SIGNAL             = (1 shl 6);

  GNUTLS_DEFAULT_HANDSHAKE_TIMEOUT = cardinal(-1);

   GNUTLS_E_SUCCESS = 0;
  	GNUTLS_E_UNKNOWN_COMPRESSION_ALGORITHM= -3;
  	GNUTLS_E_UNKNOWN_CIPHER_TYPE =-6;
  	GNUTLS_E_LARGE_PACKET =-7;
   GNUTLS_E_UNSUPPORTED_VERSION_PACKET =-8	;
   GNUTLS_E_UNEXPECTED_PACKET_LENGTH =-9	;
   GNUTLS_E_INVALID_SESSION =-10;
   GNUTLS_E_FATAL_ALERT_RECEIVED =-12;
   GNUTLS_E_UNEXPECTED_PACKET =-15;
   GNUTLS_E_WARNING_ALERT_RECEIVED= -16;
   GNUTLS_E_ERROR_IN_FINISHED_PACKET= -18;
   GNUTLS_E_UNEXPECTED_HANDSHAKE_PACKET= -19;
  	GNUTLS_E_UNKNOWN_CIPHER_SUITE= -21;
  	GNUTLS_E_UNWANTED_ALGORITHM= -22;
  	GNUTLS_E_MPI_SCAN_FAILED= -23;
   GNUTLS_E_DECRYPTION_FAILED= -24;
   GNUTLS_E_MEMORY_ERROR =-25;
   GNUTLS_E_DECOMPRESSION_FAILED =-26;
   GNUTLS_E_COMPRESSION_FAILED= -27;
   GNUTLS_E_AGAIN= -28;
   GNUTLS_E_EXPIRED =-29;
   GNUTLS_E_DB_ERROR =-30;
   GNUTLS_E_SRP_PWD_ERROR =-31;
   GNUTLS_E_INSUFFICIENT_CREDENTIALS =-32;
   GNUTLS_E_INSUFICIENT_CREDENTIALS =GNUTLS_E_INSUFFICIENT_CREDENTIALS;
   GNUTLS_E_INSUFFICIENT_CRED =GNUTLS_E_INSUFFICIENT_CREDENTIALS;
   GNUTLS_E_INSUFICIENT_CRED =GNUTLS_E_INSUFFICIENT_CREDENTIALS;

   GNUTLS_E_HASH_FAILED =-33;
   GNUTLS_E_BASE64_DECODING_ERROR =-34;

  	GNUTLS_E_MPI_PRINT_FAILED =-35;
   GNUTLS_E_REHANDSHAKE =-37;
   GNUTLS_E_GOT_APPLICATION_DATA =-38;
   GNUTLS_E_RECORD_LIMIT_REACHED =-39;
   GNUTLS_E_ENCRYPTION_FAILED= -40;

   GNUTLS_E_PK_ENCRYPTION_FAILED =-44;
   GNUTLS_E_PK_DECRYPTION_FAILED =-45;
   GNUTLS_E_PK_SIGN_FAILED =-46;
   GNUTLS_E_X509_UNSUPPORTED_CRITICAL_EXTENSION =-47;
   GNUTLS_E_KEY_USAGE_VIOLATION =-48;
   GNUTLS_E_NO_CERTIFICATE_FOUND =-49;
   GNUTLS_E_INVALID_REQUEST= -50;
   GNUTLS_E_SHORT_MEMORY_BUFFER =-51;
   GNUTLS_E_INTERRUPTED =-52;
   GNUTLS_E_PUSH_ERROR =-53;
   GNUTLS_E_PULL_ERROR =-54;
   GNUTLS_E_RECEIVED_ILLEGAL_PARAMETER =-55;
   GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE =-56;
   GNUTLS_E_PKCS1_WRONG_PAD =-57;
   GNUTLS_E_RECEIVED_ILLEGAL_EXTENSION =-58;
   GNUTLS_E_INTERNAL_ERROR =-59;
   GNUTLS_E_DH_PRIME_UNACCEPTABLE =-63;
   GNUTLS_E_FILE_ERROR =-64;
   GNUTLS_E_TOO_MANY_EMPTY_PACKETS= -78;
   GNUTLS_E_UNKNOWN_PK_ALGORITHM =-80;
   GNUTLS_E_TOO_MANY_HANDSHAKE_PACKETS =-81;

    GNUTLS_E_NO_TEMPORARY_RSA_PARAMS = -(84);    
    GNUTLS_E_NO_COMPRESSION_ALGORITHMS = -(86);    
    GNUTLS_E_NO_CIPHER_SUITES = -(87);    
    GNUTLS_E_OPENPGP_GETKEY_FAILED = -(88);    
    GNUTLS_E_PK_SIG_VERIFY_FAILED = -(89);    
    GNUTLS_E_ILLEGAL_SRP_USERNAME = -(90);    
    GNUTLS_E_SRP_PWD_PARSING_ERROR = -(91);    
    GNUTLS_E_NO_TEMPORARY_DH_PARAMS = -(93);    
  { For certificate and key stuff
      }
    GNUTLS_E_ASN1_ELEMENT_NOT_FOUND = -(67);    
    GNUTLS_E_ASN1_IDENTIFIER_NOT_FOUND = -(68);    
    GNUTLS_E_ASN1_DER_ERROR = -(69);    
    GNUTLS_E_ASN1_VALUE_NOT_FOUND = -(70);    
    GNUTLS_E_ASN1_GENERIC_ERROR = -(71);    
    GNUTLS_E_ASN1_VALUE_NOT_VALID = -(72);    
    GNUTLS_E_ASN1_TAG_ERROR = -(73);    
    GNUTLS_E_ASN1_TAG_IMPLICIT = -(74);    
    GNUTLS_E_ASN1_TYPE_ANY_ERROR = -(75);    
    GNUTLS_E_ASN1_SYNTAX_ERROR = -(76);    
    GNUTLS_E_ASN1_DER_OVERFLOW = -(77);    
    GNUTLS_E_OPENPGP_UID_REVOKED = -(79);    
    GNUTLS_E_CERTIFICATE_ERROR = -(43);    
    GNUTLS_E_X509_CERTIFICATE_ERROR = GNUTLS_E_CERTIFICATE_ERROR;    
    GNUTLS_E_CERTIFICATE_KEY_MISMATCH = -(60);    
  { GNUTLS_A_UNSUPPORTED_CERTIFICATE  }
    GNUTLS_E_UNSUPPORTED_CERTIFICATE_TYPE = -(61);    
    GNUTLS_E_X509_UNKNOWN_SAN = -(62);    
    GNUTLS_E_OPENPGP_FINGERPRINT_UNSUPPORTED = -(94);    
    GNUTLS_E_X509_UNSUPPORTED_ATTRIBUTE = -(95);    
    GNUTLS_E_UNKNOWN_HASH_ALGORITHM = -(96);    
    GNUTLS_E_UNKNOWN_PKCS_CONTENT_TYPE = -(97);    
    GNUTLS_E_UNKNOWN_PKCS_BAG_TYPE = -(98);    
    GNUTLS_E_INVALID_PASSWORD = -(99);    
  { for PKCS #12 MAC  }
    GNUTLS_E_MAC_VERIFY_FAILED = -(100);    
    GNUTLS_E_CONSTRAINT_ERROR = -(101);    
    GNUTLS_E_WARNING_IA_IPHF_RECEIVED = -(102);    
    GNUTLS_E_WARNING_IA_FPHF_RECEIVED = -(103);    
    GNUTLS_E_IA_VERIFY_FAILED = -(104);    
    GNUTLS_E_UNKNOWN_ALGORITHM = -(105);    
    GNUTLS_E_UNSUPPORTED_SIGNATURE_ALGORITHM = -(106);    
    GNUTLS_E_SAFE_RENEGOTIATION_FAILED = -(107);    
    GNUTLS_E_UNSAFE_RENEGOTIATION_DENIED = -(108);    
    GNUTLS_E_UNKNOWN_SRP_USERNAME = -(109);    
    GNUTLS_E_PREMATURE_TERMINATION = -(110);    
    GNUTLS_E_BASE64_ENCODING_ERROR = -(201);    
  { obsolete  }
    GNUTLS_E_INCOMPATIBLE_GCRYPT_LIBRARY = -(202);    
    GNUTLS_E_INCOMPATIBLE_CRYPTO_LIBRARY = -(202);    
    GNUTLS_E_INCOMPATIBLE_LIBTASN1_LIBRARY = -(203);    
    GNUTLS_E_OPENPGP_KEYRING_ERROR = -(204);    
    GNUTLS_E_X509_UNSUPPORTED_OID = -(205);    
    GNUTLS_E_RANDOM_FAILED = -(206);    
    GNUTLS_E_BASE64_UNEXPECTED_HEADER_ERROR = -(207);    
    GNUTLS_E_OPENPGP_SUBKEY_ERROR = -(208);    
    GNUTLS_E_ALREADY_REGISTERED = -(209);    
    GNUTLS_E_CRYPTO_ALREADY_REGISTERED = GNUTLS_E_ALREADY_REGISTERED;    
    GNUTLS_E_HANDSHAKE_TOO_LARGE = -(210);    
    GNUTLS_E_CRYPTODEV_IOCTL_ERROR = -(211);    
    GNUTLS_E_CRYPTODEV_DEVICE_ERROR = -(212);    
    GNUTLS_E_CHANNEL_BINDING_NOT_AVAILABLE = -(213);    
    GNUTLS_E_BAD_COOKIE = -(214);    
    GNUTLS_E_OPENPGP_PREFERRED_KEY_ERROR = -(215);    
    GNUTLS_E_INCOMPAT_DSA_KEY_WITH_TLS_PROTOCOL = -(216);    
    GNUTLS_E_INSUFFICIENT_SECURITY = -(217);    
    GNUTLS_E_HEARTBEAT_PONG_RECEIVED = -(292);    
    GNUTLS_E_HEARTBEAT_PING_RECEIVED = -(293);    
  { PKCS11 related  }
    GNUTLS_E_PKCS11_ERROR = -(300);    
    GNUTLS_E_PKCS11_LOAD_ERROR = -(301);    
    GNUTLS_E_PARSING_ERROR = -(302);    
    GNUTLS_E_PKCS11_PIN_ERROR = -(303);    
    GNUTLS_E_PKCS11_SLOT_ERROR = -(305);    
    GNUTLS_E_LOCKING_ERROR = -(306);    
    GNUTLS_E_PKCS11_ATTRIBUTE_ERROR = -(307);    
    GNUTLS_E_PKCS11_DEVICE_ERROR = -(308);    
    GNUTLS_E_PKCS11_DATA_ERROR = -(309);    
    GNUTLS_E_PKCS11_UNSUPPORTED_FEATURE_ERROR = -(310);    
    GNUTLS_E_PKCS11_KEY_ERROR = -(311);    
    GNUTLS_E_PKCS11_PIN_EXPIRED = -(312);    
    GNUTLS_E_PKCS11_PIN_LOCKED = -(313);    
    GNUTLS_E_PKCS11_SESSION_ERROR = -(314);    
    GNUTLS_E_PKCS11_SIGNATURE_ERROR = -(315);    
    GNUTLS_E_PKCS11_TOKEN_ERROR = -(316);    
    GNUTLS_E_PKCS11_USER_ERROR = -(317);    
    GNUTLS_E_CRYPTO_INIT_FAILED = -(318);    
    GNUTLS_E_TIMEDOUT = -(319);    
    GNUTLS_E_USER_ERROR = -(320);    
    GNUTLS_E_ECC_NO_SUPPORTED_CURVES = -(321);    
    GNUTLS_E_ECC_UNSUPPORTED_CURVE = -(322);    
    GNUTLS_E_PKCS11_REQUESTED_OBJECT_NOT_AVAILBLE = -(323);    
    GNUTLS_E_CERTIFICATE_LIST_UNSORTED = -(324);    
    GNUTLS_E_ILLEGAL_PARAMETER = -(325);    
    GNUTLS_E_NO_PRIORITIES_WERE_SET = -(326);    
    GNUTLS_E_X509_UNSUPPORTED_EXTENSION = -(327);    
    GNUTLS_E_SESSION_EOF = -(328);    
    GNUTLS_E_TPM_ERROR = -(329);    
    GNUTLS_E_TPM_KEY_PASSWORD_ERROR = -(330);    
    GNUTLS_E_TPM_SRK_PASSWORD_ERROR = -(331);    
    GNUTLS_E_TPM_SESSION_ERROR = -(332);    
    GNUTLS_E_TPM_KEY_NOT_FOUND = -(333);    
    GNUTLS_E_TPM_UNINITIALIZED = -(334);    
    GNUTLS_E_TPM_NO_LIB = -(335);    
    GNUTLS_E_NO_CERTIFICATE_STATUS = -(340);    
    GNUTLS_E_OCSP_RESPONSE_ERROR = -(341);    
    GNUTLS_E_RANDOM_DEVICE_ERROR = -(342);    
    GNUTLS_E_AUTH_ERROR = -(343);    
    GNUTLS_E_NO_APPLICATION_PROTOCOL = -(344);    
    GNUTLS_E_SOCKETS_INIT_ERROR = -(345);    
    GNUTLS_E_KEY_IMPORT_FAILED = -(346);    
  {GNUTLS_A_INAPPROPRIATE_FALLBACK }
    GNUTLS_E_INAPPROPRIATE_FALLBACK = -(347);    
    GNUTLS_E_CERTIFICATE_VERIFICATION_ERROR = -(348);    
    GNUTLS_E_SELF_TEST_ERROR = -(400);    
    GNUTLS_E_NO_SELF_TEST = -(401);    
    GNUTLS_E_LIB_IN_ERROR_STATE = -(402);    
    GNUTLS_E_PK_GENERATION_ERROR = -(403);    
    GNUTLS_E_IDNA_ERROR = -(404);    
    GNUTLS_E_NEED_FALLBACK = -(405);    
    GNUTLS_E_UNIMPLEMENTED_FEATURE = -(1250);    
    GNUTLS_E_APPLICATION_ERROR_MAX = -(65000);    
    GNUTLS_E_APPLICATION_ERROR_MIN = -(65500);    

  DefaultCerts : PChar =  '/etc/ssl/certs/ca-certificates.crt';

Var
  sock : TInetSocket;
  ret,sd,ii : integer;
  session : tgnutls_session_t;
  buf : Array[0..MAX_BUF] of char;
  cred : tgnutls_certificate_credentials_t;
  errptr,desc : pchar;
  S : String;
  
begin
  LoadGNutls();
  gnutls_global_init();
  ret := gnutls_certificate_allocate_credentials (@cred);
  if (ret <> GNUTLS_E_SUCCESS) then
    begin
    writeln(stderr, 'error: gnutls_certificate_allocate_credentials: ', gnutls_strerror(ret));
    halt(1);
    end;
  ret := gnutls_certificate_set_x509_trust_file(cred, defaultcerts, GNUTLS_X509_FMT_PEM);
  if (ret = 0) then
    begin
    writeln(stderr, 'error: no certificates found in:', defaultcerts);
    halt(1);
    end
  else if (ret < 0) then
    begin
    writeln(stderr, 'error: gnutls_certificate_set_x509_trust_files(',defaultcerts,'): ',
	     gnutls_strerror(ret));
    halt(1);
    end;

  gnutls_init(@session, GNUTLS_CLIENT);
//  gnutls_priority_set_direct(session,'PERFORMANCE:+ANON-ECDH:+ANON-DH',Nil);

  ret := gnutls_priority_set_direct(session, 'NORMAL', @errptr);
  if (ret <> GNUTLS_E_SUCCESS) then
    begin
    writeln(stderr, 'error: gnutls_priority_set_direct: ',gnutls_strerror(ret) , ' error: at: ', errptr);
    halt(1);
    end;

  ret := gnutls_credentials_set(session, GNUTLS_CRD_CERTIFICATE, cred);
  if (ret <> GNUTLS_E_SUCCESS) then
    begin
    writeln(stderr, 'error: gnutls_credentials_set: ', gnutls_strerror(ret));
    halt(1);
    end;

  Sock:=TINetSocket.Create('www.freepascal.org',443);
  gnutls_transport_set_int(session, Sock.Handle);
  gnutls_handshake_set_timeout(session,GNUTLS_DEFAULT_HANDSHAKE_TIMEOUT);

  Repeat
    ret:=gnutls_handshake(session);
  until (ret>0)  or (gnutls_error_is_fatal(ret) <> 0);
  if (ret < 0) then
    begin
    writeln(stderr, '*** Handshake failed');
    gnutls_perror(ret);
    end
  else  
    begin
    desc := gnutls_session_get_desc(session);
    writeln('- Session info: ', desc);
//    gnutls_free(desc);
    end;
  gnutls_record_send(session, @MSG[1], length(MSG));
  ret := gnutls_record_recv(session, @buf, MAX_BUF);
  if (ret=0) then
    writeln('- Peer has closed the TLS connection\n')
  else if ((ret < 0) and (gnutls_error_is_fatal(ret) = 0)) then
    writeln(stderr, '*** Warning: ', gnutls_strerror(ret))
  else if (ret < 0) then
    Writeln(stderr, '*** Error: ', gnutls_strerror(ret))
  else if (ret > 0) then
      begin
      writeln('- Received %d bytes: ', ret);
      SetLength(S,Ret);
      Move(Buf[0],S[1],Ret);
      Writeln(S);
      gnutls_bye(session, GNUTLS_SHUT_RDWR);
      end;
  Sock.Free;
  gnutls_deinit(session);
  gnutls_certificate_free_credentials(cred);
  gnutls_global_deinit();
  FreeGnuTLS;
end.
