with System;
with RFLX.SPDM;
with RFLX.SPDM_Responder.Session;
with RFLX.SPDM_Responder.Digests_Data;
with RFLX.SPDM_Responder.Signature;
#if FEATURE_KEY_EXCHANGE then
with RFLX.SPDM_Responder.Exchange_Data;
with RFLX.SPDM_Responder.Hash;
#end if;
with RFLX.SPDM_Responder.Opaque_Data;
with RFLX.SPDM.Certificate_Response;
with RFLX.SPDM.Nonce;
with RFLX.SPDM.DMTF_Measurement_Field;
with RFLX.RFLX_Types;

package SPDM_C_Responder with
   SPARK_Mode,
   Elaborate_Body
is

   --  Implementation defined.
   --
   --  This record can be defined freely by the implementer to
   --  hold any value required by the platform. Memory management
   --  for this struct is also the responsibility of the
   --  implementer.
   type Context is new RFLX.SPDM_Responder.Session.Context with record
      Instance : System.Address := System.Null_Address;
   end record;

   --  Ensure initialization of   --  instance.
   --
   --  This function is not part of the required API. It exists to
   --  allow the platform performing initialization tasks before
   --  the state machine calls any other platform function. If the instance
   --  pointer is initialized by other means this function can
   --  be removed.
   --
   --  @param Ctx Context.
   procedure Plat_Initialize (Ctx : in out Context);
#if FEATURE_KEY_EXCHANGE then
   --
   --  Check if the state machine is currently in a secure session.
   --
   --  This function is a helper for the platform to check if the
   --  state machine is in a secure session (e.g. to set the correct
   --  flags in the transport protocoll).
   --
   --  @param Ctx Context.
   --  @return Secure session.
   function Plat_Is_Secure_Session (Ctx : Context) return Boolean;
#end if;

   --  Return CT exponent (DSP0274_1.1.0 [178]).
   --
   --  @param Ctx Context.
   --  @param result CT exponent.
   overriding
   procedure Plat_Cfg_CT_Exponent
      (Ctx    : in out Context;
       Result :    out RFLX.SPDM.CT_Exponent);

   --  Check if measurements without restart are supported (DSP0274_1.1.0 [178]).
   --
   --  @param Ctx Context.
   --  @param Result If True, capability is set.
   overriding
   procedure Plat_Cfg_Cap_Meas_Fresh
      (Ctx    : in out Context;
       Result :    out Boolean);

   --  Check if measurements are supported (DSP0274_1.1.0 [178]).
   --
   --  @param instance Platform instance.
   --  @param Result Measurement capability.
   overriding
   procedure Plat_Cfg_Cap_Meas
      (Ctx    : in out Context;
       Result :    out RFLX.SPDM.Meas_Cap);

   --  Check if challenge authentication is supported (DSP0274_1.1.0 [178]).
   --
   --  @param Ctx Context.
   --  @param Result If True, capability is set.
   overriding
   procedure Plat_Cfg_Cap_Chal
      (Ctx    : in out Context;
       Result :    out Boolean);

   --  Check if digests and certificate responses are supported (DSP0274_1.1.0 [178]).
   --
   --  @param Ctx Context.
   --  @param Result If True, capability is set.
   overriding
   procedure Plat_Cfg_Cap_Cert
      (Ctx    : in out Context;
       Result :    out Boolean);

   --  Check if key update is supported (DSP0274_1.1.0 [178]).
   --
   --  @param Ctx Context.
   --  @param Result If True, capability is set.
   overriding
   procedure Plat_Cfg_Cap_Cache
      (Ctx    : in out Context;
       Result :    out Boolean);

   --  Check if key update is supported (DSP0274_1.1.0 [178]).
   --
   --  @param Ctx Context.
   --  @param Result If True, capability is set.
   overriding
   procedure Plat_Cfg_Cap_Key_Upd
      (Ctx    : in out Context;
       Result :    out Boolean);

   --  Check if heartbeat messages are supported (DSP0274_1.1.0 [178]).
   --
   --  @param Ctx Context.
   --  @param Result If True, capability is set.
   overriding
   procedure Plat_Cfg_Cap_Hbeat
      (Ctx    : in out Context;
       Result :    out Boolean);

   --  Check if encapsulated messages are supported (DSP0274_1.1.0 [178]).
   --
   --  @param Ctx Context.
   --  @param Result If True, capability is set.
   overriding
   procedure Plat_Cfg_Cap_Encap
      (Ctx    : in out Context;
       Result :    out Boolean);

   --  Check if mutual authentication is supported (DSP0274_1.1.0 [178]).
   --
   --  @param Ctx Context.
   --  @param Result If True, capability is set.
   overriding
   procedure Plat_Cfg_Cap_Mut_Auth
      (Ctx    : in out Context;
       Result :    out Boolean);

   --  Check if the public key of the responder was provisioned to the requester (DSP0274_1.1.0 [178]).
   --
   --  @param Ctx Context.
   --  @param Result If True, capability is set.
   overriding
   procedure Plat_Cfg_Cap_Pub_Key_ID
      (Ctx    : in out Context;
       Result :    out Boolean);
#if FEATURE_KEY_EXCHANGE then
   --
   --  Check if message authentication is supported (DSP0274_1.1.0 [178]).
   --
   --  @param Ctx Context.
   --  @param Result If True, capability is set.
   overriding
   procedure Plat_Cfg_Cap_MAC
      (Ctx    : in out Context;
       Result :    out Boolean);

   --  Check if message encryption is supported (DSP0274_1.1.0 [178]).
   --
   --  @param Ctx Context.
   --  @param Result If True, capability is set.
   overriding
   procedure Plat_Cfg_Cap_Encrypt
      (Ctx    : in out Context;
       Result :    out Boolean);

   --  Check if pre-shared keys are supported (DSP0274_1.1.0 [178]).
   --
   --  @param Ctx Context.
   --  @param Result If True, capability is set.
   overriding
   procedure Plat_Cfg_Cap_PSK
      (Ctx    : in out Context;
       Result :    out RFLX.SPDM.PSK_Resp_Cap);

   --  Check if key exchange is supported (DSP0274_1.1.0 [178]).
   --
   --  @param Ctx Context.
   --  @param Result If True, capability is set.
   overriding
   procedure Plat_Cfg_Cap_Key_Ex
      (Ctx    : in out Context;
       Result :    out Boolean);

   --  Check if handshake without encryption or authentication is supported (DSP0274_1.1.0 [178]).
   --
   --  @param Ctx Context.
   --  @param Result If True, capability is set.
   overriding
   procedure Plat_Cfg_Cap_Handshake_In_The_Clear
      (Ctx    : in out Context;
       Result :    out Boolean);
#end if;
   --
   --  Select measurement hash algorithm (DSP0274_1.1.0 [185]).
   --
   --  The arguments describe the hash algorithms supported by the requester.
   --  This function should select one of the provided algorithms.
   --
   --  @param Ctx Context.
   --  @param TPM_ALG_SHA_256 SHA-256 supported and requested.
   --  @param TPM_ALG_SHA_384 SHA-384 supported and requested.
   --  @param TPM_ALG_SHA_512 SHA-512 supported and requested.
   --  @param TPM_ALG_SHA3_256 SHA3-256 supported and requested.
   --  @param TPM_ALG_SHA3_384 SHA3-384 supported and requested.
   --  @param TPM_ALG_SHA3_512 SHA3-512 supported and requested.
   --  @param RAW_BIT_STREAMS_ONLY Raw bit streams supported and requested.
   --  @param Result Selected algorithm.
   overriding
   procedure Plat_Cfg_Sel_Measurement_Hash_Algo
      (Ctx                  : in out Context;
       TPM_ALG_SHA_256      :        Boolean;
       TPM_ALG_SHA_384      :        Boolean;
       TPM_ALG_SHA_512      :        Boolean;
       TPM_ALG_SHA3_256     :        Boolean;
       TPM_ALG_SHA3_384     :        Boolean;
       TPM_ALG_SHA3_512     :        Boolean;
       Raw_Bit_Streams_Only :        Boolean;
       Result               :    out RFLX.SPDM.Measurement_Hash_Algo);

   --  Select base asymmetric algorithm (DSP0274_1.1.0 [185]).
   --
   --  The arguments describe the signature algorithms supported by the requester.
   --  This function should select one of the provided algorithms.
   --
   --  @param Ctx Context.
   --  @param TPM_ALG_ECDSA_ECC_NIST_P384 ECDSA-ECC-384 supported and requested.
   --  @param TPM_ALG_RSAPSS_4096 RSAPSS-4096 supported and requested.
   --  @param TPM_ALG_RSASSA_4096 RSASSA-4096 supported and requested.
   --  @param TPM_ALG_ECDSA_ECC_NIST_P256 ECDSA-ECC-256 supported and requested.
   --  @param TPM_ALG_RSAPSS_3072 RSAPSS-3072 supported and requested.
   --  @param TPM_ALG_RSASSA_3072 RSASSA-3072 supported and requested.
   --  @param TPM_ALG_RSAPSS_2048 RSAPSS-2048 supported and requested.
   --  @param TPM_ALG_RSASSA_2048 RSASSA-2048 supported and requested.
   --  @param TPM_ALG_ECDSA_ECC_NIST_P521 ECDSA-ECC-521 supported and requested.
   --  @param Result Selected algorithm.
   overriding
   procedure Plat_Cfg_Sel_Base_Asym_Algo
      (Ctx                         : in out Context;
       TPM_ALG_ECDSA_ECC_NIST_P384 :        Boolean;
       TPM_ALG_RSAPSS_4096         :        Boolean;
       TPM_ALG_RSASSA_4096         :        Boolean;
       TPM_ALG_ECDSA_ECC_NIST_P256 :        Boolean;
       TPM_ALG_RSAPSS_3072         :        Boolean;
       TPM_ALG_RSASSA_3072         :        Boolean;
       TPM_ALG_RSAPSS_2048         :        Boolean;
       TPM_ALG_RSASSA_2048         :        Boolean;
       TPM_ALG_ECDSA_ECC_NIST_P521 :        Boolean;
       Result                      :    out RFLX.SPDM.Base_Asym_Algo);

   --  Select base hash algorithm (DSP0274_1.1.0 [185]).
   --
   --  The arguments describe the hash algorithms supported by the requester.
   --  This function should select one of the provided algorithms.
   --
   --  @param Ctx Context.
   --  @param TPM_ALG_SHA_256 SHA-256 supported and requested.
   --  @param TPM_ALG_SHA_384 SHA-384 supported and requested.
   --  @param TPM_ALG_SHA_512 SHA-512 supported and requested.
   --  @param TPM_ALG_SHA3_256 SHA3-256 supported and requested.
   --  @param TPM_ALG_SHA3_384 SHA3-384 supported and requested.
   --  @param TPM_ALG_SHA3_512 SHA3-512 supported and requested.
   --  @param Result Selected algorithm.
   overriding
   procedure Plat_Cfg_Sel_Base_Hash_Algo
      (Ctx              : in out Context;
       TPM_ALG_SHA_256  :        Boolean;
       TPM_ALG_SHA_384  :        Boolean;
       TPM_ALG_SHA_512  :        Boolean;
       TPM_ALG_SHA3_256 :        Boolean;
       TPM_ALG_SHA3_384 :        Boolean;
       TPM_ALG_SHA3_512 :        Boolean;
       Result           :    out RFLX.SPDM.Base_Hash_Algo);
#if FEATURE_KEY_EXCHANGE then
   --
   --  Select the Diffie-Hellman Ephemeral group (DSP0274_1.1.0 [189]).
   --
   --  The arguments describe the group supported by the requester.
   --  This function should select one of the provided groups.
   --
   --  @param Ctx Context.
   --  @param Req_SecP521r1 SECP521R1 supported and requested.
   --  @param Req_SecP384r1 SECP384R1 supported and requested.
   --  @param Req_SecP256r1 SECP256R1 supported and requested.
   --  @param Req_FFDHE4096 FFDHE4092 supported and requested.
   --  @param Req_FFDHE3072 FFDHE3072 supported and requested.
   --  @param Req_FFDHE2048 FFDHE2048 supported and requested.
   --  @param Result Selected algorithm.
   overriding
   procedure Plat_Cfg_Sel_DHE
      (Ctx           : in out Context;
       Req_SecP521r1 :        Boolean;
       Req_SecP384r1 :        Boolean;
       Req_SecP256r1 :        Boolean;
       Req_FFDHE4096 :        Boolean;
       Req_FFDHE3072 :        Boolean;
       Req_FFDHE2048 :        Boolean;
       Result        :    out RFLX.SPDM_Responder.DHE_Algo);

   --  Select the AEAD algorithm (DSP0274_1.1.0 [190]).
   --
   --  The arguments describe the algorithm supported by the requester.
   --  This function should select one of the provided algorithms.
   --
   --  @param Ctx Context.
   --  @param Req_ChaCha20_Poly1305 CHACHA20-POLY135 supported and requested.
   --  @param Req_AES_256_GCM AES-256-GCM supported and requested.
   --  @param Req_AES_128_GCM AES-128-GCM supported and requested.
   --  @param Result Selected algorithm.
   overriding
   procedure Plat_Cfg_Sel_AEAD
      (Ctx                   : in out Context;
       Req_ChaCha20_Poly1305 :        Boolean;
       Req_AES_256_GCM       :        Boolean;
       Req_AES_128_GCM       :        Boolean;
       Result                :    out RFLX.SPDM_Responder.AEAD_Algo);
#end if;
   --
   --  Select base asymmetric algorithm (DSP0274_1.1.0 [191]).
   --
   --  The arguments describe the key signature algorithms supported by the requester.
   --  This function should select one of the provided algorithms.
   --
   --  @param Ctx Context.
   --  @param Req_TPM_ALG_ECDSA_ECC_NIST_P384 ECDSA-ECC-384 supported and requested.
   --  @param Req_TPM_ALG_RSAPSS_4096 RSAPSS-4096 supported and requested.
   --  @param Req_TPM_ALG_RSASSA_4096 RSASSA-4096 supported and requested.
   --  @param Req_TPM_ALG_ECDSA_ECC_NIST_P256 ECDSA-ECC-256 supported and requested.
   --  @param Req_TPM_ALG_RSAPSS_3072 RSAPSS-3072 supported and requested.
   --  @param Req_TPM_ALG_RSASSA_3072 RSASSA-3072 supported and requested.
   --  @param Req_TPM_ALG_RSAPSS_2048 RSAPSS-2048 supported and requested.
   --  @param Req_TPM_ALG_RSASSA_2048 RSASSA-2048 supported and requested.
   --  @param Req_TPM_ALG_ECDSA_ECC_NIST_P521 ECDSA-ECC-521 supported and requested.
   --  @param Result Selected algorithm.
   overriding
   procedure Plat_Cfg_Sel_RBAA
      (Ctx                             : in out Context;
       Req_TPM_ALG_ECDSA_ECC_NIST_P384 :        Boolean;
       Req_TPM_ALG_RSAPSS_4096         :        Boolean;
       Req_TPM_ALG_RSASSA_4096         :        Boolean;
       Req_TPM_ALG_ECDSA_ECC_NIST_P256 :        Boolean;
       Req_TPM_ALG_RSAPSS_3072         :        Boolean;
       Req_TPM_ALG_RSASSA_3072         :        Boolean;
       Req_TPM_ALG_RSAPSS_2048         :        Boolean;
       Req_TPM_ALG_RSASSA_2048         :        Boolean;
       Req_TPM_ALG_ECDSA_ECC_NIST_P521 :        Boolean;
       Result                          :    out RFLX.SPDM.Base_Asym_Algo);

   --  Get digests for digests response (DSP0274_1.1.0 [232]).
   --
   --  @param Ctx Context.
   --  @param Result Digests data.
   overriding
   procedure Plat_Get_Digests_Data
      (Ctx    : in out Context;
       Result :    out RFLX.SPDM_Responder.Digests_Data.Structure);

   --  Validate an incoming certificate request (DSP0274_1.1.0 [238]).
   --
   --  @param Ctx Context.
   --  @param Slot Certificate slot number.
   --  @param Offset Certificate portion offset.
   --  @param Length Certificate portion length.
   --  @param Result Success.
   overriding
   procedure Plat_Valid_Certificate_Request
      (Ctx    : in out Context;
       Slot   :        RFLX.SPDM.Slot;
       Offset :        RFLX.SPDM.Offset;
       Length :        RFLX.SPDM.Length_16;
       Result :    out Boolean);

   --  Provide requested certificate chain.
   --
   --  @param Ctx Context.
   --  @param Slot Requested certificate slot.
   --  @param Offset Offset in the certificate chain.
   --  @param Length Contains a pointer to the maximum size of data. On return the size
   --               must be changed to the actual size of the data copied to data.
   --  @param Result Certificate response including certificate data, portion length and
   --               remainder length.
   overriding
   procedure Plat_Get_Certificate_Response
      (Ctx    : in out Context;
       Slot   :        RFLX.SPDM.Slot;
       Offset :        RFLX.SPDM.Offset;
       Length :        RFLX.SPDM.Length_16;
       Result :    out RFLX.SPDM.Certificate_Response.Structure);

   --  Get the number of measurement indices (DSP0274_1.1.0 [327]).
   --
   --  Returns the number of indices available from the responder. This
   --  may be zero if none are available and up to 254.
   --
   --  @param Ctx Context.
   --  @param Result Number of measurements.
   overriding
   procedure Plat_Get_Number_Of_Indices
      (Ctx    : in out Context;
       Result :    out RFLX.SPDM.Measurement_Count);
#if FEATURE_KEY_EXCHANGE then
   --  Get the number of measurement indices that include the trusted computing base (DSP0274_1.1.0 [422]).
   --
   --  Otherwise it has the same behaviour as spdm_platform_get_number_of_indices.
   --
   --  @param Ctx Context.
   --  @param Result Number of measurements in TCB.
   overriding
   procedure Plat_Get_Number_Of_Indices_TCB
      (Ctx    : in out Context;
       Result :    out RFLX.SPDM.Measurement_Count);
#end if;
   --  Generate a nonce for cryptographic operations.
   --
   --  The platform must always keep the latest generated nonce and should
   --  add it to the transcript when spdm_platform_update_transcript_nonce
   --  is called. Only after this function is called the nonce can be marked
   --  as valid.
   --
   --  @param Ctx Context.
   --  @param Result Nonce containing 32 byte long buffer.
   overriding
   procedure Plat_Get_Nonce (Ctx    : in out Context;
                             Result :    out RFLX.SPDM.Nonce.Structure);

   --  Return a DMTF measurement field (DSP0274_1.1.0 [335]).
   --
   --  @param Ctx Context.
   --  @param index Requested measurement index.
   --  @param Result Complete DMTF measurement field.
   overriding
   procedure Plat_Get_DMTF_Measurement_Field (Ctx    : in out Context;
                                              Index  :        RFLX.SPDM.Index;
                                              Result :    out RFLX.SPDM.DMTF_Measurement_Field.Structure);

   --  Provide opaque data for the measurement response (DSP0274_1.1.0 [327]).
   --
   --  @param Ctx Context.
   --  @param Result Opaque data.
   overriding
   procedure Plat_Get_Meas_Opaque_Data (Ctx    : in out Context;
                                        Result :    out RFLX.SPDM_Responder.Opaque_Data.Structure);

   --  Register a new transcript.
   --
   --  Registers a new transcript with the platform. The returned ID must be
   --  used on all subsequent operations on the same transcript.
   --
   --  @param Ctx Context.
   --  @param kind Transcript kind.
   --  @param Result Transcript ID. On success spdm_platform_valid_transcript_id
   --         must return true on this ID.
   overriding
   procedure Plat_Get_New_Transcript (Ctx    : in out Context;
                                      Kind   :        RFLX.SPDM_Responder.Transcript_Kind;
                                      Result :    out RFLX.SPDM_Responder.Transcript_ID);

   --  Check if a transcript ID is valid.
   --
   --  @param Ctx Context.
   --  @param Transcript Transcript ID.
   --  @param Result Success.
   overriding
   procedure Plat_Valid_Transcript_ID (Ctx        : in out Context;
                                       Transcript :        RFLX.SPDM_Responder.Transcript_ID;
                                       Result     :    out Boolean);

   --  Reset an already registered transcript.
   --
   --  Resets a transcript on the platform. This operation may change the
   --  transcript kind, too. The returned transcript ID may be different from
   --  the provided one. It has the same behaviour as spdm_platform_get_new_transcript
   --  except that it allows reusing an existing resource.
   --
   --  @param Ctx Context.
   --  @param Transcript Old transcript ID.
   --  @param Kind Transcript kind for the new transcript.
   --  @param Result New transcript ID.
   overriding
   procedure Plat_Reset_Transcript (Ctx        : in out Context;
                                    Transcript :        RFLX.SPDM_Responder.Transcript_ID;
                                    Kind       :        RFLX.SPDM_Responder.Transcript_Kind;
                                    Result     :    out RFLX.SPDM_Responder.Transcript_ID);

   --  Append a chunk of data to the transcript.
   --
   --  @param Ctx Context.
   --  @param Transcript Transcript ID.
   --  @param Data Transcript data to be appended.
   --  @param Offset Offset in data.
   --  @param Length Length of data to be appended to the transcript,
   --               Length + Offset is less or equal to the size of data.
   --  @param Result Success.
   overriding
   procedure Plat_Update_Transcript (Ctx        : in out Context;
                                     Transcript :        RFLX.SPDM_Responder.Transcript_ID;
                                     Data       :        RFLX.RFLX_Types.Bytes;
                                     Offset     :        RFLX.SPDM.Length_16;
                                     Length     :        RFLX.SPDM.Length_16;
                                     Result     :    out Boolean);

   --  Append the latest generated nonce to the transcript.
   --
   --  Append the latest nonce generated by spdm_platform_get_nonce to the
   --  transcript. The nonce must be marked as invalid after this operation.
   --  If the nonce is already marked as invalid when this function is called
   --  the operation must fail.
   --
   --  @param Ctx Context.
   --  @param Transcript Transcript ID.
   --  @param Result Success.
   overriding
   procedure Plat_Update_Transcript_Nonce (Ctx        : in out Context;
                                           Transcript :        RFLX.SPDM_Responder.Transcript_ID;
                                           Result     :    out Boolean);

   --  Generate signature from current transcript.
   --
   --  Generate the signature from the current state of the transcript. This
   --  does not invalidate or reset the transcript.
   --
   --  @param Ctx Context.
   --  @param Transcript Transcript ID.
   --  @param Slot Slot ID of the signing key.
   --  @param Result Signature.
   overriding
   procedure Plat_Get_Signature (Ctx        : in out Context;
                                 Transcript :        RFLX.SPDM_Responder.Transcript_ID;
                                 Slot       :        RFLX.SPDM.Slot;
                                 Result     :    out RFLX.SPDM_Responder.Signature.Structure);
#if FEATURE_KEY_EXCHANGE then
   --  Handle the exchange data of the key exchange (DSP0274_1.1.0 [421]).
   --
   --  @param Ctx Context.
   --  @param Exchange_Data Data sent by the requester.
   --  @param Result Exchange data to be send with the response.
   overriding
   procedure Plat_Get_Exchange_Data (Ctx           : in out Context;
                                     Exchange_Data :        RFLX.RFLX_Types.Bytes;
                                     Result        :    out RFLX.SPDM_Responder.Exchange_Data.Structure);

   --  Get heartbeat period (DSP0274_1.1.0 [422]).
   --
   --  @param Ctx Context.
   --  @param Result Heartbeat period.
   overriding
   procedure Plat_Get_Heartbeat_Period (Ctx    : in out Context;
                                        Result :    out RFLX.SPDM.Heartbeat_Period);

   --  Check session ID sent by the requester for validity (DSP0274_1.1.0 [421]).
   --
   --  @param Ctx Context.
   --  @param Req_Session_ID Requester session ID.
   --  @param Result If True, the ID is valid.
   overriding
   procedure Plat_Valid_Session_ID (Ctx            : in out Context;
                                    Req_Session_ID :        RFLX.SPDM.Session_ID;
                                    Result         :    out Boolean);

   --  Get session ID for key exchange response (DSP0274_1.1.0 [422]).
   --
   --  @param Ctx Context.
   --  @param Req_Session_ID Requester session ID
   --  @param Result Response session ID.
   overriding
   procedure Plat_Get_Session_ID (Ctx            : in out Context;
                                  Req_Session_ID :        RFLX.SPDM.Session_ID;
                                  Result         :    out RFLX.SPDM.Session_ID);

   --  Request mutual authentication in key exchange response (DSP0274_1.1.0 [422]).
   --
   --  Returning true only enables regular mutual authentication. Encapsulated
   --  and implicit mutual authentication are not supported.
   --
   --  @param Ctx Context.
   --  @param Result Mutual auth requested.
   overriding
   procedure Plat_Use_Mutual_Auth (Ctx    : in out Context;
                                   Result :    out Boolean);

   --  Get the hash over the measurement summary (DSP0274_1.1.0 [422]).
   --
   --  @param Ctx Context.
   --  @param Data Measurement summary data.
   --  @param Result Measurement summary hash.
   overriding
   procedure Plat_Get_Summary_Hash (Ctx    : in out Context;
                                    Data   :        RFLX.RFLX_Types.Bytes;
                                    Result :    out RFLX.SPDM_Responder.Hash.Structure);

   --  Append the selected certificate chain to the transcript.
   --
   --  @param Ctx Context.
   --  @param Transcript Transcript ID.
   --  @param Slot Slot ID for certificate selection.
   --  @param Result Success.
   overriding
   procedure Plat_Update_Transcript_Cert (Ctx        : in out Context;
                                          Transcript :        RFLX.SPDM_Responder.Transcript_ID;
                                          Slot       :        RFLX.SPDM.Slot;
                                          Result     :    out Boolean);

   --  Handle key exchange opaque data (DSP0274_1.1.0 [422]).
   --
   --  @param Ctx Context.
   --  @param Request_Data Opaque data sent by the requester.
   --  @param Result Opaque data to be sent with the response.
   overriding
   procedure Plat_Get_Key_Ex_Opaque_Data (Ctx          : in out Context;
                                          Request_Data :        RFLX.RFLX_Types.Bytes;
                                          Result       :    out RFLX.SPDM_Responder.Opaque_Data.Structure);

   --  Generate the responder verify data for key exchange (DSP0274_1.1.0 [422]).
   --
   --  @param Ctx Context.
   --  @param Result Key exchange verify data.
   overriding
   procedure Plat_Get_Key_Ex_Verify_Data (Ctx    : in out Context;
                                          Result :    out RFLX.SPDM_Responder.Hash.Structure);

   --  Validate the finish signature sent by the requester (DSP0274_1.1.0 [432]).
   --
   --  @param Ctx Context.
   --  @param Transcript Transcript ID.
   --  @param Signature Signature sent by the requester.
   --  @param Slot Slot ID of the signing key.
   --  @param Result Success.
   overriding
   procedure Plat_Validate_Finish_Signature (Ctx        : in out Context;
                                             Transcript :        RFLX.SPDM_Responder.Transcript_ID;
                                             Signature  :        RFLX.RFLX_Types.Bytes;
                                             Slot       :        RFLX.SPDM.Slot;
                                             Result     :    out Boolean);

   --  Validate the finish HMAC sent by the requester (DSP0274_1.1.0 [432]).
   --
   --  @param Ctx Context.
   --  @param Transcript Transcript ID.
   --  @param HMAC HMAC sent by the requester.
   --  @param Slot Slot ID used for the HMAC generation.
   --  @param Result Success.
   overriding
   procedure Plat_Validate_Finish_HMAC (Ctx        : in out Context;
                                        Transcript :        RFLX.SPDM_Responder.Transcript_ID;
                                        HMAC       :        RFLX.RFLX_Types.Bytes;
                                        Slot       :        RFLX.SPDM.Slot;
                                        Result     :    out Boolean);

   --  Generate the responder verify data finish (DSP0274_1.1.0 [433]).
   --
   --  @param Ctx Context.
   --  @param Transcript Transcript ID.
   --  @param Slot Slot ID for the used key.
   --  @param Result Responder verify data for finish response.
   overriding
   procedure Plat_Get_Finish_Verify_Data (Ctx        : in out Context;
                                          Transcript :        RFLX.SPDM_Responder.Transcript_ID;
                                          Slot       :        RFLX.SPDM.Slot;
                                          Result     :    out RFLX.SPDM_Responder.Hash.Structure);

   --  Set the current session phase.
   --
   --  Set the current session phase to the phase passed as argument. If
   --  an error occurs the session phase is set to the error value, otherwise
   --  it's set to the requested phase.
   --
   --  @param Ctx Context.
   --  @param phase Requested session phase.
   --  @param Result Updated session phase.
   overriding
   procedure Plat_Set_Session_Phase (Ctx    : in out Context;
                                     Phase  :        RFLX.SPDM_Responder.Session_Phase;
                                     Result :    out RFLX.SPDM_Responder.Session_Phase);

   --  Do a key update operation as.
   --
   --  @param Ctx Context.
   --  @param Operation Key update operation.
   --  @param Tag Key update tag.
   --  @result Success.
   overriding
   procedure Plat_Key_Update (Ctx       : in out Context;
                              Operation :        RFLX.SPDM.Key_Operation;
                              Tag       :        RFLX.SPDM.Key_Update_Tag;
                              Result    :    out Boolean);

   --  Initialization function for empty hashes.
   --
   --  @param Ctx Context.
   --  @param Length Length of the zeroed hash.
   --  @param Result Zeroed out hash data.
   overriding
   procedure Null_Hash (Ctx    : in out Context;
                        Length :        RFLX.SPDM.Hash_Length;
                        Result :    out RFLX.SPDM_Responder.Hash.Structure);
#end if;
   --  Initialization function for empty signatures.
   --
   --  @param Ctx Context.
   --  @param Length Length of the zeroed signature.
   --  @param Result Zeroed out signature data.
   overriding
   procedure Null_Signature (Ctx    : in out Context;
                             Length :        RFLX.SPDM.Signature_Length;
                             Result :    out RFLX.SPDM_Responder.Signature.Structure);
end SPDM_C_Responder;
