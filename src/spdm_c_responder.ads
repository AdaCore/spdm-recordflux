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

   type Context is new RFLX.SPDM_Responder.Session.Context with record
      Instance : System.Address := System.Null_Address;
   end record;

   procedure Plat_Initialize (Ctx : in out Context);
#if FEATURE_KEY_EXCHANGE then
   function Plat_Is_Secure_Session (Ctx : Context) return Boolean;
#end if;

   overriding
   procedure Plat_Cfg_CT_Exponent
      (Ctx    : in out Context;
       Result :    out RFLX.SPDM.CT_Exponent);

   overriding
   procedure Plat_Cfg_Cap_Meas_Fresh
      (Ctx    : in out Context;
       Result :    out Boolean);

   overriding
   procedure Plat_Cfg_Cap_Meas
      (Ctx    : in out Context;
       Result :    out RFLX.SPDM.Meas_Cap);

   overriding
   procedure Plat_Cfg_Cap_Chal
      (Ctx    : in out Context;
       Result :    out Boolean);

   overriding
   procedure Plat_Cfg_Cap_Cert
      (Ctx    : in out Context;
       Result :    out Boolean);

   overriding
   procedure Plat_Cfg_Cap_Cache
      (Ctx    : in out Context;
       Result :    out Boolean);

   overriding
   procedure Plat_Cfg_Cap_Key_Upd
      (Ctx    : in out Context;
       Result :    out Boolean);

   overriding
   procedure Plat_Cfg_Cap_Hbeat
      (Ctx    : in out Context;
       Result :    out Boolean);

   overriding
   procedure Plat_Cfg_Cap_Encap
      (Ctx    : in out Context;
       Result :    out Boolean);

   overriding
   procedure Plat_Cfg_Cap_Mut_Auth
      (Ctx    : in out Context;
       Result :    out Boolean);

   overriding
   procedure Plat_Cfg_Cap_Pub_Key_ID
      (Ctx    : in out Context;
       Result :    out Boolean);
#if FEATURE_KEY_EXCHANGE then
   overriding
   procedure Plat_Cfg_Cap_MAC
      (Ctx    : in out Context;
       Result :    out Boolean);

   overriding
   procedure Plat_Cfg_Cap_Encrypt
      (Ctx    : in out Context;
       Result :    out Boolean);

   overriding
   procedure Plat_Cfg_Cap_PSK
      (Ctx    : in out Context;
       Result :    out RFLX.SPDM.PSK_Resp_Cap);

   overriding
   procedure Plat_Cfg_Cap_Key_Ex
      (Ctx    : in out Context;
       Result :    out Boolean);

   overriding
   procedure Plat_Cfg_Cap_Handshake_In_The_Clear
      (Ctx    : in out Context;
       Result :    out Boolean);
#end if;
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

   overriding
   procedure Plat_Cfg_Sel_AEAD
      (Ctx                   : in out Context;
       Req_ChaCha20_Poly1305 :        Boolean;
       Req_AES_256_GCM       :        Boolean;
       Req_AES_128_GCM       :        Boolean;
       Result                :    out RFLX.SPDM_Responder.AEAD_Algo);
#end if;
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

   overriding
   procedure Plat_Get_Digests_Data
      (Ctx    : in out Context;
       Algo   :        RFLX.SPDM.Measurement_Hash_Algo;
       Result :    out RFLX.SPDM_Responder.Digests_Data.Structure);

   overriding
   procedure Plat_Valid_Certificate_Request
      (Ctx    : in out Context;
       Slot   :        RFLX.SPDM.Slot;
       Offset :        RFLX.SPDM.Offset;
       Length :        RFLX.SPDM.Length_16;
       Result :    out Boolean);

   overriding
   procedure Plat_Get_Certificate_Response
      (Ctx    : in out Context;
       Slot   :        RFLX.SPDM.Slot;
       Offset :        RFLX.SPDM.Offset;
       Length :        RFLX.SPDM.Length_16;
       Result :    out RFLX.SPDM.Certificate_Response.Structure);

   overriding
   procedure Plat_Get_Number_Of_Indices
      (Ctx    : in out Context;
       Result :    out RFLX.SPDM.Measurement_Count);
#if FEATURE_KEY_EXCHANGE then
   overriding
   procedure Plat_Get_Number_Of_Indices_TCB
      (Ctx    : in out Context;
       Result :    out RFLX.SPDM.Measurement_Count);
#end if;
   overriding
   procedure Plat_Get_Nonce (Ctx    : in out Context;
                             Result :    out RFLX.SPDM.Nonce.Structure);

   overriding
   procedure Plat_Get_DMTF_Measurement_Field (Ctx    : in out Context;
                                              Index  :        RFLX.SPDM.Index;
                                              Result :    out RFLX.SPDM.DMTF_Measurement_Field.Structure);

   overriding
   procedure Plat_Get_Meas_Opaque_Data (Ctx    : in out Context;
                                        Result :    out RFLX.SPDM_Responder.Opaque_Data.Structure);

   overriding
   procedure Plat_Get_New_Hash (Ctx    : in out Context;
                                Result :    out RFLX.SPDM_Responder.Hash_ID);

   overriding
   procedure Plat_Valid_Hash_ID (Ctx    : in out Context;
                                 Hash   :        RFLX.SPDM_Responder.Hash_ID;
                                 Result :    out Boolean);

   overriding
   procedure Plat_Reset_Hash (Ctx    : in out Context;
                              Hash   :        RFLX.SPDM_Responder.Hash_ID;
                              Result :    out RFLX.SPDM_Responder.Hash_ID);

   overriding
   procedure Plat_Update_Hash (Ctx    : in out Context;
                               Hash   :        RFLX.SPDM_Responder.Hash_ID;
                               Data   :        RFLX.RFLX_Types.Bytes;
                               Offset :        RFLX.SPDM.Length_16;
                               Length :        RFLX.SPDM.Length_16;
                               Result :    out Boolean);

   overriding
   procedure Plat_Update_Hash_Nonce (Ctx    : in out Context;
                                     Hash   :        RFLX.SPDM_Responder.Hash_ID;
                                     Result :    out Boolean);

   overriding
   procedure Plat_Get_Signature (Ctx    : in out Context;
                                 Hash   :        RFLX.SPDM_Responder.Hash_ID;
                                 Slot   :        RFLX.SPDM.Slot;
                                 Result :    out RFLX.SPDM_Responder.Signature.Structure);
#if FEATURE_KEY_EXCHANGE then
   overriding
   procedure Plat_Get_Exchange_Data (Ctx           : in out Context;
                                     Exchange_Data :        RFLX.RFLX_Types.Bytes;
                                     Result        :    out RFLX.SPDM_Responder.Exchange_Data.Structure);

   overriding
   procedure Plat_Get_Heartbeat_Period (Ctx    : in out Context;
                                        Result :    out RFLX.SPDM.Heartbeat_Period);

   overriding
   procedure Plat_Valid_Session_ID (Ctx            : in out Context;
                                    Req_Session_ID :        RFLX.SPDM.Session_ID;
                                    Result         :    out Boolean);

   overriding
   procedure Plat_Get_Session_ID (Ctx            : in out Context;
                                  Req_Session_ID :        RFLX.SPDM.Session_ID;
                                  Result         :    out RFLX.SPDM.Session_ID);

   overriding
   procedure Plat_Use_Mutual_Auth (Ctx    : in out Context;
                                   Result :    out Boolean);

   overriding
   procedure Plat_Get_Summary_Hash (Ctx    : in out Context;
                                    Data   :        RFLX.RFLX_Types.Bytes;
                                    Result :    out RFLX.SPDM_Responder.Hash.Structure);

   overriding
   procedure Plat_Update_Hash_Cert (Ctx    : in out Context;
                                    Hash   :        RFLX.SPDM_Responder.Hash_ID;
                                    Slot   :        RFLX.SPDM.Slot;
                                    Result :    out Boolean);

   overriding
   procedure Plat_Get_Key_Ex_Opaque_Data (Ctx          : in out Context;
                                          Request_Data :        RFLX.RFLX_Types.Bytes;
                                          Result       :    out RFLX.SPDM_Responder.Opaque_Data.Structure);

   overriding
   procedure Plat_Get_Key_Ex_Verify_Data (Ctx    : in out Context;
                                          Result :    out RFLX.SPDM_Responder.Hash.Structure);

   overriding
   procedure Plat_Validate_Finish_Signature (Ctx       : in out Context;
                                             Hash      :        RFLX.SPDM_Responder.Hash_ID;
                                             Signature :        RFLX.RFLX_Types.Bytes;
                                             Slot      :        RFLX.SPDM.Slot;
                                             Result    :    out Boolean);

   overriding
   procedure Plat_Validate_Finish_HMAC (Ctx    : in out Context;
                                        Hash   :        RFLX.SPDM_Responder.Hash_ID;
                                        HMAC   :        RFLX.RFLX_Types.Bytes;
                                        Slot   :        RFLX.SPDM.Slot;
                                        Result :    out Boolean);

   overriding
   procedure Plat_Get_Finish_Verify_Data (Ctx    : in out Context;
                                          Hash   :        RFLX.SPDM_Responder.Hash_ID;
                                          Slot   :        RFLX.SPDM.Slot;
                                          Result :    out RFLX.SPDM_Responder.Hash.Structure);

   overriding
   procedure Plat_Set_Session_Phase (Ctx    : in out Context;
                                     Phase  :        RFLX.SPDM_Responder.Session_Phase;
                                     Result :    out RFLX.SPDM_Responder.Session_Phase);

   overriding
   procedure Plat_Key_Update (Ctx       : in out Context;
                              Operation :        RFLX.SPDM.Key_Operation;
                              Tag       :        RFLX.SPDM.Key_Update_Tag;
                              Result    :    out Boolean);

   overriding
   procedure Null_Hash (Ctx    : in out Context;
                        Length :        RFLX.SPDM.Hash_Length;
                        Result :    out RFLX.SPDM_Responder.Hash.Structure);
#end if;
   overriding
   procedure Null_Signature (Ctx    : in out Context;
                             Length :        RFLX.SPDM.Signature_Length;
                             Result :    out RFLX.SPDM_Responder.Signature.Structure);
end SPDM_C_Responder;
