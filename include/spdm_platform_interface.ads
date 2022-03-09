with RFLX.SPDM;
with RFLX.SPDM_Responder.Digests_Data;

package SPDM_Platform_Interface is

   --  Retrieve CT exponent configuration from platform
   procedure Config_CT_Exponent (Result : out RFLX.SPDM.CT_Exponent);

   --  Retrieve MAC_CAP
   procedure Config_Cap_MAC (Result : out Boolean);

   --  Retrieve ENCRYPT_CAP
   procedure Config_Cap_Encrypt (Result : out Boolean);

   --  Retrieve MEAS_FRESH_CAP
   procedure Config_Cap_Meas_Fresh (Result : out Boolean);

   --  Retrieve MEAS_CAP
   procedure Config_Cap_Meas (Result : out RFLX.SPDM.Meas_Cap);

   --  Retrieve CHAL_CAP
   procedure Config_Cap_Chal (Result : out Boolean);

   --  Retrieve CERT_CAP
   procedure Config_Cap_Cert (Result : out Boolean);

   --  Retrieve CACHE_CAP
   procedure Config_Cap_Cache (Result : out Boolean);

   --  Retrieve HANDSHAKE_IN_THE_CLEAR_CAP
   procedure Config_Cap_Handshake_In_The_Clear (Result : out Boolean);

   --  Retrieve KEY_UPD_CAP
   procedure Config_Cap_Key_Upd (Result : out Boolean);

   --  Retrieve HBEAT_CAP
   procedure Config_Cap_Hbeat (Result : out Boolean);

   --  Retrieve ENCAP_CAP
   procedure Config_Cap_Encap (Result : out Boolean);

   --  Retrieve PSK_CAP
   procedure Config_Cap_PSK (Result : out RFLX.SPDM.PSK_Resp_Cap);

   --  Retrieve KEY_EX_CAP
   procedure Config_Cap_Key_Ex (Result : out Boolean);

   --  Retrieve MUT_AUTH_CAP
   procedure Config_Cap_Mut_Auth (Result : out Boolean);

   --  Retrieve PUB_KEY_ID_CAP
   procedure Config_Cap_Pub_Key_ID (Result : out Boolean);

   --  Select a measurement hash algorithm based on receive proposal
   procedure Select_Measurement_Hash_Algo
                (Result               : out RFLX.SPDM.Measurement_Hash_Algo;
                 TPM_ALG_SHA_256      :     Boolean;
                 TPM_ALG_SHA_384      :     Boolean;
                 TPM_ALG_SHA_512      :     Boolean;
                 TPM_ALG_SHA3_256     :     Boolean;
                 TPM_ALG_SHA3_384     :     Boolean;
                 TPM_ALG_SHA3_512     :     Boolean;
                 Raw_Bit_Streams_Only :     Boolean);

   --  Select an asymetric algorithm based on receive proposal
   procedure Select_Base_Asym_Algo
                (Result                      : out RFLX.SPDM.Base_Asym_Sel;
                 TPM_ALG_ECDSA_ECC_NIST_P384 :     Boolean;
                 TPM_ALG_RSAPSS_4096         :     Boolean;
                 TPM_ALG_RSASSA_4096         :     Boolean;
                 TPM_ALG_ECDSA_ECC_NIST_P256 :     Boolean;
                 TPM_ALG_RSAPSS_3072         :     Boolean;
                 TPM_ALG_RSASSA_3072         :     Boolean;
                 TPM_ALG_RSAPSS_2048         :     Boolean;
                 TPM_ALG_RSASSA_2048         :     Boolean;
                 TPM_ALG_ECDSA_ECC_NIST_P521 :     Boolean);

   --  Select a hash algorithm based on receive proposal
   procedure Select_Base_Hash_Algo
                (Result           : out RFLX.SPDM.Base_Hash_Sel;
                 TPM_ALG_SHA_256  :     Boolean;
                 TPM_ALG_SHA_384  :     Boolean;
                 TPM_ALG_SHA_512  :     Boolean;
                 TPM_ALG_SHA3_256 :     Boolean;
                 TPM_ALG_SHA3_384 :     Boolean;
                 TPM_ALG_SHA3_512 :     Boolean);

   procedure Select_DHE
      (Result    : out RFLX.SPDM_Responder.DHE_Algo;
       SecP521r1 :     Boolean;
       SecP384r1 :     Boolean;
       SecP256r1 :     Boolean;
       FFDHE4096 :     Boolean;
       FFDHE3072 :     Boolean;
       FFDHE2048 :     Boolean);

   procedure Select_AEAD
      (Result            : out RFLX.SPDM_Responder.AEAD_Algo;
       ChaCha20_Poly1305 :     Boolean;
       AES_256_GCM       :     Boolean;
       AES_128_GCM       :     Boolean);

   procedure Select_RBAA
      (Result                      : out RFLX.SPDM.Base_Asym_Sel;
       TPM_ALG_ECDSA_ECC_NIST_P384 :     Boolean;
       TPM_ALG_RSAPSS_4096         :     Boolean;
       TPM_ALG_RSASSA_4096         :     Boolean;
       TPM_ALG_ECDSA_ECC_NIST_P256 :     Boolean;
       TPM_ALG_RSAPSS_3072         :     Boolean;
       TPM_ALG_RSASSA_3072         :     Boolean;
       TPM_ALG_RSAPSS_2048         :     Boolean;
       TPM_ALG_RSASSA_2048         :     Boolean;
       TPM_ALG_ECDSA_ECC_NIST_P521 :     Boolean);

   procedure Get_Digests_Data (Plat_Get_Digests_Data : out RFLX.SPDM_Responder.Digests_Data.Structure;
                               Algorithm             :     RFLX.SPDM.Measurement_Hash_Algo);

end SPDM_Platform_Interface;
