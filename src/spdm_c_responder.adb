with Interfaces.C;
with System;
with RFLX.RFLX_Builtin_Types;
with RFLX.SPDM_Responder.Digests_Data;

package body SPDM_C_Responder with
   SPARK_Mode
is

   overriding
   procedure Plat_Cfg_CT_Exponent
      (Ctx    : in out Context;
       Result :    out RFLX.SPDM.CT_Exponent)
   is
      function C_Interface return Interfaces.C.unsigned_char with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_config_ct_exponent";
   begin
      Result := RFLX.SPDM.CT_Exponent (C_Interface);
   end Plat_Cfg_CT_Exponent;

   overriding
   procedure Plat_Cfg_Cap_MAC
      (Ctx    : in out Context;
       Result :    out Boolean)
   is
      function C_Interface return Interfaces.C.unsigned_char with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_config_cap_mac";
      use type Interfaces.C.unsigned_char;
   begin
      Result := C_Interface > 0;
   end Plat_Cfg_Cap_MAC;

   overriding
   procedure Plat_Cfg_Cap_Encrypt
      (Ctx    : in out Context;
       Result :    out Boolean)
   is
      function C_Interface return Interfaces.C.unsigned_char with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_config_cap_encrypt";
      use type Interfaces.C.unsigned_char;
   begin
      Result := C_Interface > 0;
   end Plat_Cfg_Cap_Encrypt;

   overriding
   procedure Plat_Cfg_Cap_Meas_Fresh
      (Ctx    : in out Context;
       Result :    out Boolean)
   is
      function C_Interface return Interfaces.C.unsigned_char with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_config_cap_meas_fresh";
      use type Interfaces.C.unsigned_char;
   begin
      Result := C_Interface > 0;
   end Plat_Cfg_Cap_Meas_Fresh;

   overriding
   procedure Plat_Cfg_Cap_Meas
      (Ctx    : in out Context;
       Result :    out RFLX.SPDM.Meas_Cap)
   is
      function C_Interface return Interfaces.C.unsigned_char with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_config_cap_meas";
      Value : constant Interfaces.C.unsigned_char := C_Interface;
      use type Interfaces.C.unsigned_char;
   begin
      pragma Assert (Value < 3);
      Result := (case Value is
                 when 0      => RFLX.SPDM.Meas_Unsupported,
                 when 1      => RFLX.SPDM.Meas_Plain,
                 when 2      => RFLX.SPDM.MEas_Signed,
                 when others => raise Program_Error);
   end Plat_Cfg_Cap_Meas;

   overriding
   procedure Plat_Cfg_Cap_Chal
      (Ctx    : in out Context;
       Result :    out Boolean)
   is
      function C_Interface return Interfaces.C.unsigned_char with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_config_cap_chal";
      use type Interfaces.C.unsigned_char;
   begin
      Result := C_Interface > 0;
   end Plat_Cfg_Cap_Chal;

   overriding
   procedure Plat_Cfg_Cap_Cert
      (Ctx    : in out Context;
       Result :    out Boolean)
   is
      function C_Interface return Interfaces.C.unsigned_char with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_config_cap_cert";
      use type Interfaces.C.unsigned_char;
   begin
      Result := C_Interface > 0;
   end Plat_Cfg_Cap_Cert;

   overriding
   procedure Plat_Cfg_Cap_Cache
      (Ctx    : in out Context;
       Result :    out Boolean)
   is
      function C_Interface return Interfaces.C.unsigned_char with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_config_cap_cache";
      use type Interfaces.C.unsigned_char;
   begin
      Result := C_Interface > 0;
   end Plat_Cfg_Cap_Cache;

   overriding
   procedure Plat_Cfg_Cap_Handshake_In_The_Clear
      (Ctx    : in out Context;
       Result :    out Boolean)
   is
      function C_Interface return Interfaces.C.unsigned_char with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_config_cap_handshake_in_the_clear";
      use type Interfaces.C.unsigned_char;
   begin
      Result := C_Interface > 0;
   end Plat_Cfg_Cap_Handshake_In_The_Clear;

   overriding
   procedure Plat_Cfg_Cap_Key_Upd
      (Ctx    : in out Context;
       Result :    out Boolean)
   is
      function C_Interface return Interfaces.C.unsigned_char with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_config_cap_key_upd";
      use type Interfaces.C.unsigned_char;
   begin
      Result := C_Interface > 0;
   end Plat_Cfg_Cap_Key_Upd;

   overriding
   procedure Plat_Cfg_Cap_Hbeat
      (Ctx    : in out Context;
       Result :    out Boolean)
   is
      function C_Interface return Interfaces.C.unsigned_char with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_config_cap_hbeat";
      use type Interfaces.C.unsigned_char;
   begin
      Result := C_Interface > 0;
   end Plat_Cfg_Cap_Hbeat;

   overriding
   procedure Plat_Cfg_Cap_Encap
      (Ctx    : in out Context;
       Result :    out Boolean)
   is
      function C_Interface return Interfaces.C.unsigned_char with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_config_cap_encap";
      use type Interfaces.C.unsigned_char;
   begin
      Result := C_Interface > 0;
   end Plat_Cfg_Cap_Encap;

   overriding
   procedure Plat_Cfg_Cap_PSK
      (Ctx    : in out Context;
       Result :    out RFLX.SPDM.PSK_Resp_Cap)
   is
      function C_Interface return Interfaces.C.unsigned_char with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_config_cap_psk";
      Value : constant Interfaces.C.unsigned_char := C_Interface;
      use type Interfaces.C.unsigned_char;
   begin
      pragma Assert (Value < 3);
      Result := (case Value is
                 when 0 => RFLX.SPDM.PSK_Resp_Unsupported,
                 when 1 => RFLX.SPDM.PSK_Resp_Without_Context,
                 when 2 => RFLX.SPDM.PSK_Resp_With_Context,
                 when others => raise Program_Error);
   end Plat_Cfg_Cap_PSK;

   overriding
   procedure Plat_Cfg_Cap_Key_Ex
      (Ctx    : in out Context;
       Result :    out Boolean)
   is
      function C_Interface return Interfaces.C.unsigned_char with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_config_cap_key_ex";
      use type Interfaces.C.unsigned_char;
   begin
      Result := C_Interface > 0;
   end Plat_Cfg_Cap_Key_Ex;

   overriding
   procedure Plat_Cfg_Cap_Mut_Auth
      (Ctx    : in out Context;
       Result :    out Boolean)
   is
      function C_Interface return Interfaces.C.unsigned_char with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_config_cap_mut_auth";
      use type Interfaces.C.unsigned_char;
   begin
      Result := C_Interface > 0;
   end Plat_Cfg_Cap_Mut_Auth;

   overriding
   procedure Plat_Cfg_Cap_Pub_Key_ID
      (Ctx    : in out Context;
       Result :    out Boolean)
   is
      function C_Interface return Interfaces.C.unsigned_char with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_config_cap_pub_key_id";
      use type Interfaces.C.unsigned_char;
   begin
      Result := C_Interface > 0;
   end Plat_Cfg_Cap_Pub_Key_ID;

   function C_Bool (Value : Boolean) return Interfaces.C.unsigned_char is
      (if Value then 1 else 0);

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
       Result               :    out RFLX.SPDM.Measurement_Hash_Algo)
   is
      function C_Interface
         (TPM_ALG_SHA_256      : Interfaces.C.unsigned_char;
          TPM_ALG_SHA_384      : Interfaces.C.unsigned_char;
          TPM_ALG_SHA_512      : Interfaces.C.unsigned_char;
          TPM_ALG_SHA3_256     : Interfaces.C.unsigned_char;
          TPM_ALG_SHA3_384     : Interfaces.C.unsigned_char;
          TPM_ALG_SHA3_512     : Interfaces.C.unsigned_char;
          Raw_Bit_Streams_Only : Interfaces.C.unsigned_char) return Interfaces.C.unsigned_char
      with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_select_measurement_hash_algo";
      Value : constant Interfaces.C.unsigned_char :=
         C_Interface (TPM_ALG_SHA_256      => C_Bool (TPM_ALG_SHA_256),
                      TPM_ALG_SHA_384      => C_Bool (TPM_ALG_SHA_384),
                      TPM_ALG_SHA_512      => C_Bool (TPM_ALG_SHA_512),
                      TPM_ALG_SHA3_256     => C_Bool (TPM_ALG_SHA3_256),
                      TPM_ALG_SHA3_384     => C_Bool (TPM_ALG_SHA3_384),
                      TPM_ALG_SHA3_512     => C_Bool (TPM_ALG_SHA3_512),
                      Raw_Bit_Streams_Only => C_Bool (Raw_Bit_Streams_Only));
      use type Interfaces.C.unsigned_char;
   begin
      --  Values could be used directly
      --  ISSUE: Componolit/RecordFlux#913
      Result := (case Value is
                 when     64 => RFLX.SPDM.MH_TPM_ALG_SHA3_512,
                 when     32 => RFLX.SPDM.MH_TPM_ALG_SHA3_384,
                 when     16 => RFLX.SPDM.MH_TPM_ALG_SHA3_256,
                 when      8 => RFLX.SPDM.MH_TPM_ALG_SHA_512,
                 when      4 => RFLX.SPDM.MH_TPM_ALG_SHA_384,
                 when      2 => RFLX.SPDM.MH_TPM_ALG_SHA_256,
                 when      1 => RFLX.SPDM.MH_Raw_Bit_Streams_Only,
                 when      0 => RFLX.SPDM.MH_Unsupported,
                 when others => raise Constraint_Error);
   end Plat_Cfg_Sel_Measurement_Hash_Algo;

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
       Result                      :    out RFLX.SPDM.Base_Asym_Sel)
   is
      function C_Interface
         (TPM_ALG_ECDSA_ECC_NIST_P384 : Interfaces.C.unsigned_char;
          TPM_ALG_RSAPSS_4096         : Interfaces.C.unsigned_char;
          TPM_ALG_RSASSA_4096         : Interfaces.C.unsigned_char;
          TPM_ALG_ECDSA_ECC_NIST_P256 : Interfaces.C.unsigned_char;
          TPM_ALG_RSAPSS_3072         : Interfaces.C.unsigned_char;
          TPM_ALG_RSASSA_3072         : Interfaces.C.unsigned_char;
          TPM_ALG_RSAPSS_2048         : Interfaces.C.unsigned_char;
          TPM_ALG_RSASSA_2048         : Interfaces.C.unsigned_char;
          TPM_ALG_ECDSA_ECC_NIST_P521 : Interfaces.C.unsigned_char) return Interfaces.C.long
      with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_select_base_asym_algo";
      Value : constant Interfaces.C.long :=
         C_Interface (TPM_ALG_ECDSA_ECC_NIST_P384 => C_Bool (TPM_ALG_ECDSA_ECC_NIST_P384),
                      TPM_ALG_RSAPSS_4096         => C_Bool (TPM_ALG_RSAPSS_4096),
                      TPM_ALG_RSASSA_4096         => C_Bool (TPM_ALG_RSASSA_4096),
                      TPM_ALG_ECDSA_ECC_NIST_P256 => C_Bool (TPM_ALG_ECDSA_ECC_NIST_P256),
                      TPM_ALG_RSAPSS_3072         => C_Bool (TPM_ALG_RSAPSS_3072),
                      TPM_ALG_RSASSA_3072         => C_Bool (TPM_ALG_RSASSA_3072),
                      TPM_ALG_RSAPSS_2048         => C_Bool (TPM_ALG_RSAPSS_2048),
                      TPM_ALG_RSASSA_2048         => C_Bool (TPM_ALG_RSASSA_2048),
                      TPM_ALG_ECDSA_ECC_NIST_P521 => C_Bool (TPM_ALG_ECDSA_ECC_NIST_P521));
      use type Interfaces.C.long;
   begin
      --  Values could be used directly
      --  ISSUE: Componolit/RecordFlux#913
      Result := (case Value is
                 when    256 => RFLX.SPDM.BA_TPM_ALG_ECDSA_ECC_NIST_P521,
                 when    128 => RFLX.SPDM.BA_TPM_ALG_ECDSA_ECC_NIST_P384,
                 when     64 => RFLX.SPDM.BA_TPM_ALG_RSAPSS_4096,
                 when     32 => RFLX.SPDM.BA_TPM_ALG_RSASSA_4096,
                 when     16 => RFLX.SPDM.BA_TPM_ALG_ECDSA_ECC_NIST_P256,
                 when      8 => RFLX.SPDM.BA_TPM_ALG_RSAPSS_3072,
                 when      4 => RFLX.SPDM.BA_TPM_ALG_RSASSA_3072,
                 when      2 => RFLX.SPDM.BA_TPM_ALG_RSAPSS_2048,
                 when      1 => RFLX.SPDM.BA_TPM_ALG_RSASSA_2048,
                 when      0 => RFLX.SPDM.BA_Unsupported,
                 when others => raise Constraint_Error);
   end Plat_Cfg_Sel_Base_Asym_Algo;

   overriding
   procedure Plat_Cfg_Sel_Base_Hash_Algo
      (Ctx              : in out Context;
       TPM_ALG_SHA_256  :        Boolean;
       TPM_ALG_SHA_384  :        Boolean;
       TPM_ALG_SHA_512  :        Boolean;
       TPM_ALG_SHA3_256 :        Boolean;
       TPM_ALG_SHA3_384 :        Boolean;
       TPM_ALG_SHA3_512 :        Boolean;
       Result           :    out RFLX.SPDM.Base_Hash_Sel)
   is
      function C_Interface
         (TPM_ALG_SHA_256  : Interfaces.C.unsigned_char;
          TPM_ALG_SHA_384  : Interfaces.C.unsigned_char;
          TPM_ALG_SHA_512  : Interfaces.C.unsigned_char;
          TPM_ALG_SHA3_256 : Interfaces.C.unsigned_char;
          TPM_ALG_SHA3_384 : Interfaces.C.unsigned_char;
          TPM_ALG_SHA3_512 : Interfaces.C.unsigned_char) return Interfaces.C.unsigned_char
      with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_select_base_hash_algo";
      Value : constant Interfaces.C.unsigned_char :=
         C_Interface (TPM_ALG_SHA_256  => C_Bool (TPM_ALG_SHA_256),
                      TPM_ALG_SHA_384  => C_Bool (TPM_ALG_SHA_384),
                      TPM_ALG_SHA_512  => C_Bool (TPM_ALG_SHA_512),
                      TPM_ALG_SHA3_256 => C_Bool (TPM_ALG_SHA3_256),
                      TPM_ALG_SHA3_384 => C_Bool (TPM_ALG_SHA3_384),
                      TPM_ALG_SHA3_512 => C_Bool (TPM_ALG_SHA3_512));
      use type Interfaces.C.unsigned_char;
   begin
      --  Values could be used directly
      --  ISSUE: Componolit/RecordFlux#913
      Result := (case Value is
                 when     32 => RFLX.SPDM.BH_TPM_ALG_SHA_256,
                 when     16 => RFLX.SPDM.BH_TPM_ALG_SHA_384,
                 when      8 => RFLX.SPDM.BH_TPM_ALG_SHA_512,
                 when      4 => RFLX.SPDM.BH_TPM_ALG_SHA3_256,
                 when      2 => RFLX.SPDM.BH_TPM_ALG_SHA3_384,
                 when      1 => RFLX.SPDM.BH_TPM_ALG_SHA3_512,
                 when others => raise Constraint_Error);
   end Plat_Cfg_Sel_Base_Hash_Algo;

   overriding
   procedure Plat_Cfg_Sel_DHE
      (Ctx           : in out Context;
       Req_SecP521r1 :        Boolean;
       Req_SecP384r1 :        Boolean;
       Req_SecP256r1 :        Boolean;
       Req_FFDHE4096 :        Boolean;
       Req_FFDHE3072 :        Boolean;
       Req_FFDHE2048 :        Boolean;
       Result        :    out RFLX.SPDM_Responder.DHE_Algo)
   is
      function C_Interface
         (SecP521r1 : Interfaces.C.unsigned_char;
          SecP384r1 : Interfaces.C.unsigned_char;
          SecP256r1 : Interfaces.C.unsigned_char;
          FFDHE4096 : Interfaces.C.unsigned_char;
          FFDHE3072 : Interfaces.C.unsigned_char;
          FFDHE2048 : Interfaces.C.unsigned_char) return Interfaces.C.unsigned_char
      with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_select_dhe";
      Value : constant Interfaces.C.unsigned_char :=
         C_Interface (SecP521r1 => C_Bool (Req_SecP521r1),
                      SecP384r1 => C_Bool (Req_SecP384r1),
                      SecP256r1 => C_Bool (Req_SecP256r1),
                      FFDHE4096 => C_Bool (Req_FFDHE4096),
                      FFDHE3072 => C_Bool (Req_FFDHE3072),
                      FFDHE2048 => C_Bool (Req_FFDHE2048));
      use type Interfaces.C.unsigned_char;
   begin
      --  Values could be used directly
      --  ISSUE: Componolit/RecordFlux#913
      Result := (case Value is
                 when     32 => RFLX.SPDM_Responder.DA_SecP521r1,
                 when     16 => RFLX.SPDM_Responder.DA_SecP384r1,
                 when      8 => RFLX.SPDM_Responder.DA_SecP256r1,
                 when      4 => RFLX.SPDM_Responder.DA_FFDHE4096,
                 when      2 => RFLX.SPDM_Responder.DA_FFDHE3072,
                 when      1 => RFLX.SPDM_Responder.DA_FFDHE2048,
                 when others => raise Constraint_Error);
   end Plat_Cfg_Sel_DHE;

   overriding
   procedure Plat_Cfg_Sel_AEAD
      (Ctx                   : in out Context;
       Req_ChaCha20_Poly1305 :        Boolean;
       Req_AES_256_GCM       :        Boolean;
       Req_AES_128_GCM       :        Boolean;
       Result                :    out RFLX.SPDM_Responder.AEAD_Algo)
   is
      function C_Interface
         (AA_ChaCha20_Poly1305 : Interfaces.C.unsigned_char;
          AA_AES_256_GCM       : Interfaces.C.unsigned_char;
          AA_AES_128_GCM       : Interfaces.C.unsigned_char) return Interfaces.C.unsigned_char
      with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_select_aead";
      Value : constant Interfaces.C.unsigned_char :=
         C_Interface (AA_ChaCha20_Poly1305 => C_Bool (Req_ChaCha20_Poly1305),
                      AA_AES_256_GCM       => C_Bool (Req_AES_256_GCM),
                      AA_AES_128_GCM       => C_Bool (Req_AES_128_GCM));
      use type Interfaces.C.unsigned_char;
   begin
      --  Values could be used directly
      --  ISSUE: Componolit/RecordFlux#913
      Result := (case Value is
                 when      4 => RFLX.SPDM_Responder.AA_ChaCha20_Poly1305,
                 when      2 => RFLX.SPDM_Responder.AA_AES_256_GCM,
                 when      1 => RFLX.SPDM_Responder.AA_AES_128_GCM,
                 when others => raise Constraint_Error);
   end Plat_Cfg_Sel_AEAD;

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
       Result                          :    out RFLX.SPDM_Responder.RBAA_Algo)
   is
      function C_Interface
         (RA_TPM_ALG_ECDSA_ECC_NIST_P384 : Interfaces.C.unsigned_char;
          RA_TPM_ALG_RSAPSS_4096         : Interfaces.C.unsigned_char;
          RA_TPM_ALG_RSASSA_4096         : Interfaces.C.unsigned_char;
          RA_TPM_ALG_ECDSA_ECC_NIST_P256 : Interfaces.C.unsigned_char;
          RA_TPM_ALG_RSAPSS_3072         : Interfaces.C.unsigned_char;
          RA_TPM_ALG_RSASSA_3072         : Interfaces.C.unsigned_char;
          RA_TPM_ALG_RSAPSS_2048         : Interfaces.C.unsigned_char;
          RA_TPM_ALG_RSASSA_2048         : Interfaces.C.unsigned_char;
          RA_TPM_ALG_ECDSA_ECC_NIST_P521 : Interfaces.C.unsigned_char) return Interfaces.C.long
      with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_select_rbba";
      Value : constant Interfaces.C.long :=
         C_Interface (RA_TPM_ALG_ECDSA_ECC_NIST_P384 => C_Bool (Req_TPM_ALG_ECDSA_ECC_NIST_P384),
                      RA_TPM_ALG_RSAPSS_4096         => C_Bool (Req_TPM_ALG_RSAPSS_4096),
                      RA_TPM_ALG_RSASSA_4096         => C_Bool (Req_TPM_ALG_RSASSA_4096),
                      RA_TPM_ALG_ECDSA_ECC_NIST_P256 => C_Bool (Req_TPM_ALG_ECDSA_ECC_NIST_P256),
                      RA_TPM_ALG_RSAPSS_3072         => C_Bool (Req_TPM_ALG_RSAPSS_3072),
                      RA_TPM_ALG_RSASSA_3072         => C_Bool (Req_TPM_ALG_RSASSA_3072),
                      RA_TPM_ALG_RSAPSS_2048         => C_Bool (Req_TPM_ALG_RSAPSS_2048),
                      RA_TPM_ALG_RSASSA_2048         => C_Bool (Req_TPM_ALG_RSASSA_2048),
                      RA_TPM_ALG_ECDSA_ECC_NIST_P521 => C_Bool (Req_TPM_ALG_ECDSA_ECC_NIST_P521));
      use type Interfaces.C.unsigned_char;
   begin
      --  Values could be used directly
      --  ISSUE: Componolit/RecordFlux#913
      Result := (case Value is
                 when    256 => RFLX.SPDM_Responder.RA_TPM_ALG_ECDSA_ECC_NIST_P384,
                 when    128 => RFLX.SPDM_Responder.RA_TPM_ALG_RSAPSS_4096,
                 when     64 => RFLX.SPDM_Responder.RA_TPM_ALG_RSASSA_4096,
                 when     32 => RFLX.SPDM_Responder.RA_TPM_ALG_ECDSA_ECC_NIST_P256,
                 when     16 => RFLX.SPDM_Responder.RA_TPM_ALG_RSAPSS_3072,
                 when      8 => RFLX.SPDM_Responder.RA_TPM_ALG_RSASSA_3072,
                 when      4 => RFLX.SPDM_Responder.RA_TPM_ALG_RSAPSS_2048,
                 when      2 => RFLX.SPDM_Responder.RA_TPM_ALG_RSASSA_2048,
                 when      1 => RFLX.SPDM_Responder.RA_TPM_ALG_ECDSA_ECC_NIST_P521,
                 when others => raise Constraint_Error);
   end Plat_Cfg_Sel_RBAA;

   overriding
   procedure Plat_Get_Digests_Data
      (Ctx    : in out Context;
       Algo   :        RFLX.SPDM.Measurement_Hash_Algo;
       Result :    out RFLX.SPDM_Responder.Digests_Data.Structure)
   is
      Slot_Mask : Interfaces.C.unsigned_char;
      Length    : Interfaces.C.long;

      procedure C_Interface (Data   : System.Address;
                             Length : System.Address;
                             Slots  : System.Address) with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_get_digests_data";
      use type Interfaces.C.unsigned_char;
      use type RFLX.SPDM_Responder.Digests_Length;
   begin
      Length := Interfaces.C.long (Result.Value'Length);
      C_Interface (Data   => Result.Value'Address,
                   Length => Length'Address,
                   Slots  => Slot_Mask'Address);
      Result.Length := RFLX.SPDM_Responder.Digests_Length (Length);
      Result.Slot_0_Present := RFLX.SPDM.Slot_Present (Slot_Mask and 16#01#);
      Result.Slot_1_Present := RFLX.SPDM.Slot_Present ((Slot_Mask and 16#02#) / 16#02#);
      Result.Slot_2_Present := RFLX.SPDM.Slot_Present ((Slot_Mask and 16#04#) / 16#04#);
      Result.Slot_3_Present := RFLX.SPDM.Slot_Present ((Slot_Mask and 16#08#) / 16#08#);
      Result.Slot_4_Present := RFLX.SPDM.Slot_Present ((Slot_Mask and 16#10#) / 16#10#);
      Result.Slot_5_Present := RFLX.SPDM.Slot_Present ((Slot_Mask and 16#20#) / 16#20#);
      Result.Slot_6_Present := RFLX.SPDM.Slot_Present ((Slot_Mask and 16#40#) / 16#40#);
      Result.Slot_7_Present := RFLX.SPDM.Slot_Present ((Slot_Mask and 16#80#) / 16#80#);
   end Plat_Get_Digests_Data;

end SPDM_C_Responder;
