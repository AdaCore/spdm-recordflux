with Interfaces.C;

package body SPDM_Platform_Interface is

   procedure Config_CT_Exponent (Result : out RFLX.SPDM.CT_Exponent)
   is
      function C_Interface return Interfaces.C.unsigned_char with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_config_ct_exponent";
   begin
      Result := RFLX.SPDM.CT_Exponent (C_Interface);
   end Config_CT_Exponent;

   procedure Config_Cap_MAC (Result : out Boolean)
   is
      function C_Interface return Interfaces.C.unsigned_char with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_config_cap_mac";
      use type Interfaces.C.unsigned_char;
   begin
      Result := C_Interface > 0;
   end Config_Cap_MAC;

   procedure Config_Cap_Encrypt (Result : out Boolean)
   is
      function C_Interface return Interfaces.C.unsigned_char with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_config_cap_encrypt";
      use type Interfaces.C.unsigned_char;
   begin
      Result := C_Interface > 0;
   end Config_Cap_Encrypt;

   procedure Config_Cap_Meas_Fresh (Result : out Boolean)
   is
      function C_Interface return Interfaces.C.unsigned_char with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_config_cap_meas_fresh";
      use type Interfaces.C.unsigned_char;
   begin
      Result := C_Interface > 0;
   end Config_Cap_Meas_Fresh;

   procedure Config_Cap_Meas (Result : out RFLX.SPDM.Meas_Cap)
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
   end Config_Cap_Meas;

   procedure Config_Cap_Chal (Result : out Boolean)
   is
      function C_Interface return Interfaces.C.unsigned_char with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_config_cap_chal";
      use type Interfaces.C.unsigned_char;
   begin
      Result := C_Interface > 0;
   end Config_Cap_Chal;

   procedure Config_Cap_Cert (Result : out Boolean)
   is
      function C_Interface return Interfaces.C.unsigned_char with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_config_cap_cert";
      use type Interfaces.C.unsigned_char;
   begin
      Result := C_Interface > 0;
   end Config_Cap_Cert;

   procedure Config_Cap_Cache (Result : out Boolean)
   is
      function C_Interface return Interfaces.C.unsigned_char with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_config_cap_cache";
      use type Interfaces.C.unsigned_char;
   begin
      Result := C_Interface > 0;
   end Config_Cap_Cache;

   procedure Config_Cap_Handshake_In_The_Clear (Result : out Boolean)
   is
      function C_Interface return Interfaces.C.unsigned_char with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_config_cap_handshake_in_the_clear";
      use type Interfaces.C.unsigned_char;
   begin
      Result := C_Interface > 0;
   end Config_Cap_Handshake_In_The_Clear;

   procedure Config_Cap_Key_Upd (Result : out Boolean)
   is
      function C_Interface return Interfaces.C.unsigned_char with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_config_cap_key_upd";
      use type Interfaces.C.unsigned_char;
   begin
      Result := C_Interface > 0;
   end Config_Cap_Key_Upd;

   procedure Config_Cap_Hbeat (Result : out Boolean)
   is
      function C_Interface return Interfaces.C.unsigned_char with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_config_cap_hbeat";
      use type Interfaces.C.unsigned_char;
   begin
      Result := C_Interface > 0;
   end Config_Cap_Hbeat;

   procedure Config_Cap_Encap (Result : out Boolean)
   is
      function C_Interface return Interfaces.C.unsigned_char with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_config_cap_encap";
      use type Interfaces.C.unsigned_char;
   begin
      Result := C_Interface > 0;
   end Config_Cap_Encap;

   procedure Config_Cap_PSK (Result : out RFLX.SPDM.PSK_Resp_Cap)
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
   end Config_Cap_PSK;

   procedure Config_Cap_Key_Ex (Result : out Boolean)
   is
      function C_Interface return Interfaces.C.unsigned_char with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_config_cap_key_ex";
      use type Interfaces.C.unsigned_char;
   begin
      Result := C_Interface > 0;
   end Config_Cap_Key_Ex;

   procedure Config_Cap_Mut_Auth (Result : out Boolean)
   is
      function C_Interface return Interfaces.C.unsigned_char with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_config_cap_mut_auth";
      use type Interfaces.C.unsigned_char;
   begin
      Result := C_Interface > 0;
   end Config_Cap_Mut_Auth;

   procedure Config_Cap_Pub_Key_ID (Result : out Boolean)
   is
      function C_Interface return Interfaces.C.unsigned_char with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_config_cap_pub_key_id";
      use type Interfaces.C.unsigned_char;
   begin
      Result := C_Interface > 0;
   end Config_Cap_Pub_Key_ID;

   procedure Select_Measurement_Hash_Algo
                (Result           : out RFLX.SPDM.Measurement_Hash_Algo;
                 TPM_ALG_SHA256   :     Boolean;
                 TPM_ALG_SHA_384  :     Boolean;
                 TPM_ALG_SHA_512  :     Boolean;
                 TPM_ALG_SHA3_256 :     Boolean;
                 TPM_ALG_SHA3_384 :     Boolean;
                 TPM_ALG_SHA3_512 :     Boolean)
   is
   begin
      --  FIXME
      Result := RFLX.SPDM.MH_TPM_ALG_SHA3_256;
   end Select_Measurement_Hash_Algo;

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
                 TPM_ALG_ECDSA_ECC_NIST_P521 :     Boolean)
   is
   begin
      --  FIXME
      Result := RFLX.SPDM.BA_Unsupported;
   end Select_Base_Asym_Algo;

   procedure Select_Base_Hash_Algo
                (Result           : out RFLX.SPDM.Base_Hash_Sel;
                 TPM_ALG_SHA256   :     Boolean;
                 TPM_ALG_SHA_384  :     Boolean;
                 TPM_ALG_SHA_512  :     Boolean;
                 TPM_ALG_SHA3_256 :     Boolean;
                 TPM_ALG_SHA3_384 :     Boolean;
                 TPM_ALG_SHA3_512 :     Boolean)
   is
   begin
      --  FIXME
      Result := RFLX.SPDM.BH_TPM_ALG_SHA3_512;
   end Select_Base_Hash_Algo;

   procedure Select_DHE
      (Result        : out RFLX.SPDM_Responder.DHE_Algo;
       Req_SecP521r1 :     Boolean;
       Req_SecP384r1 :     Boolean;
       Req_SecP256r1 :     Boolean;
       Req_FFDHE4096 :     Boolean;
       Req_FFDHE3072 :     Boolean;
       Req_FFDHE2048 :     Boolean)
   is
   begin
      -- FIXME
      Result := RFLX.SPDM_Responder.DA_SecP384r1;
   end Select_DHE;

   procedure Select_AEAD
      (Result                : out RFLX.SPDM_Responder.AEAD_Algo;
       Req_ChaCha20_Poly1305 :     Boolean;
       Req_AES_256_GCM       :     Boolean;
       Req_AES_128_GCM       :     Boolean)
   is
   begin
      -- FIXME
      Result := RFLX.SPDM_Responder.AA_AES_256_GCM;
   end Select_AEAD;

   procedure Select_RBAA
      (Result                          : out RFLX.SPDM_Responder.RBAA_Algo;
       Req_TPM_ALG_ECDSA_ECC_NIST_P384 :     Boolean;
       Req_TPM_ALG_RSAPSS_4096         :     Boolean;
       Req_TPM_ALG_RSASSA_4096         :     Boolean;
       Req_TPM_ALG_ECDSA_ECC_NIST_P256 :     Boolean;
       Req_TPM_ALG_RSAPSS_3072         :     Boolean;
       Req_TPM_ALG_RSASSA_3072         :     Boolean;
       Req_TPM_ALG_RSAPSS_2048         :     Boolean;
       Req_TPM_ALG_RSASSA_2048         :     Boolean;
       Req_TPM_ALG_ECDSA_ECC_NIST_P521 :     Boolean)
   is
   begin
      -- FIXME
      Result := RFLX.SPDM_Responder.RA_TPM_ALG_RSAPSS_3072;
   end Select_RBAA;

end SPDM_Platform_Interface;
