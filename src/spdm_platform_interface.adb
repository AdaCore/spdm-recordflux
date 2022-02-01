with System;
with RFLX.RFLX_Builtin_Types;
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

   function C_Bool (Value : Boolean) return Interfaces.C.unsigned_char is
      (if Value then 1 else 0);

   procedure Select_Measurement_Hash_Algo
                (Result               : out RFLX.SPDM.Measurement_Hash_Algo;
                 TPM_ALG_SHA_256      :     Boolean;
                 TPM_ALG_SHA_384      :     Boolean;
                 TPM_ALG_SHA_512      :     Boolean;
                 TPM_ALG_SHA3_256     :     Boolean;
                 TPM_ALG_SHA3_384     :     Boolean;
                 TPM_ALG_SHA3_512     :     Boolean;
                 Raw_Bit_Streams_Only :     Boolean)
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
   end Select_Base_Asym_Algo;

   procedure Select_Base_Hash_Algo
                (Result           : out RFLX.SPDM.Base_Hash_Sel;
                 TPM_ALG_SHA_256  :     Boolean;
                 TPM_ALG_SHA_384  :     Boolean;
                 TPM_ALG_SHA_512  :     Boolean;
                 TPM_ALG_SHA3_256 :     Boolean;
                 TPM_ALG_SHA3_384 :     Boolean;
                 TPM_ALG_SHA3_512 :     Boolean)
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
   end Select_Base_Hash_Algo;

   procedure Select_DHE
      (Result    : out RFLX.SPDM_Responder.DHE_Algo;
       SecP521r1 :     Boolean;
       SecP384r1 :     Boolean;
       SecP256r1 :     Boolean;
       FFDHE4096 :     Boolean;
       FFDHE3072 :     Boolean;
       FFDHE2048 :     Boolean)
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
         C_Interface (SecP521r1 => C_Bool (SecP521r1),
                      SecP384r1 => C_Bool (SecP384r1),
                      SecP256r1 => C_Bool (SecP256r1),
                      FFDHE4096 => C_Bool (FFDHE4096),
                      FFDHE3072 => C_Bool (FFDHE3072),
                      FFDHE2048 => C_Bool (FFDHE2048));
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
   end Select_DHE;

   procedure Select_AEAD
      (Result            : out RFLX.SPDM_Responder.AEAD_Algo;
       ChaCha20_Poly1305 :     Boolean;
       AES_256_GCM       :     Boolean;
       AES_128_GCM       :     Boolean)
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
         C_Interface (AA_ChaCha20_Poly1305 => C_Bool (ChaCha20_Poly1305),
                      AA_AES_256_GCM       => C_Bool (AES_256_GCM),
                      AA_AES_128_GCM       => C_Bool (AES_128_GCM));
      use type Interfaces.C.unsigned_char;
   begin
      --  Values could be used directly
      --  ISSUE: Componolit/RecordFlux#913
      Result := (case Value is
                 when      4 => RFLX.SPDM_Responder.AA_ChaCha20_Poly1305,
                 when      2 => RFLX.SPDM_Responder.AA_AES_256_GCM,
                 when      1 => RFLX.SPDM_Responder.AA_AES_128_GCM,
                 when others => raise Constraint_Error);
   end Select_AEAD;

   procedure Select_RBAA
      (Result                      : out RFLX.SPDM_Responder.RBAA_Algo;
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
         C_Interface (RA_TPM_ALG_ECDSA_ECC_NIST_P384 => C_Bool (TPM_ALG_ECDSA_ECC_NIST_P384),
                      RA_TPM_ALG_RSAPSS_4096         => C_Bool (TPM_ALG_RSAPSS_4096),
                      RA_TPM_ALG_RSASSA_4096         => C_Bool (TPM_ALG_RSASSA_4096),
                      RA_TPM_ALG_ECDSA_ECC_NIST_P256 => C_Bool (TPM_ALG_ECDSA_ECC_NIST_P256),
                      RA_TPM_ALG_RSAPSS_3072         => C_Bool (TPM_ALG_RSAPSS_3072),
                      RA_TPM_ALG_RSASSA_3072         => C_Bool (TPM_ALG_RSASSA_3072),
                      RA_TPM_ALG_RSAPSS_2048         => C_Bool (TPM_ALG_RSAPSS_2048),
                      RA_TPM_ALG_RSASSA_2048         => C_Bool (TPM_ALG_RSASSA_2048),
                      RA_TPM_ALG_ECDSA_ECC_NIST_P521 => C_Bool (TPM_ALG_ECDSA_ECC_NIST_P521));
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
   end Select_RBAA;


   procedure Get_Digests_Data (Plat_Get_Digests_Data : out RFLX.SPDM_Responder.Digests_Data.Structure;
                               Algorithm             :     RFLX.SPDM.Measurement_Hash_Algo)
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
      Length := Interfaces.C.long (Plat_Get_Digests_Data.Value'Length);
      C_Interface (Data   => Plat_Get_Digests_Data.Value'Address,
                   Length => Length'Address,
                   Slots  => Slot_Mask'Address);
      Plat_Get_Digests_Data.Length := RFLX.SPDM_Responder.Digests_Length (Length);
      Plat_Get_Digests_Data.Slot_0_Present := RFLX.SPDM.Slot_Present (Slot_Mask and 16#01#);
      Plat_Get_Digests_Data.Slot_1_Present := RFLX.SPDM.Slot_Present ((Slot_Mask and 16#02#) / 16#02#);
      Plat_Get_Digests_Data.Slot_2_Present := RFLX.SPDM.Slot_Present ((Slot_Mask and 16#04#) / 16#04#);
      Plat_Get_Digests_Data.Slot_3_Present := RFLX.SPDM.Slot_Present ((Slot_Mask and 16#08#) / 16#08#);
      Plat_Get_Digests_Data.Slot_4_Present := RFLX.SPDM.Slot_Present ((Slot_Mask and 16#10#) / 16#10#);
      Plat_Get_Digests_Data.Slot_5_Present := RFLX.SPDM.Slot_Present ((Slot_Mask and 16#20#) / 16#20#);
      Plat_Get_Digests_Data.Slot_6_Present := RFLX.SPDM.Slot_Present ((Slot_Mask and 16#40#) / 16#40#);
      Plat_Get_Digests_Data.Slot_7_Present := RFLX.SPDM.Slot_Present ((Slot_Mask and 16#80#) / 16#80#);
   end Get_Digests_Data;

end SPDM_Platform_Interface;
