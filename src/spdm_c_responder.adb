with Interfaces.C;
with RFLX.SPDM_Responder.Digests_Data;
with RFLX.SPDM;

package body SPDM_C_Responder with
   SPARK_Mode
is

   procedure Plat_Initialize (Ctx : in out Context)
   is
      procedure C_Interface (Instance : out System.Address) with
         Import,
         Convention => C,
         External_Name => "spdm_platform_initialize";
   begin
      C_Interface (Ctx.Instance);
   end Plat_Initialize;

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
      if not RFLX.SPDM.Valid (RFLX.SPDM.CT_Exponent_Base (C_Interface)) then
         raise Constraint_Error;
      end if;
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
      Value : constant RFLX.SPDM.Meas_Cap_Base := RFLX.SPDM.Meas_Cap_Base (C_Interface);
   begin
      if not RFLX.SPDM.Valid (Value) then
         raise Constraint_Error;
      end if;
      Result := RFLX.SPDM.To_Actual (Value);
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
      Value : constant RFLX.SPDM.PSK_Resp_Cap_Base := RFLX.SPDM.PSK_Resp_Cap_Base (C_Interface);
   begin
      if not RFLX.SPDM.Valid (Value) then
         raise Constraint_Error;
      end if;
      Result := RFLX.SPDM.To_Actual (Value);
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
      Value : constant RFLX.SPDM.Measurement_Hash_Algo_Base :=
         RFLX.SPDM.Measurement_Hash_Algo_Base
            (C_Interface
               (TPM_ALG_SHA_256      => C_Bool (TPM_ALG_SHA_256),
                TPM_ALG_SHA_384      => C_Bool (TPM_ALG_SHA_384),
                TPM_ALG_SHA_512      => C_Bool (TPM_ALG_SHA_512),
                TPM_ALG_SHA3_256     => C_Bool (TPM_ALG_SHA3_256),
                TPM_ALG_SHA3_384     => C_Bool (TPM_ALG_SHA3_384),
                TPM_ALG_SHA3_512     => C_Bool (TPM_ALG_SHA3_512),
                Raw_Bit_Streams_Only => C_Bool (Raw_Bit_Streams_Only)));
   begin
      if not RFLX.SPDM.Valid (Value) then
         raise Constraint_Error;
      end if;
      Result := RFLX.SPDM.To_Actual (Value);
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
       Result                      :    out RFLX.SPDM.Base_Asym_Algo)
   is
      function C_Interface
         (Instance                    : System.Address;
          TPM_ALG_ECDSA_ECC_NIST_P384 : Interfaces.C.unsigned_char;
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
      Value : constant RFLX.SPDM.Base_Asym_Algo_Base :=
         RFLX.SPDM.Base_Asym_Algo_Base
            (C_Interface
               (Instance                    => Ctx.Instance,
                TPM_ALG_ECDSA_ECC_NIST_P384 => C_Bool (TPM_ALG_ECDSA_ECC_NIST_P384),
                TPM_ALG_RSAPSS_4096         => C_Bool (TPM_ALG_RSAPSS_4096),
                TPM_ALG_RSASSA_4096         => C_Bool (TPM_ALG_RSASSA_4096),
                TPM_ALG_ECDSA_ECC_NIST_P256 => C_Bool (TPM_ALG_ECDSA_ECC_NIST_P256),
                TPM_ALG_RSAPSS_3072         => C_Bool (TPM_ALG_RSAPSS_3072),
                TPM_ALG_RSASSA_3072         => C_Bool (TPM_ALG_RSASSA_3072),
                TPM_ALG_RSAPSS_2048         => C_Bool (TPM_ALG_RSAPSS_2048),
                TPM_ALG_RSASSA_2048         => C_Bool (TPM_ALG_RSASSA_2048),
                TPM_ALG_ECDSA_ECC_NIST_P521 => C_Bool (TPM_ALG_ECDSA_ECC_NIST_P521)));
   begin
      if not RFLX.SPDM.Valid (Value) then
         raise Constraint_Error;
      end if;
      Result := RFLX.SPDM.To_Actual (Value);
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
       Result           :    out RFLX.SPDM.Base_Hash_Algo)
   is
      function C_Interface
         (Instance         : System.Address;
          TPM_ALG_SHA_256  : Interfaces.C.unsigned_char;
          TPM_ALG_SHA_384  : Interfaces.C.unsigned_char;
          TPM_ALG_SHA_512  : Interfaces.C.unsigned_char;
          TPM_ALG_SHA3_256 : Interfaces.C.unsigned_char;
          TPM_ALG_SHA3_384 : Interfaces.C.unsigned_char;
          TPM_ALG_SHA3_512 : Interfaces.C.unsigned_char) return Interfaces.C.unsigned_char
      with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_select_base_hash_algo";
      Value : constant RFLX.SPDM.Base_Hash_Algo_Base :=
         RFLX.SPDM.Base_Hash_Algo_Base
            (C_Interface
               (Instance         => Ctx.Instance,
                TPM_ALG_SHA_256  => C_Bool (TPM_ALG_SHA_256),
                TPM_ALG_SHA_384  => C_Bool (TPM_ALG_SHA_384),
                TPM_ALG_SHA_512  => C_Bool (TPM_ALG_SHA_512),
                TPM_ALG_SHA3_256 => C_Bool (TPM_ALG_SHA3_256),
                TPM_ALG_SHA3_384 => C_Bool (TPM_ALG_SHA3_384),
                TPM_ALG_SHA3_512 => C_Bool (TPM_ALG_SHA3_512)));
   begin
      if not RFLX.SPDM.Valid (Value) then
         raise Constraint_Error;
      end if;
      Result := RFLX.SPDM.To_Actual (Value);
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
      Value : constant RFLX.SPDM_Responder.DHE_Algo_Base :=
         RFLX.SPDM_Responder.DHE_Algo_Base
            (C_Interface
               (SecP521r1 => C_Bool (Req_SecP521r1),
                SecP384r1 => C_Bool (Req_SecP384r1),
                SecP256r1 => C_Bool (Req_SecP256r1),
                FFDHE4096 => C_Bool (Req_FFDHE4096),
                FFDHE3072 => C_Bool (Req_FFDHE3072),
                FFDHE2048 => C_Bool (Req_FFDHE2048)));
   begin
      if not RFLX.SPDM_Responder.Valid (Value) then
         raise Constraint_Error;
      end if;
      Result := RFLX.SPDM_Responder.To_Actual (Value);
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
      Value : constant RFLX.SPDM_Responder.AEAD_Algo_Base :=
         RFLX.SPDM_Responder.AEAD_Algo_Base
            (C_Interface
               (AA_ChaCha20_Poly1305 => C_Bool (Req_ChaCha20_Poly1305),
                AA_AES_256_GCM       => C_Bool (Req_AES_256_GCM),
                AA_AES_128_GCM       => C_Bool (Req_AES_128_GCM)));
   begin
      if not RFLX.SPDM_Responder.Valid (Value) then
         raise Constraint_Error;
      end if;
      Result := RFLX.SPDM_Responder.To_Actual (Value);
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
       Result                          :    out RFLX.SPDM.Base_Asym_Algo)
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
      Value : constant RFLX.SPDM.Base_Asym_Algo_Base :=
         (RFLX.SPDM.Base_Asym_Algo_Base
            (C_Interface
               (RA_TPM_ALG_ECDSA_ECC_NIST_P384 => C_Bool (Req_TPM_ALG_ECDSA_ECC_NIST_P384),
                RA_TPM_ALG_RSAPSS_4096         => C_Bool (Req_TPM_ALG_RSAPSS_4096),
                RA_TPM_ALG_RSASSA_4096         => C_Bool (Req_TPM_ALG_RSASSA_4096),
                RA_TPM_ALG_ECDSA_ECC_NIST_P256 => C_Bool (Req_TPM_ALG_ECDSA_ECC_NIST_P256),
                RA_TPM_ALG_RSAPSS_3072         => C_Bool (Req_TPM_ALG_RSAPSS_3072),
                RA_TPM_ALG_RSASSA_3072         => C_Bool (Req_TPM_ALG_RSASSA_3072),
                RA_TPM_ALG_RSAPSS_2048         => C_Bool (Req_TPM_ALG_RSAPSS_2048),
                RA_TPM_ALG_RSASSA_2048         => C_Bool (Req_TPM_ALG_RSASSA_2048),
                RA_TPM_ALG_ECDSA_ECC_NIST_P521 => C_Bool (Req_TPM_ALG_ECDSA_ECC_NIST_P521))));
   begin
      if not RFLX.SPDM.Valid (Value) then
         raise Constraint_Error;
      end if;
      Result := RFLX.SPDM.To_Actual (Value);
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

   overriding
   procedure Plat_Valid_Certificate_Request
      (Ctx    : in out Context;
       Slot   :        RFLX.SPDM.Slot;
       Offset :        RFLX.SPDM.Offset;
       Length :        RFLX.SPDM.Length_16;
       Result :    out Boolean)
   is
      function C_Interface (Slot   : Interfaces.C.unsigned_char;
                            Offset : Interfaces.C.unsigned_short;
                            Length : Interfaces.C.unsigned_short) return Interfaces.C.unsigned_char
      with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_validate_certificate_request";
      use type Interfaces.C.unsigned_char;
      use type Interfaces.C.unsigned_short;
   begin
      Result := 0 /= C_Interface (Slot   => Interfaces.C.unsigned_char (RFLX.SPDM.To_Base (Slot)),
                                  Offset => Interfaces.C.unsigned_short (Offset),
                                  Length => Interfaces.C.unsigned_short (Length));
   end Plat_Valid_Certificate_Request;

   overriding
   procedure Plat_Get_Certificate_Response
      (Ctx    : in out Context;
       Slot   :        RFLX.SPDM.Slot;
       Offset :        RFLX.SPDM.Offset;
       Length :        RFLX.SPDM.Length_16;
       Result :    out RFLX.SPDM.Certificate_Response.Structure)
   is
      procedure C_Interface (Instance     :        System.Address;
                             Data         :        System.Address;
                             Slot         :        Interfaces.C.unsigned_char;
                             Offset       :        Interfaces.C.unsigned_short;
                             Length       : in out Interfaces.C.unsigned_short;
                             Total_Length :    out Interfaces.C.unsigned_short)
      with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_get_certificate_data";
      use type Interfaces.C.unsigned_short;
      use type RFLX.SPDM.Length_16;
      Max_Length : constant RFLX.SPDM.Length_16 := 508;
      Cert_Length : Interfaces.C.unsigned_short;
      Total_Length : Interfaces.C.unsigned_short;
   begin
      if Length <= Max_Length then
         Cert_Length := Interfaces.C.unsigned_short (Length);
      else
         Cert_Length := Interfaces.C.unsigned_short (Max_Length);
      end if;
      C_Interface (Instance     => Ctx.Instance,
                   Data         => Result.Cert_Chain'Address,
                   Slot         => Interfaces.C.unsigned_char (RFLX.SPDM.To_Base (Slot)),
                   Offset       => Interfaces.C.unsigned_short (Offset),
                   Length       => Cert_Length,
                   Total_Length => Total_Length);
      Result.Slot := Slot;
      if Cert_Length = Interfaces.C.unsigned_short (Max_Length) then
         Result.Portion_Length := Max_Length;
      else
         Result.Portion_Length := RFLX.SPDM.Length_16 (Cert_Length);
      end if;
      Result.Remainder_Length := RFLX.SPDM.Length_16 (Total_Length - Cert_Length - Interfaces.C.unsigned_short (Offset));
   end Plat_Get_Certificate_Response;

end SPDM_C_Responder;
