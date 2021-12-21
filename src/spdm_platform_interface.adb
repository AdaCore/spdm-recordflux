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

end SPDM_Platform_Interface;
