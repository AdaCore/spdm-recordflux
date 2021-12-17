with Interfaces.C;

package body SPDM_Platform_Interface is

   procedure Config_CT_Exponent (Platform_Config_CT_Exponent : out RFLX.SPDM.CT_Exponent)
   is
      function C_Config_CT_Exponent return Interfaces.C.unsigned_char with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_config_ct_exponent";
   begin
      Platform_Config_CT_Exponent := RFLX.SPDM.CT_Exponent (C_Config_CT_Exponent);
   end Config_CT_Exponent;

   procedure Config_CAP_MAC (Platform_Config_CAP_MAC : out Boolean)
   is
      function C_Config_CAP_MAC return Interfaces.C.unsigned_char with
         Import        => True,
         Convention    => C,
         External_Name => "spdm_platform_config_cap_mac";
      use type Interfaces.C.unsigned_char;
   begin
      Platform_Config_CAP_MAC := C_Config_CAP_MAC > 0;
   end Config_CAP_MAC;

end SPDM_Platform_Interface;
