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

end SPDM_Platform_Interface;
