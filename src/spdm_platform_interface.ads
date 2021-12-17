with RFLX.SPDM;

package SPDM_Platform_Interface is

   --  Retrieve CT exponent configuration from platform
   procedure Config_CT_Exponent (Platform_Config_CT_Exponent : out RFLX.SPDM.CT_Exponent);

   --  Retrieve CAP_MAC
   procedure Config_CAP_MAC (Platform_Config_CAP_MAC : out Boolean);

end SPDM_Platform_Interface;
