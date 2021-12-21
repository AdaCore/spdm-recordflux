with RFLX.SPDM;

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

end SPDM_Platform_Interface;
