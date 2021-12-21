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

end SPDM_Platform_Interface;
