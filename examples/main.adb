with SPDM_Platform_Interface;

with RFLX.SPDM_Responder.Session;
with RFLX.RFLX_Types;
with RFLX.RFLX_Builtin_Types;

procedure Main
is
   package Responder is new RFLX.SPDM_Responder.Session
      (Plat_Cfg_CT_Exponent               => SPDM_Platform_Interface.Config_CT_Exponent,
       Plat_Cfg_Cap_MAC                   => SPDM_Platform_Interface.Config_Cap_MAC,
       Plat_Cfg_Cap_Encrypt               => SPDM_Platform_Interface.Config_Cap_Encrypt,
       Plat_Cfg_Cap_Meas_Fresh            => SPDM_Platform_Interface.Config_Cap_Meas_Fresh,
       Plat_Cfg_Cap_Meas                  => SPDM_Platform_Interface.Config_Cap_Meas,
       Plat_Cfg_Cap_Chal                  => SPDM_Platform_Interface.Config_Cap_Chal,
       Plat_Cfg_Cap_Cert                  => SPDM_Platform_Interface.Config_Cap_Cert,
       Plat_Cfg_Cap_Cache                 => SPDM_Platform_Interface.Config_Cap_Cache,
       Plat_Cfg_Cap_Handshake_In_The_Clear=> SPDM_Platform_Interface.Config_Cap_Handshake_In_The_Clear,
       Plat_Cfg_Cap_Key_Upd               => SPDM_Platform_Interface.Config_Cap_Key_Upd,
       Plat_Cfg_Cap_Hbeat                 => SPDM_Platform_Interface.Config_Cap_Hbeat,
       Plat_Cfg_Cap_Encap                 => SPDM_Platform_Interface.Config_Cap_Encap,
       Plat_Cfg_Cap_PSK                   => SPDM_Platform_Interface.Config_Cap_PSK,
       Plat_Cfg_Cap_Key_Ex                => SPDM_Platform_Interface.Config_Cap_Key_Ex,
       Plat_Cfg_Cap_Mut_Auth              => SPDM_Platform_Interface.Config_Cap_Mut_Auth,
       Plat_Cfg_Cap_Pub_Key_ID            => SPDM_Platform_Interface.Config_Cap_Pub_Key_ID,
       Plat_Cfg_Sel_Measurement_Hash_Algo => SPDM_Platform_Interface.Select_Measurement_Hash_Algo,
       Plat_Cfg_Sel_Base_Asym_Algo        => SPDM_Platform_Interface.Select_Base_Asym_Algo,
       Plat_Cfg_Sel_Base_Hash_Algo        => SPDM_Platform_Interface.Select_Base_Hash_Algo,
       Plat_Cfg_Sel_DHE                   => SPDM_Platform_Interface.Select_DHE,
       Plat_Cfg_Sel_AEAD                  => SPDM_Platform_Interface.Select_AEAD,
       Plat_Cfg_Sel_RBAA                  => SPDM_Platform_Interface.Select_RBAA,
       Plat_Get_Digests_Data              => SPDM_Platform_Interface.Get_Digests_Data);
   use type RFLX.RFLX_Types.Index;
   Buffer : RFLX.RFLX_Types.Bytes (RFLX.RFLX_Types.Index'First .. RFLX.RFLX_Types.Index'First + 4095);
begin
   Responder.Initialize;
   while Responder.Active loop
      for C in Responder.Channel'Range loop
         if Responder.Has_Data (C) then
            Responder.Read (C, Buffer);
            pragma Inspection_Point (Buffer);
         end if;
         if Responder.Needs_Data (C) then
            Responder.Write (C, Buffer);
         end if;
      end loop;
      Responder.Run;
   end loop;
   pragma Warnings (Off, """*"" is set by ""Finalize"" but not used after the call");
   Responder.Finalize;
   pragma Warnings (On, """*"" is set by ""Finalize"" but not used after the call");
end Main;
