with Server;
with Channel;
with RFLX.SPDM_Responder.Session;
with RFLX.RFLX_Types;
with RFLX.RFLX_Builtin_Types;
with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Command_Line;
with GNAT.Traceback.Symbolic;
with SPDM_Platform_Interface;

procedure Responder
is
   use type RFLX.RFLX_Types.Index;
   use type RFLX.RFLX_Types.Length;

   Listener   : Server.Server_Type := Server.Create_Server;
   Connection : Server.Server_Type;
   Buffer     : RFLX.RFLX_Types.Bytes (RFLX.RFLX_Types.Index'First .. RFLX.RFLX_Types.Index'First + 4095);
   Length     : RFLX.RFLX_Builtin_Types.Length;

   package SPI renames SPDM_Platform_Interface;

   package Responder is new RFLX.SPDM_Responder.Session
      (Plat_Cfg_CT_Exponent               => SPI.Config_CT_Exponent,
       Plat_Cfg_Cap_MAC                   => SPI.Config_Cap_MAC,
       Plat_Cfg_Cap_Encrypt               => SPI.Config_Cap_Encrypt,
       Plat_Cfg_Cap_Meas_Fresh            => SPI.Config_Cap_Meas_Fresh,
       Plat_Cfg_Cap_Meas                  => SPI.Config_Cap_Meas,
       Plat_Cfg_Cap_Chal                  => SPI.Config_Cap_Chal,
       Plat_Cfg_Cap_Cert                  => SPI.Config_Cap_Cert,
       Plat_Cfg_Cap_Cache                 => SPI.Config_Cap_Cache,
       Plat_Cfg_Cap_Handshake_In_The_Clear=> SPI.Config_Cap_Handshake_In_The_Clear,
       Plat_Cfg_Cap_Key_Upd               => SPI.Config_Cap_Key_Upd,
       Plat_Cfg_Cap_Hbeat                 => SPI.Config_Cap_Hbeat,
       Plat_Cfg_Cap_Encap                 => SPI.Config_Cap_Encap,
       Plat_Cfg_Cap_PSK                   => SPI.Config_Cap_PSK,
       Plat_Cfg_Cap_Key_Ex                => SPI.Config_Cap_Key_Ex,
       Plat_Cfg_Cap_Mut_Auth              => SPI.Config_Cap_Mut_Auth,
       Plat_Cfg_Cap_Pub_Key_ID            => SPI.Config_Cap_Pub_Key_ID,
       Plat_Cfg_Slot_0_Present            => SPI.Config_Slot_0_Present,
       Plat_Cfg_Slot_1_Present            => SPI.Config_Slot_1_Present,
       Plat_Cfg_Slot_2_Present            => SPI.Config_Slot_2_Present,
       Plat_Cfg_Slot_3_Present            => SPI.Config_Slot_3_Present,
       Plat_Cfg_Slot_4_Present            => SPI.Config_Slot_4_Present,
       Plat_Cfg_Slot_5_Present            => SPI.Config_Slot_5_Present,
       Plat_Cfg_Slot_6_Present            => SPI.Config_Slot_6_Present,
       Plat_Cfg_Slot_7_Present            => SPI.Config_Slot_7_Present,
       Plat_Cfg_Sel_Measurement_Hash_Algo => SPI.Select_Measurement_Hash_Algo,
       Plat_Cfg_Sel_Base_Asym_Algo        => SPI.Select_Base_Asym_Algo,
       Plat_Cfg_Sel_Base_Hash_Algo        => SPI.Select_Base_Hash_Algo,
       Plat_Cfg_Sel_DHE                   => SPI.Select_DHE,
       Plat_Cfg_Sel_AEAD                  => SPI.Select_AEAD,
       Plat_Cfg_Sel_RBAA                  => SPI.Select_RBAA,
       Plat_Get_Digests_Data              => SPI.Get_Digests_Data);
   package TCP_Channel is new Channel (Connection);
begin
   Server.Listen (Listener, 2324, Connection);
   Responder.Initialize;
   while Responder.Active loop
      pragma Loop_Invariant (Responder.Initialized);
      for C in Responder.Channel'Range loop
         pragma Loop_Invariant (Responder.Initialized);
         if Responder.Has_Data (C) then
            declare
               BS : constant RFLX.RFLX_Types.Length := Responder.Read_Buffer_Size (C);
            begin
               if Buffer'Length >= BS then
                  Responder.Read (C,
                                  Buffer (Buffer'First .. Buffer'First - 2 + RFLX.RFLX_Types.Index (BS + 1)));
                  TCP_Channel.Send (Buffer (Buffer'First .. Buffer'First - 2 + RFLX.RFLX_Types.Index (BS + 1)));
               end if;
            end;
         end if;

         if Responder.Needs_Data (C) then
            declare
               BS : constant RFLX.RFLX_Types.Length := Responder.Write_Buffer_Size (C);
            begin
               TCP_Channel.Receive (Buffer, Length);
               if Length > 0 and Length <= BS then
                  Responder.Write
                     (C,
                      Buffer (Buffer'First .. Buffer'First +  RFLX.RFLX_Builtin_Types.Index (Length) - 1));
               end if;
            end;
         end if;
      end loop;
      Responder.Run;
   end loop;
   --  ISSUE: Componolit/Workarounds#32
   pragma Warnings (Off, """*"" is set by ""Finalize"" but not used after the call");
   Responder.Finalize;
   pragma Warnings (On, """*"" is set by ""Finalize"" but not used after the call");
exception
   when E : others =>
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E)
                            & ASCII.LF
                            & GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
      Ada.Command_Line.Set_Exit_Status (1);
end Responder;
