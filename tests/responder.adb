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

   package Responder is new RFLX.SPDM_Responder.Session
      (Platform_Config_CT_Exponent                => SPDM_Platform_Interface.Config_CT_Exponent,
       Platform_Config_Cap_MAC                    => SPDM_Platform_Interface.Config_Cap_MAC,
       Platform_Config_Cap_Encrypt                => SPDM_Platform_Interface.Config_Cap_Encrypt,
       Platform_Config_Cap_Meas_Fresh             => SPDM_Platform_Interface.Config_Cap_Meas_Fresh,
       Platform_Config_Cap_Meas                   => SPDM_Platform_Interface.Config_Cap_Meas,
       Platform_Config_Cap_Chal                   => SPDM_Platform_Interface.Config_Cap_Chal,
       Platform_Config_Cap_Cert                   => SPDM_Platform_Interface.Config_Cap_Cert,
       Platform_Config_Cap_Cache                  => SPDM_Platform_Interface.Config_Cap_Cache,
       Platform_Config_Cap_Handshake_In_The_Clear => SPDM_Platform_Interface.Config_Cap_Handshake_In_The_Clear,
       Platform_Config_Cap_Key_Upd                => SPDM_Platform_Interface.Config_Cap_Key_Upd,
       Platform_Config_Cap_Hbeat                  => SPDM_Platform_Interface.Config_Cap_Hbeat,
       Platform_Config_Cap_Encap                  => SPDM_Platform_Interface.Config_Cap_Encap,
       Platform_Config_Cap_PSK                    => SPDM_Platform_Interface.Config_Cap_PSK,
       Platform_Config_Cap_Key_Ex                 => SPDM_Platform_Interface.Config_Cap_Key_Ex,
       Platform_Config_Cap_Mut_Auth               => SPDM_Platform_Interface.Config_Cap_Mut_Auth,
       Platform_Config_Cap_Pub_Key_ID             => SPDM_Platform_Interface.Config_Cap_Pub_Key_ID);
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
