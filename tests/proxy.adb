with Client;
with Server;
with Channel;
with Ada.Exceptions;
with Ada.Command_Line;
with Ada.Text_IO;
with GNAT.Traceback.Symbolic;
with RFLX.SPDM_Proxy.Proxy;
with RFLX.RFLX_Types;
with RFLX.RFLX_Builtin_Types;

procedure Proxy
is
   use type RFLX.RFLX_Types.Index;
   use type RFLX.RFLX_Types.Length;

   Listener          : Server.Server_Type := Server.Create_Server;
   Server_Connection : Server.Server_Type;
   Client_Connection : Client.Connection_Type;
   Buffer            : RFLX.RFLX_Types.Bytes (RFLX.RFLX_Types.Index'First .. RFLX.RFLX_Types.Index'First + 4095);
   Length            : RFLX.RFLX_Builtin_Types.Length;

   package Responder_Channel is new Channel (Client_Connection);
   package Emu_Channel is new Channel (Server_Connection);
   package SPDM_Proxy is new RFLX.SPDM_Proxy.Proxy;
begin
   SPDM_Proxy.Initialize;
   Client.Connect (2324, Client_Connection);
   Server.Listen (Listener, 2323, Server_Connection);

   while SPDM_Proxy.Active loop

      pragma Loop_Invariant (SPDM_Proxy.Initialized);

      if SPDM_Proxy.Has_Data (SPDM_Proxy.C_Emu_Transport) then
         declare
            BS : constant RFLX.RFLX_Types.Length := SPDM_Proxy.Read_Buffer_Size (SPDM_Proxy.C_Emu_Transport);
         begin
            if Buffer'Length >= BS then
               SPDM_Proxy.Read (SPDM_Proxy.C_Emu_Transport,
                                Buffer (Buffer'First .. Buffer'First - 2 + RFLX.RFLX_Types.Index (BS + 1)));
               Emu_Channel.Send (Buffer (Buffer'First .. Buffer'First - 2 + RFLX.RFLX_Types.Index (BS + 1)));
            end if;
         end;
      end if;

      if SPDM_Proxy.Needs_Data (SPDM_Proxy.C_Emu_Transport) then
         declare
            BS : constant RFLX.RFLX_Types.Length := SPDM_Proxy.Write_Buffer_Size (SPDM_Proxy.C_Emu_Transport);
         begin
            Emu_Channel.Receive (Buffer, Length);
            if Length > 0 and Length <= BS then
               SPDM_Proxy.Write
                  (SPDM_Proxy.C_Emu_Transport,
                   Buffer (Buffer'First .. Buffer'First +  RFLX.RFLX_Builtin_Types.Index (Length) - 1));
            end if;
         end;
      end if;

      if SPDM_Proxy.Has_Data (SPDM_Proxy.C_SPDM_Transport) then
         declare
            BS : constant RFLX.RFLX_Types.Length := SPDM_Proxy.Read_Buffer_Size (SPDM_Proxy.C_SPDM_Transport);
         begin
            if Buffer'Length >= BS then
               SPDM_Proxy.Read (SPDM_Proxy.C_SPDM_Transport,
                                Buffer (Buffer'First .. Buffer'First - 2 + RFLX.RFLX_Types.Index (BS + 1)));
               Responder_Channel.Send (Buffer (Buffer'First .. Buffer'First - 2 + RFLX.RFLX_Types.Index (BS + 1)));
            end if;
         end;
      end if;

      if SPDM_Proxy.Needs_Data (SPDM_Proxy.C_SPDM_Transport) then
         declare
            BS : constant RFLX.RFLX_Types.Length := SPDM_Proxy.Write_Buffer_Size (SPDM_Proxy.C_SPDM_Transport);
         begin
            Responder_Channel.Receive (Buffer, Length);
            if Length > 0 and Length <= BS then
               SPDM_Proxy.Write
                  (SPDM_Proxy.C_SPDM_Transport,
                   Buffer (Buffer'First .. Buffer'First +  RFLX.RFLX_Builtin_Types.Index (Length) - 1));
            end if;
         end;
      end if;

      SPDM_Proxy.Run;
   end loop;

   --  ISSUE: Componolit/Workarounds#32
   pragma Warnings (Off, """*"" is set by ""Finalize"" but not used after the call");
   SPDM_Proxy.Finalize;
   pragma Warnings (On, """*"" is set by ""Finalize"" but not used after the call");
exception
   when E : others =>
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E)
                            & ASCII.LF
                            & GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
      Ada.Command_Line.Set_Exit_Status (1);
end Proxy;
