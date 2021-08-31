with Client;
with Server;
with Channel;
with Ada.Exceptions;
with Ada.Command_Line;
with Ada.Text_IO;
with GNAT.Traceback.Symbolic;
with RFLX.SPDM_Proxy.Proxy;

procedure Proxy
is
   Listener : Server.Server_Type := Server.Create_Server;
   Server_Connection : Server.Server_Type;
   Client_Connection : Client.Connection_Type;
begin
   Client.Connect (2324, Client_Connection);
   Server.Listen (Listener, 2323, Server_Connection);
   declare
      package SPDM_Channel is new Channel (Client_Connection);
      package Emu_Channel is new Channel (Server_Connection);
      package SPDM_Proxy is new RFLX.SPDM_Proxy.Proxy (Emu_Channel.Has_Data,
                                                       Emu_Channel.Receive,
                                                       Emu_Channel.Send,
                                                       SPDM_Channel.Has_Data,
                                                       SPDM_Channel.Receive,
                                                       SPDM_Channel.Send);
   begin
      SPDM_Proxy.Run;
   end;
exception
   when E : others =>
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E)
                            & ASCII.LF
                            & GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
      Ada.Command_Line.Set_Exit_Status (1);
end Proxy;
