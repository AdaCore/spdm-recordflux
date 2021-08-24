with Server;
with Channel;
with RFLX.SPDM.Responder;
with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Command_Line;
with GNAT.Traceback.Symbolic;

procedure Main
is
   Listener : Server.Server_Type := Server.Create_Server;
   Connection : Server.Server_Type;
begin
   Server.Listen (Listener, 2323, Connection);
   declare
      package TCP_Channel is new Channel (Connection);
      package Responder is new RFLX.SPDM.Responder (TCP_Channel.Has_Data,
                                                    TCP_Channel.Receive,
                                                    TCP_Channel.Send);
   begin
      Responder.Run;
   end;
exception
   when E : others =>
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E)
                            & ASCII.LF
                            & GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
      Ada.Command_Line.Set_Exit_Status (1);
end Main;
