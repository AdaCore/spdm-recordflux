with Server;
with Channel;
with RFLX.SPDM.Responder;
with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Command_Line;
with GNAT.Traceback.Symbolic;

procedure Responder
is
   Listener : Server.Server_Type := Server.Create_Server;
   Connection : Server.Server_Type;
   procedure Signal_Error (Signal_Error : out Boolean;
                           Value        :     Boolean)
   is
   begin
      Signal_Error := Value;
      Ada.Command_Line.Set_Exit_Status (1);
   end Signal_Error;
begin
   Server.Listen (Listener, 2324, Connection);
   declare
      package TCP_Channel is new Channel (Connection);
      package Responder is new RFLX.SPDM.Responder (TCP_Channel.Has_Data,
                                                    TCP_Channel.Receive,
                                                    TCP_Channel.Send,
                                                    Signal_Error);
   begin
      Responder.Run;
   end;
exception
   when E : others =>
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E)
                            & ASCII.LF
                            & GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
      Ada.Command_Line.Set_Exit_Status (1);
end Responder;
