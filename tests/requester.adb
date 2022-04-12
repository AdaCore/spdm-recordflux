with Client;
with Channel;
with Ada.Text_IO;
with GNAT.Traceback.Symbolic;
with RFLX.SPDM_Requester.Requester;
with RFLX.RFLX_Types;
with SPDM_Requester;
with Ada.Command_Line;
with Ada.Exceptions;

procedure Requester
is
   use type RFLX.RFLX_Types.Index;
   use type RFLX.RFLX_Types.Length;
   Connection : Client.Connection_Type;
   package Transport is new Channel (Connection);
   function First_Index return Positive is (1);
   package SR is new SPDM_Requester
      (First_Index, Ada.Command_Line.Argument_Count, Ada.Command_Line.Argument);
   package Req renames RFLX.SPDM_Requester.Requester;
   Context : SR.Context;
   Buffer  : RFLX.RFLX_Types.Bytes (RFLX.RFLX_Types.Index'First .. RFLX.RFLX_Types.Index'First + 4095);
   Length  : RFLX.RFLX_Types.Length;
begin
   Req.Initialize (Context);
   Ada.Text_IO.Put_Line ("Connect...");
   Client.Connect (2323, Connection);
   Ada.Text_IO.Put_Line ("Starting...");
   while Req.Active (Context) loop
      if Req.Has_Data (Context, Req.C_Transport) then
         Length := Req.Read_Buffer_Size (Context, Req.C_Transport);
         if Buffer'Length >= Length then
            Req.Read (Context, Req.C_Transport,
                      Buffer (Buffer'First .. Buffer'First - 2 + RFLX.RFLX_Types.Index (Length + 1)));
            Transport.Send (Buffer (Buffer'First .. Buffer'First - 2 + RFLX.RFLX_Types.Index (Length + 1)));
         end if;
      end if;
      if Req.Needs_Data (Context, Req.C_Transport) then
         Transport.Receive (Buffer, Length);
         if Length > 0 and Length <= Req.Write_Buffer_Size (Context, Req.C_Transport) then
            Req.Write (Context, Req.C_Transport,
                      Buffer (Buffer'First .. Buffer'First + RFLX.RFLX_Types.Index (Length) - 1));
         end if;
      end if;
      Req.Run (Context);
   end loop;
   Req.Finalize (Context);
exception
   when E : others =>
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E)
                            & ASCII.LF
                            & GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
      Ada.Command_Line.Set_Exit_Status (1);
end Requester;
