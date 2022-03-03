with Server;
with Channel;
with RFLX.SPDM_Responder.Session;
with RFLX.RFLX_Types;
with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Command_Line;
with GNAT.Traceback.Symbolic;
with SPDM_C_Responder;

procedure Responder
is
   use type RFLX.RFLX_Types.Index;
   use type RFLX.RFLX_Types.Length;

   Listener   : Server.Server_Type := Server.Create_Server;
   Connection : Server.Server_Type;
   Buffer     : RFLX.RFLX_Types.Bytes (RFLX.RFLX_Types.Index'First .. RFLX.RFLX_Types.Index'First + 4095);
   Length     : RFLX.RFLX_Types.Length;
   Context    : SPDM_C_Responder.Context;

   package TCP_Channel is new Channel (Connection);
   package SR renames RFLX.SPDM_Responder.Session;
begin
   loop
      Server.Listen (Listener, 2324, Connection);
      SR.Initialize (Context);
      while SR.Active (Context) loop
         pragma Loop_Invariant (SR.Initialized (Context));
         for C in SR.Channel'Range loop
            pragma Loop_Invariant (SR.Initialized (Context));
            if SR.Has_Data (Context, C) then
               declare
                  BS : constant RFLX.RFLX_Types.Length := SR.Read_Buffer_Size (Context, C);
               begin
                  if Buffer'Length >= BS then
                     SR.Read
                        (Context,
                         C,
                         Buffer (Buffer'First .. Buffer'First - 2 + RFLX.RFLX_Types.Index (BS + 1)));
                     TCP_Channel.Send (Buffer (Buffer'First .. Buffer'First - 2 + RFLX.RFLX_Types.Index (BS + 1)));
                  end if;
               end;
            end if;

            if SR.Needs_Data (Context, C) then
               declare
                  BS : constant RFLX.RFLX_Types.Length := SR.Write_Buffer_Size (Context, C);
               begin
                  TCP_Channel.Receive (Buffer, Length);
                  if Length > 0 and Length <= BS then
                     SR.Write
                        (Context,
                         C,
                         Buffer (Buffer'First .. Buffer'First +  RFLX.RFLX_Types.Index (Length) - 1));
                  end if;
               end;
            end if;
         end loop;
         SR.Run (Context);
      end loop;
      --  ISSUE: Componolit/Workarounds#32
      pragma Warnings (Off, """*"" is set by ""Finalize"" but not used after the call");
      SR.Finalize (Context);
      pragma Warnings (On, """*"" is set by ""Finalize"" but not used after the call");
   end loop;
exception
   when E : others =>
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E)
                            & ASCII.LF
                            & GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
      Ada.Command_Line.Set_Exit_Status (1);
end Responder;
