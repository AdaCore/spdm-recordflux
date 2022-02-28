with Channel;
with RFLX.SPDM_Responder.Session;
with RFLX.RFLX_Types;
with SPDM_C_Responder;

procedure Main_Multiple_Responders
is
   use type RFLX.RFLX_Types.Index;
   use type RFLX.RFLX_Types.Length;

   Context_1  : SPDM_C_Responder.Context;
   Context_2  : SPDM_C_Responder.Context;

   package Channel_1 is new Channel;
   package Channel_2 is new Channel;

   package SR renames RFLX.SPDM_Responder.Session;
begin
   SR.Initialize (Context_1);
   SR.Initialize (Context_2);

   while SR.Active (Context_1) or SR.Active (Context_2) loop
      pragma Loop_Invariant (SR.Initialized (Context_1));
      pragma Loop_Invariant (SR.Initialized (Context_2));

      if SR.Active (Context_1) then
         declare
            Skip_Run : Boolean := False;
            Buffer : RFLX.RFLX_Types.Bytes (RFLX.RFLX_Types.Index'First .. RFLX.RFLX_Types.Index'First + 4095);
            Length : RFLX.RFLX_Types.Length;
         begin
            if SR.Has_Data (Context_1, SR.C_Transport) then
               declare
                  BS : constant RFLX.RFLX_Types.Length := SR.Read_Buffer_Size (Context_1, SR.C_Transport);
               begin
                  if Buffer'Length >= BS then
                     SR.Read
                        (Context_1,
                        SR.C_Transport,
                        Buffer (Buffer'First .. Buffer'First - 2 + RFLX.RFLX_Types.Index (BS + 1)));
                     Channel_1.Send (Buffer (Buffer'First .. Buffer'First - 2 + RFLX.RFLX_Types.Index (BS + 1)));
                  end if;
               end;
            end if;

            if SR.Needs_Data (Context_1, SR.C_Transport) then
               if Channel_1.Has_Data then
                  declare
                     BS : constant RFLX.RFLX_Types.Length := SR.Write_Buffer_Size (Context_1, SR.C_Transport);
                  begin
                     Channel_1.Receive (Buffer, Length);
                     if Length > 0 and Length <= BS then
                        SR.Write
                           (Context_1,
                           SR.C_Transport,
                           Buffer (Buffer'First .. Buffer'First +  RFLX.RFLX_Types.Index (Length) - 1));
                     end if;
                  end;
               else
                  Skip_Run := True;
               end if;
            end if;

            if not Skip_Run then
               SR.Run (Context_1);
            end if;
         end;
      end if;

      if SR.Active (Context_2) then
         declare
            Skip_Run : Boolean := False;
            Buffer : RFLX.RFLX_Types.Bytes (RFLX.RFLX_Types.Index'First .. RFLX.RFLX_Types.Index'First + 4095);
            Length : RFLX.RFLX_Types.Length;
         begin
            if SR.Has_Data (Context_2, SR.C_Transport) then
               declare
                  BS : constant RFLX.RFLX_Types.Length := SR.Read_Buffer_Size (Context_2, SR.C_Transport);
               begin
                  if Buffer'Length >= BS then
                     SR.Read
                        (Context_2,
                        SR.C_Transport,
                        Buffer (Buffer'First .. Buffer'First - 2 + RFLX.RFLX_Types.Index (BS + 1)));
                     Channel_2.Send (Buffer (Buffer'First .. Buffer'First - 2 + RFLX.RFLX_Types.Index (BS + 1)));
                  end if;
               end;
            end if;

            if SR.Needs_Data (Context_2, SR.C_Transport) then
               if Channel_2.Has_Data then
                  declare
                     BS : constant RFLX.RFLX_Types.Length := SR.Write_Buffer_Size (Context_2, SR.C_Transport);
                  begin
                     Channel_2.Receive (Buffer, Length);
                     if Length > 0 and Length <= BS then
                        SR.Write
                           (Context_2,
                           SR.C_Transport,
                           Buffer (Buffer'First .. Buffer'First +  RFLX.RFLX_Types.Index (Length) - 1));
                     end if;
                  end;
               else
                  Skip_Run := True;
               end if;
            end if;

            if not Skip_Run then
               SR.Run (Context_2);
            end if;
         end;
      end if;

      --  Execute application code here as required
   end loop;

   pragma Warnings (Off, """*"" is set by ""Finalize"" but not used after the call");
   SR.Finalize (Context_1);
   SR.Finalize (Context_2);
   pragma Warnings (On, """*"" is set by ""Finalize"" but not used after the call");
end Main_Multiple_Responders;
