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

   package SR renames RFLX.SPDM_Responder.Session;

   generic
      with procedure Send (Buffer : RFLX.RFLX_Types.Bytes);
      with procedure Receive (Buffer : out RFLX.RFLX_Types.Bytes; Length : out RFLX.RFLX_Types.Length);
      with function Has_Message return Boolean;
   procedure Run_Responder (Context : in out SPDM_C_Responder.Context) with
      Pre =>
         SR.Initialized (Context),
      Post =>
         SR.Initialized (Context);

   procedure Run_Responder (Context : in out SPDM_C_Responder.Context) is
   begin
      if SR.Active (Context) then
         declare
            Skip_Run : Boolean := False;
            Buffer : RFLX.RFLX_Types.Bytes (RFLX.RFLX_Types.Index'First .. RFLX.RFLX_Types.Index'First + 4095) := (others => 0);
            Length : RFLX.RFLX_Types.Length;
         begin
            if SR.Has_Data (Context, SR.C_Transport) then
               declare
                  BS : constant RFLX.RFLX_Types.Length := SR.Read_Buffer_Size (Context, SR.C_Transport);
               begin
                  if Buffer'Length >= BS then
                     SR.Read
                        (Context,
                         SR.C_Transport,
                         Buffer (Buffer'First .. Buffer'First - 2 + RFLX.RFLX_Types.Index (BS + 1)));
                     Send (Buffer (Buffer'First .. Buffer'First - 2 + RFLX.RFLX_Types.Index (BS + 1)));
                  end if;
               end;
            end if;

            if SR.Needs_Data (Context, SR.C_Transport) then
               if Has_Message then
                  declare
                     BS : constant RFLX.RFLX_Types.Length := SR.Write_Buffer_Size (Context, SR.C_Transport);
                  begin
                     Receive (Buffer, Length);
                     if Length > 0 and Length <= BS then
                        SR.Write
                           (Context,
                            SR.C_Transport,
                            Buffer (Buffer'First .. Buffer'First +  RFLX.RFLX_Types.Index (Length) - 1));
                     end if;
                  end;
               else
                  Skip_Run := True;
               end if;
            end if;

            if not Skip_Run then
               SR.Run (Context);
            end if;
         end;
      end if;
   end Run_Responder;

   package Channel_1 is new Channel;
   package Channel_2 is new Channel;

   procedure Run_Responder_1 is new Run_Responder (Channel_1.Send, Channel_1.Receive, Channel_1.Has_Message);
   procedure Run_Responder_2 is new Run_Responder (Channel_2.Send, Channel_2.Receive, Channel_2.Has_Message);
begin
   SR.Initialize (Context_1);
   SR.Initialize (Context_2);
   loop
      if not SR.Initialized (Context_1) then
         SR.Initialize (Context_1);
      end if;
      if not SR.Initialized (Context_2) then
         SR.Initialize (Context_2);
      end if;

      while SR.Active (Context_1) and SR.Active (Context_2) loop
         pragma Loop_Invariant (SR.Initialized (Context_1));
         pragma Loop_Invariant (SR.Initialized (Context_2));

         Run_Responder_1 (Context_1);
         Run_Responder_2 (Context_2);

         --  Execute application code here as required
      end loop;

      if not SR.Active (Context_1) then
         SR.Finalize (Context_1);
      end if;
      if not SR.Active (Context_2) then
         SR.Finalize (Context_2);
      end if;
   end loop;
end Main_Multiple_Responders;
