with Channel;
with RFLX.SPDM_Responder.Session;
with RFLX.RFLX_Types;
with RFLX.RFLX_Types.Operators;
with SPDM_C_Responder;

package body Responder_Multiple_Responders with
   SPARK_Mode,
   Refined_State => (Responder_State => (Context_1, Context_2))
is

   Context_1  : SPDM_C_Responder.Context;
   Context_2  : SPDM_C_Responder.Context;

   function Uninitialized return Boolean is
      (RFLX.SPDM_Responder.Session.Uninitialized (Context_1)
       and RFLX.SPDM_Responder.Session.Uninitialized (Context_2));

   procedure Responder_Main with
      Pre    => Uninitialized,
      Post   => Uninitialized,
      Global => (In_Out => (Context_1, Context_2));

   procedure Main
   is
   begin
      if Uninitialized then
         Responder_Main;
      end if;
   end Main;

   procedure Responder_Main
   is
      use RFLX.RFLX_Types;
      use RFLX.RFLX_Types.Operators;

      package SR renames RFLX.SPDM_Responder.Session;

      generic
         with procedure Send (Buffer : Bytes);
         with procedure Receive (Buffer : out Bytes; Length : out RFLX.RFLX_Types.Length);
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
               Buffer : Bytes (Index'First .. Index'First + 4095) := (others => 0);
               Length : RFLX.RFLX_Types.Length;
            begin
               if SR.Has_Data (Context, SR.C_Transport) then
                  declare
                     use type RFLX.RFLX_Types.Length;
                     BS : constant RFLX.RFLX_Types.Length := SR.Read_Buffer_Size (Context, SR.C_Transport);
                  begin
                     if Buffer'Length >= BS then
                        SR.Read
                           (Context,
                            SR.C_Transport,
                            Buffer (Buffer'First .. Buffer'First + BS - 1));
                        Send (Buffer (Buffer'First .. Buffer'First + BS - 1));
                     end if;
                  end;
               end if;

               if SR.Needs_Data (Context, SR.C_Transport) then
                  if Has_Message then
                     declare
                        BS : constant RFLX.RFLX_Types.Length := SR.Write_Buffer_Size (Context, SR.C_Transport);
                     begin
                        Receive (Buffer, Length);
                        if Length in 1 .. BS then
                           SR.Write
                              (Context,
                               SR.C_Transport,
                               Buffer (Buffer'First .. Buffer'First + Length - 1));
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
      --  Eng/RecordFlux/RecordFlux#1032
      --  Context_1.Plat_Initialize;
      --  Context_2.Plat_Initialize;
      SR.Initialize (Context_1);
      SR.Initialize (Context_2);

      loop
         pragma Loop_Invariant (SR.Initialized (Context_1));
         pragma Loop_Invariant (SR.Initialized (Context_2));

         while SR.Active (Context_1) and SR.Active (Context_2) loop
            pragma Loop_Invariant (SR.Initialized (Context_1));
            pragma Loop_Invariant (SR.Initialized (Context_2));

            Run_Responder_1 (Context_1);
            Run_Responder_2 (Context_2);

            --  Execute application code here as required
         end loop;

         pragma Assert (SR.Initialized (Context_1));
         pragma Assert (SR.Initialized (Context_2));

         if not SR.Active (Context_1) then
            SR.Finalize (Context_1);
            SR.Initialize (Context_1);
         end if;
         if not SR.Active (Context_2) then
            SR.Finalize (Context_2);
            SR.Initialize (Context_2);
         end if;
      end loop;
   end Responder_Main;

end Responder_Multiple_Responders;
