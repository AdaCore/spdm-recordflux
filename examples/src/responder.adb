with RFLX.SPDM_Responder.Session;
with RFLX.RFLX_Types;
with SPDM_C_Responder;

package body Responder with
   SPARK_Mode,
   Refined_State => (Responder_State => (Buffer, Context))
is
   use type RFLX.RFLX_Types.Index;
   Buffer : RFLX.RFLX_Types.Bytes (RFLX.RFLX_Types.Index'First .. RFLX.RFLX_Types.Index'First + 1279) := (others => 0);
   Context : SPDM_C_Responder.Context;
   pragma Annotate (GNATprove, False_Positive,
                    """Context.P"" constituent of ""Responder_State"" is not initialized",
                    "ISSUE: Componolit/RecordFlux#954");

   function Uninitialized return Boolean is (RFLX.SPDM_Responder.Session.Uninitialized (Context));

   procedure Responder_Main with
      Pre => Uninitialized,
      Post => Uninitialized,
      Global => (In_Out => (Buffer, Context));

   procedure Main
   is
   begin
      if Uninitialized then
         Responder_Main;
      end if;
   end Main;

   procedure Responder_Main
   is
      use type RFLX.RFLX_Types.Length;
      package SR renames RFLX.SPDM_Responder.Session;
   begin
      loop
         pragma Loop_Invariant (SR.Uninitialized (Context));

         SR.Initialize (Context);

         while SR.Active (Context) loop
            pragma Loop_Invariant (SR.Initialized (Context));

            if SR.Has_Data (Context, SR.C_Transport) then
               declare
                  BS : constant RFLX.RFLX_Types.Length := SR.Read_Buffer_Size (Context, SR.C_Transport);
               begin
                  if Buffer'Length >= BS then
                     SR.Read
                        (Context,
                         SR.C_Transport,
                         Buffer (Buffer'First .. Buffer'First - 2 + RFLX.RFLX_Types.Index (BS + 1)));
                     pragma Inspection_Point (Buffer);
                  end if;
               end;
            end if;

            if SR.Needs_Data (Context, SR.C_Transport) then
               SR.Write (Context, SR.C_Transport, Buffer);
            end if;

            SR.Run (Context);
         end loop;

         SR.Finalize (Context);
      end loop;
   end Responder_Main;

end Responder;
