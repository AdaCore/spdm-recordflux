with RFLX.SPDM_Responder.Session;
with RFLX.RFLX_Types;

with Main_Data;

procedure Main with
   SPARK_Mode,
   Pre =>
      RFLX.SPDM_Responder.Session.Uninitialized (Main_Data.Context)
is
   use type RFLX.RFLX_Types.Length;
   use type RFLX.RFLX_Types.Index;
   package SR renames RFLX.SPDM_Responder.Session;
begin
   loop
      pragma Loop_Invariant (SR.Uninitialized (Main_Data.Context));

      SR.Initialize (Main_Data.Context);

      while SR.Active (Main_Data.Context) loop
         pragma Loop_Invariant (SR.Initialized (Main_Data.Context));

         if SR.Has_Data (Main_Data.Context, SR.C_Transport) then
            declare
               BS : constant RFLX.RFLX_Types.Length := SR.Read_Buffer_Size (Main_Data.Context, SR.C_Transport);
            begin
               if Main_Data.Buffer'Length >= BS then
                  SR.Read
                     (Main_Data.Context,
                      SR.C_Transport,
                      Main_Data.Buffer (Main_Data.Buffer'First .. Main_Data.Buffer'First - 2 + RFLX.RFLX_Types.Index (BS + 1)));
                  pragma Inspection_Point (Main_Data.Buffer);
               end if;
            end;
         end if;

         if SR.Needs_Data (Main_Data.Context, SR.C_Transport) then
            SR.Write (Main_Data.Context, SR.C_Transport, Main_Data.Buffer);
         end if;

         SR.Run (Main_Data.Context);
      end loop;

      SR.Finalize (Main_Data.Context);
   end loop;
end Main;
