with RFLX.SPDM_Responder.Session;
with RFLX.RFLX_Builtin_Types;

with Main_Data;

procedure Main
is
   package SR renames RFLX.SPDM_Responder.Session;
begin
   SR.Initialize (Main_Data.Context);
   while SR.Active (Main_Data.Context) loop
      for C in SR.Channel'Range loop
         if SR.Has_Data (Main_Data.Context, C) then
            SR.Read (Main_Data.Context, C, Main_Data.Buffer);
            pragma Inspection_Point (Main_Data.Buffer);
         end if;
         if SR.Needs_Data (Main_Data.Context, C) then
            SR.Write (Main_Data.Context, C, Main_Data.Buffer);
         end if;
      end loop;
      SR.Run (Main_Data.Context);
   end loop;
   pragma Warnings (Off, """*"" is set by ""Finalize"" but not used after the call");
   SR.Finalize (Main_Data.Context);
   pragma Warnings (On, """*"" is set by ""Finalize"" but not used after the call");
end Main;
