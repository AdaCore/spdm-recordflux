with RFLX.SPDM_Responder.Session;
with RFLX.RFLX_Types;
with RFLX.RFLX_Builtin_Types;

with SPDM_C_Responder;

procedure Main
is
   use type RFLX.RFLX_Types.Index;

   Buffer : RFLX.RFLX_Types.Bytes (RFLX.RFLX_Types.Index'First .. RFLX.RFLX_Types.Index'First + 4095);
   Context : SPDM_C_Responder.Context;

   package SR renames RFLX.SPDM_Responder.Session;
begin
   SR.Initialize (Context);
   while SR.Active (Context) loop
      for C in SR.Channel'Range loop
         if SR.Has_Data (Context, C) then
            SR.Read (Context, C, Buffer);
            pragma Inspection_Point (Buffer);
         end if;
         if SR.Needs_Data (Context, C) then
            SR.Write (Context, C, Buffer);
         end if;
      end loop;
      SR.Run (Context);
   end loop;
   pragma Warnings (Off, """*"" is set by ""Finalize"" but not used after the call");
   SR.Finalize (Context);
   pragma Warnings (On, """*"" is set by ""Finalize"" but not used after the call");
end Main;
