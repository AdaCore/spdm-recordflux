with RFLX.SPDM_Responder.Session;
with RFLX.RFLX_Types;
with RFLX.RFLX_Builtin_Types;

procedure Embedded
is
   package Responder is new RFLX.SPDM_Responder.Session;
   use type RFLX.RFLX_Types.Index;
   Buffer : RFLX.RFLX_Types.Bytes (RFLX.RFLX_Types.Index'First .. RFLX.RFLX_Types.Index'First + 4095);
begin
   Responder.Initialize;
   while Responder.Active loop
      for C in Responder.Channel'Range loop
         if Responder.Has_Data (C) then
            Responder.Read (C, Buffer);
            pragma Inspection_Point (Buffer);
         end if;
         if Responder.Needs_Data (C) then
            Responder.Write (C, Buffer);
         end if;
      end loop;
      Responder.Run;
   end loop;
   pragma Warnings (Off, """*"" is set by ""Finalize"" but not used after the call");
   Responder.Finalize;
   pragma Warnings (On, """*"" is set by ""Finalize"" but not used after the call");
end Embedded;
