with RFLX.RFLX_Types;
with SPDM_C_Responder;

package Main_Data is
   use type RFLX.RFLX_Types.Index;
   Buffer : RFLX.RFLX_Types.Bytes (RFLX.RFLX_Types.Index'First .. RFLX.RFLX_Types.Index'First + 1279);
   Context : SPDM_C_Responder.Context;
end Main_Data;
