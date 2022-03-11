with RFLX.RFLX_Types;
with RFLX.SPDM_Responder.Session;
with SPDM_C_Responder;

package Main_Data with
   SPARK_Mode,
   Initial_Condition => RFLX.SPDM_Responder.Session.Uninitialized (Context)
is
   use type RFLX.RFLX_Types.Index;
   Buffer : RFLX.RFLX_Types.Bytes (RFLX.RFLX_Types.Index'First .. RFLX.RFLX_Types.Index'First + 1279) := (others => 0);
   Context : SPDM_C_Responder.Context;
end Main_Data;
