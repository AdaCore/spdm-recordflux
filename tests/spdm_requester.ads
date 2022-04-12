with RFLX.SPDM_Requester.Request;
with RFLX.SPDM_Requester.Requester;
with RFLX.RFLX_Types;

generic
   with function First_Index return Positive;
   with function Last_Index return Natural;
   with function Request_Arg (Index : Positive) return String;
package SPDM_Requester with
   SPARK_Mode
is

   type Context is new RFLX.SPDM_Requester.Requester.Context with record
      Index : Positive := First_Index;
   end record;

   overriding
   procedure Get_Request_Data (Ctx    : in out Context;
                               Result :    out RFLX.SPDM_Requester.Request.Structure);

   overriding
   procedure Return_Response (Ctx    : in out Context;
                              Resp   :        RFLX.RFLX_Types.Bytes;
                              Result :    out Boolean);

end SPDM_Requester;
