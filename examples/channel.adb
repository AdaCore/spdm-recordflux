package body Channel is

   procedure Send (Buffer : RFLX.RFLX_Types.Bytes) is null;

   procedure Receive (Buffer : out RFLX.RFLX_Types.Bytes; Length : out RFLX.RFLX_Types.Length) is null;

   function Has_Data return Boolean is (False);

end Channel;
