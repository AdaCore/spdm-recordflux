with GNAT.Sockets;
with RFLX.RFLX_Types;

generic
   Socket : in out GNAT.Sockets.Socket_Type;
package Channel with
  SPARK_Mode
is

   procedure Send (Buffer : RFLX.RFLX_Types.Bytes) with
      Global =>
         (Output => Socket);

   use type RFLX.RFLX_Types.Length;

   procedure Receive (Buffer : out RFLX.RFLX_Types.Bytes; Length : out RFLX.RFLX_Types.Length) with
      Post =>
         Length <= Buffer'Length;

   function Has_Data return Boolean;

end Channel;
