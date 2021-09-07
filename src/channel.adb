with Ada.Streams;

package body Channel is

   use type RFLX.RFLX_Builtin_Types.Index;
   use type Ada.Streams.Stream_Element_Offset;

   --  ISSUE: Componolit/RecordFlux#482
   --  Ada.Streams.Stream_Element_Array is not yet supported as buffer type and thus a conversion is needed.

   function To_Ada_Stream (Buffer : RFLX.RFLX_Builtin_Types.Bytes) return Ada.Streams.Stream_Element_Array with
     Pre => Buffer'First = 1
   is
      Data : Ada.Streams.Stream_Element_Array (1 .. Buffer'Length);
   begin
      for I in Buffer'Range loop
         Data (Ada.Streams.Stream_Element_Offset (I)) := Ada.Streams.Stream_Element (Buffer (I));
      end loop;
      return Data;
   end To_Ada_Stream;

   function To_RFLX_Bytes (Buffer : Ada.Streams.Stream_Element_Array) return RFLX.RFLX_Builtin_Types.Bytes with
     Pre => Buffer'First = 1
   is
      Data : RFLX.RFLX_Builtin_Types.Bytes (1 .. Buffer'Length);
   begin
      for I in Buffer'Range loop
         Data (RFLX.RFLX_Builtin_Types.Index (I)) := RFLX.RFLX_Builtin_Types.Byte (Buffer (I));
      end loop;
      return Data;
   end To_RFLX_Bytes;

   procedure Send (Buffer : RFLX.RFLX_Builtin_Types.Bytes)
   is
      Data : constant Ada.Streams.Stream_Element_Array (1 .. Buffer'Length) := To_Ada_Stream (Buffer);
      Last : Ada.Streams.Stream_Element_Offset;
   begin
      pragma Unreferenced (Last);
      GNAT.Sockets.Send_Socket (Socket => Socket,
                                Item => Data,
                                Last => Last);
   end Send;

   procedure Receive (Buffer : out RFLX.RFLX_Builtin_Types.Bytes; Length : out RFLX.RFLX_Builtin_Types.Length) is
      Data : Ada.Streams.Stream_Element_Array (1 .. Buffer'Length);
      Last : Ada.Streams.Stream_Element_Offset;
   begin
      --  FIXME: The SPDM requester sends a packet in multiple parts, we have to wait for the whole packet to parse it
      --  ISSUE: Componolit/RecordFlux#80
      --  ISSUE: Componolit/RecordFlux#644
      while not Has_Data loop
         delay 0.1;
      end loop;
      delay 0.2;
      GNAT.Sockets.Receive_Socket (Socket => Socket,
                                   Item => Data,
                                   Last => Last);
      Buffer := To_RFLX_Bytes (Data);
      Length := RFLX.RFLX_Builtin_Types.Length (Last);
   end Receive;

   function Has_Data return Boolean is
      Request : GNAT.Sockets.Request_Type := GNAT.Sockets.Request_Type'(Name => GNAT.Sockets.N_Bytes_To_Read,
                                                                        Size => 0);
   begin
      GNAT.Sockets.Control_Socket (Socket => Socket, Request => Request);
      return Request.Size > 0;
   end Has_Data;

end Channel;
