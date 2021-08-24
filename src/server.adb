package body Server
is

   function Create_Server return Server_Type
   is
      Socket : GNAT.Sockets.Socket_Type;
   begin
      GNAT.Sockets.Create_Socket (Socket);
      return Socket;
   end Create_Server;

   procedure Listen (Socket     : in out Server_Type;
                     Port       :        GNAT.Sockets.Port_Type;
                     Connection : out    Connection_Type)
   is
      Address : GNAT.Sockets.Sock_Addr_Type;
   begin
      Address.Addr := GNAT.Sockets.Addresses (GNAT.Sockets.Get_Host_By_Name ("localhost"), 1);
      Address.Port := Port;
      GNAT.Sockets.Set_Socket_Option (Socket, GNAT.Sockets.Socket_Level, (GNAT.Sockets.Reuse_Address, True));
      GNAT.Sockets.Bind_Socket (Socket, Address);
      GNAT.Sockets.Listen_Socket (Socket);
      GNAT.Sockets.Accept_Socket (Socket, Connection, Address);
   end Listen;

end Server;
